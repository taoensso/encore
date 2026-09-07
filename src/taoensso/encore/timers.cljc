(ns ^:no-doc taoensso.encore.timers
  "Experimental, subject to change without notice!"
  (:require
   [taoensso.truss :as truss]))

;;;; Timer service

(defprotocol ^:no-doc ITimers
  (^:no-doc timer-pending? [_ task-id] "Returns true iff there's a task with given id awaiting call")
  (^:no-doc timer-cancel   [_ task-id] "Returns true iff pending task with given id was cancelled")
  (^:no-doc timer-call-after
   [_ task-id msecs task-fn]
   [_         msecs task-fn]
   "Calls (task-fn) after given msecs, no auto binding conveyance!
   Returns (fn cancel []) which can be used to cancel call.
   If given `task-id`, first cancels pending task with that id.")

  #?(:clj
     (^:no-doc timers-gc [_]
      "Purges all lazily cancelled timer tasks and returns the number purged.
      O(num-tasks) on Java 11+")))

#?(:clj
   (deftype ^:no-doc TimerTask [^long msecs-due task-id f ^java.util.concurrent.atomic.AtomicBoolean done?]
     Comparable       (compareTo [_ task] (Long/compare msecs-due (.-msecs-due ^TimerTask task)))
     clojure.lang.IFn (invoke    [_] (f))))

#?(:clj
   (defn ^:no-doc timer-service
     "Returns a lightweight timer service inspired by http-kit's timer code.
     Uses a single task thread that'll auto start+stop as needed.
     Callbacks execute serially; keep them short to avoid delaying others.
     Workers keep the JVM alive by default, including while idle.
     `:daemon? true` allows JVM exit without waiting for pending callbacks.
     `:inactivity-timeout-msecs` must be >= 1 ms (fractions are truncated).
     A custom `:thread-fn` must start the supplied fn asynchronously,
     return promptly, and throw only if the worker was not started.
     It controls its own daemon status, overriding `:daemon?`.

     Scheduling: O(log(num-tasks))
     Cancelling: O(1)         with `:lazy`  cancel mode (default), or
                 O(num-tasks) with `:eager` cancel mode."

     ([] (timer-service nil))
     ([{:keys [cancel-mode inactivity-timeout-msecs daemon? thread-fn]
        :or
        {cancel-mode              :lazy
         inactivity-timeout-msecs 60000
         daemon?                  false}}]

      (let [inactivity-timeout-msecs (long (truss/have pos? (long inactivity-timeout-msecs)))
            thread-fn
            (or thread-fn
              (fn [f]
                (doto (Thread. ^Runnable f "encore/timers")
                  (.setDaemon (boolean daemon?))
                  (.start))))
            eager-cancel? (= :eager (truss/have #{:lazy :eager} cancel-mode))
            nano-origin (System/nanoTime)
            now-msecs (fn ^long [] (quot (unchecked-subtract (System/nanoTime) nano-origin) 1000000))
            running? (java.util.concurrent.atomic.AtomicBoolean. false)
            by-id    (java.util.concurrent.ConcurrentHashMap.) ; {task-id TimerTask}
            pq       (java.util.PriorityQueue.)

            cancel-task!
            (fn [^TimerTask task remove?]
              (let [^java.util.concurrent.atomic.AtomicBoolean done? (.-done? task)]
                (when (.compareAndSet done? false true)
                  (when remove?
                    (locking pq
                      (.remove pq task) ; O(n)
                      (.notify pq)))
                  true)))

            start-runner!
            (fn [runner] ; Called with pq locked
              (when (and (not (.isEmpty pq)) (.compareAndSet running? false true))
                (try
                  (thread-fn runner)
                  (catch Throwable t
                    (.set running? false)
                    (throw t)))))

            runner
            (fn runner []
              (try
                (loop [stop-on-empty? false]
                  (let [[action task]
                        (locking pq
                          (if-let [^TimerTask task (.peek pq)]
                            (let [now (now-msecs)
                                  due (.-msecs-due task)
                                  wait (if (> due now) (- due now) 0)]
                              (if (pos? wait)
                                (do
                                  (try
                                    (.wait pq wait)
                                    (catch InterruptedException _))
                                  [:wait nil])
                                (do (.poll pq) [:call task])))

                            (if stop-on-empty?
                              (do
                                (.compareAndSet running? true false)
                                [:stop nil])
                              (do
                                (try
                                  (.wait pq inactivity-timeout-msecs)
                                  (catch InterruptedException _))
                                [:wait-empty nil]))))]

                    (case action
                      :call
                      (do
                        (let [^TimerTask task task
                              ^java.util.concurrent.atomic.AtomicBoolean done? (.-done? task)]
                          (when (.compareAndSet done? false true)
                            (let [id (.-task-id task)]
                              (when (some? id) (.remove by-id id task)))
                            (truss/catching (task))))
                        (recur false))

                      :wait (recur false)
                      :wait-empty (recur true)
                      :stop nil)))
                (catch Throwable t
                  (locking pq
                    (.set running? false)
                    (try
                      (start-runner! runner)
                      (catch Throwable launch-error
                        (when-not (identical? t launch-error)
                          (.addSuppressed t launch-error)))))
                  (throw t))))]

        (reify
          Object (toString [this] (str "encore.timer-service[" @this " " (Integer/toHexString (System/identityHashCode this)) "]"))
          clojure.lang.IDeref (deref [_] (locking pq {:running? (.get running?) :queued (.size pq)}))
          clojure.lang.IFn
          (invoke [self    msecs f] (timer-call-after self nil msecs f))
          (invoke [self id msecs f] (timer-call-after self id  msecs f))

          ITimers
          (timer-cancel   [_ id]
            (when (some? id)
              (when-let [^TimerTask task (.remove by-id id)]
                (cancel-task! task eager-cancel?))))
          (timer-pending? [_ id]
            (if-let [^TimerTask task (when (some? id) (.get by-id id))]
              (let [^java.util.concurrent.atomic.AtomicBoolean done? (.-done? task)]
                (false? (.get done?)))
              false))

          (timer-call-after [self msecs f] (timer-call-after self nil msecs f))
          (timer-call-after [_ id msecs f]
            (let [ab:done? (java.util.concurrent.atomic.AtomicBoolean. false)
                  task (TimerTask. (+ (now-msecs) (max 0 (long msecs))) id f ab:done?)]

              (when (some? id)
                (when-let [^TimerTask old-task (.put by-id id task)]
                  (cancel-task! old-task eager-cancel?)))

              (locking pq
                (.offer pq task)
                (when (and eager-cancel? (.get ab:done?)) (.remove pq task))
                (when (identical? task (.peek pq)) (.notify pq))
                (try
                  (start-runner! runner)
                  (catch Throwable t
                    (cancel-task! task false)
                    (.remove pq task)
                    (when (some? id) (.remove by-id id task))
                    (throw t))))

              (fn cancel-task
                ([       ] (cancel-task eager-cancel?))
                ([remove?]
                 (when (cancel-task! task remove?)
                   (when (some? id) (.remove by-id id task))
                   true)))))

          (timers-gc [_]
            (locking pq
              (let [size-before (.size pq)
                    _
                    (.removeIf
                      pq
                      (reify java.util.function.Predicate
                        (test [_ task]
                          (let [^TimerTask task task
                                ^java.util.concurrent.atomic.AtomicBoolean done? (.-done? task)]
                            (.get done?)))))
                    removed (- size-before (.size pq))]
                (when (pos? removed) (.notify pq))
                (do         removed)))))))))

#?(:cljs
   (defn ^:no-doc timer-service
     "Returns a simple timer service based on `js/setTimeout`.
     Callbacks execute serially; keep them short to avoid delaying others.
     Use `:eager` cancel mode to clear cancelled timeouts immediately."
     ([     ] (timer-service nil))
     ([{:keys [cancel-mode] :or {cancel-mode :lazy}}]
      (let [eager-cancel? (= :eager (truss/have #{:lazy :eager} cancel-mode))
            by-id_ (volatile! {})] ; {task-id [done?_ timeout-id]}
        (reify
          IFn
          (-invoke [self    msecs f] (timer-call-after self nil msecs f))
          (-invoke [self id msecs f] (timer-call-after self id  msecs f))

          ITimers
          (timer-pending? [_ id] (if-let [[done?_] (get @by-id_ id)] (not @done?_) false))
          (timer-cancel   [_ id]
            (when-let [[done?_ timeout-id] (get @by-id_ id)]
              (when-not @done?_
                (vreset! done?_ true)
                (vswap! by-id_ dissoc id)
                (when eager-cancel? (js/clearTimeout timeout-id))
                true)))

          (timer-call-after [self msecs f] (timer-call-after self nil msecs f))
          (timer-call-after [_ id msecs f]
            (let [done?_ (volatile! false)
                  timeout-id
                  (js/setTimeout
                    (fn run-task []
                      (when-not @done?_
                        (vreset! done?_ true)
                        (when (some? id) (vswap! by-id_ dissoc id))
                        (f)))
                    msecs)

                  entry [done?_ timeout-id]
                  _
                  (when (some? id)
                    (when-let [[old-done?_ old-timeout-id] (get @by-id_ id)]
                      (when-not @old-done?_
                        (vreset! old-done?_ true)
                        (when eager-cancel? (js/clearTimeout old-timeout-id))))

                    (vswap! by-id_ assoc id entry))]

              (fn cancel-task
                ([       ] (cancel-task eager-cancel?))
                ([remove?]
                 (when-not @done?_
                   (vreset! done?_ true)
                   (when (some? id) (vswap! by-id_ dissoc id))
                   (when remove? (js/clearTimeout timeout-id))
                   true))))))))))

(comment
  (def ts (timer-service))
  (do
    (println "---")
    (println @ts)
    (ts :foo 2000 (bound-fn [] (println "2s")))
    (ts      3000 (bound-fn [] (println "3s")))
    (ts      6000 (bound-fn [] (println "6s")))))

(def ^:no-doc default-timer-service (timer-service))

(defn call-after
  "Calls (task-fn) after given number of msecs.
  No auto binding conveyance, use `bound-fn` to keep dynamic bindings.
  Callbacks execute serially; keep them short to avoid delaying others.

  Returns (fn cancel []) which can be used to cancel call.

  If given `task-id`, first auto cancels pending task with that id
  (handy for debouncing, time extension, etc.)."

  ([        msecs task-fn] (default-timer-service nil     msecs task-fn))
  ([task-id msecs task-fn] (default-timer-service task-id msecs task-fn)))

(comment ((call-after 500 (bound-fn [] (println "foo")))))

;;;; Benching

(comment
  (let [ts (timer-service)
        n  1000000
        done (java.util.concurrent.CountDownLatch. n)
        f  (fn [] (.countDown done))
        r  (java.util.Random. 42)
        n0 (System/nanoTime)
        _  (dotimes [_ n] (ts (.nextInt r 1000) f))
        schedule-nsecs (- (System/nanoTime) n0)]

    (when-not (.await done 60 java.util.concurrent.TimeUnit/SECONDS)
      (throw (ex-info "Timer benchmark timed out" {:remaining (.getCount done)})))
    {:schedule-msecs (/ schedule-nsecs 1e6)
     :total-msecs    (/ (- (System/nanoTime) n0) 1e6)}))
