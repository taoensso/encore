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
   (deftype ^:no-doc TimerTask [^long udt-due task-id f ^java.util.concurrent.atomic.AtomicBoolean done?]
     Comparable       (compareTo [_ task] (Long/compare udt-due (.-udt-due ^TimerTask task)))
     clojure.lang.IFn (invoke    [_] (f))))

#?(:clj
   (defn ^:no-doc timer-service
     "Returns a lightweight timer service inspired by http-kit's timer code.
     Uses a single task thread that'll auto start+stop as needed.
     Workers keep the JVM alive by default, including while idle.
     `:daemon? true` allows JVM exit without waiting for pending callbacks.
     A custom `:thread-fn` must start the supplied fn asynchronously,
     return promptly, and throw only if the worker was not started.
     It controls its own daemon status, overriding `:daemon?`.

     Scheduling: O(log(num-tasks)
     Cancelling: O(1)         with `:lazy`  cancel mode (default), or
                 O(num-tasks) with `:eager` cancel mode."

     ([] (timer-service nil))
     ([{:keys [cancel-mode inactivity-timeout-msecs daemon? thread-fn]
        :or
        {cancel-mode              :lazy
         inactivity-timeout-msecs 60000
         daemon?                  false}}]

      ;; Benched ~20% faster than equivalent `java.util.concurrent.DelayQueue`,
      ;; and also supports thread auto start/stop

      (let [thread-fn
            (or thread-fn
              (fn [f]
                (doto (Thread. ^Runnable f "encore/timers")
                  (.setDaemon (boolean daemon?))
                  (.start))))
            eager-cancel? (= :eager (truss/have #{:lazy :eager} cancel-mode))
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
                            (let [now (System/currentTimeMillis)
                                  due (.-udt-due task)
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
                                  (.wait pq (long inactivity-timeout-msecs))
                                  (catch InterruptedException _))
                                [:wait-empty nil]))))]

                    (case action
                      :call
                      (do
                        (let [^TimerTask task task
                              ^java.util.concurrent.atomic.AtomicBoolean done? (.-done? task)]
                          (when (.compareAndSet done? false true)
                            (when-let [id (.-task-id task)]
                              (.remove by-id id task))
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
          clojure.lang.IDeref (deref [_] {:running? (.get running?) :queued (.size pq)})
          clojure.lang.IFn
          (invoke [self    msecs f] (timer-call-after self nil msecs f))
          (invoke [self id msecs f] (timer-call-after self id  msecs f))

          ITimers
          (timer-cancel   [_ id] (when-let [^TimerTask task (.remove by-id id)] (cancel-task! task eager-cancel?)))
          (timer-pending? [_ id]
            (if-let [^TimerTask task (.get by-id id)]
              (let [^java.util.concurrent.atomic.AtomicBoolean done? (.-done? task)]
                (false? (.get done?)))
              false))

          (timer-call-after [self msecs f] (timer-call-after self nil msecs f))
          (timer-call-after [_ id msecs f]
            (let [ab:done? (java.util.concurrent.atomic.AtomicBoolean. false)
                  task (TimerTask. (+ (System/currentTimeMillis) (long msecs)) id f ab:done?)]

              (when id
                (when-let [^TimerTask old-task
                           (loop []
                             (if-let [old  (.get by-id id)]
                               (if (.replace     by-id id old task) old (recur))
                               (if (.putIfAbsent by-id id     task) (recur) nil)))]
                  (cancel-task! old-task eager-cancel?)))

              (locking pq
                (.offer pq task)
                (when (and eager-cancel? (.get ab:done?)) (.remove pq task))
                (.notify pq)
                (try
                  (start-runner! runner)
                  (catch Throwable t
                    (cancel-task! task false)
                    (.remove pq task)
                    (when id (.remove by-id id task))
                    (throw t))))

              (fn cancel-task
                ([       ] (cancel-task eager-cancel?))
                ([remove?]
                 (when (cancel-task! task remove?)
                   (when id      (.remove by-id id task))
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
                        (when id (vswap! by-id_ dissoc id))
                        (f)))
                    msecs)

                  entry [done?_ timeout-id]
                  _
                  (when id
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
                   (when id      (vswap! by-id_ dissoc id))
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

  Returns (fn cancel []) which can be used to cancel call.

  If given `task-id`, first auto cancels pending task with that id
  (handy for debouncing, time extension, etc.)."

  ([        msecs task-fn] (default-timer-service nil     msecs task-fn))
  ([task-id msecs task-fn] (default-timer-service task-id msecs task-fn)))

(comment ((call-after 500 (bound-fn [] (println "foo")))))

;;;; Benching

(comment
  (let [ts (timer-service)
        n0 (System/nanoTime)
        c  (java.util.concurrent.atomic.AtomicLong. 0)
        r  (java.util.Random.)
        n  1e6]

    (dotimes [_ n]
      (ts (.nextInt (java.util.Random.) 1000)
        (fn [] (.incrementAndGet c))))

    (while (< (.get c) n))
    (- (System/nanoTime) n0)))
