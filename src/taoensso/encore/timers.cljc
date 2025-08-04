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
   If given `task-id`, first cancels pending task with that id."))

#?(:clj
   (deftype ^:no-doc TimerTask  [^long udt-due f]
     Comparable (compareTo [_ task] (- udt-due (.-udt-due ^TimerTask task)))
     clojure.lang.IFn (invoke [_] (f))))

#?(:clj
   (defn ^:no-doc timer-service
     "Returns a lightweight timer service inspired by http-kit's timer code.
     Uses a single task thread that'll auto start+stop as needed.
     Scheduling: O(log(num-tasks)
     Cancelling: O(1) (cancel only), or O(num-tasks) (with queue removal)."
     ([] (timer-service nil))
     ([{:keys [inactivity-timeout-msecs thread-fn]
        :or
        {inactivity-timeout-msecs 60000
         thread-fn future-call}}]

      ;; Benched ~20% faster than equivalent `java.util.concurrent.DelayQueue`,
      ;; and also supports thread auto start/stop

      (let [running? (java.util.concurrent.atomic.AtomicBoolean. false)
            by-id    (java.util.concurrent.ConcurrentHashMap.) ; {task-id ab:done?}
            pq       (java.util.PriorityQueue.)
            runner
            (fn runner []
              (loop [stop-on-empty? false]
                (if-let [^TimerTask task (locking pq (.peek pq))]
                  (let [wait (- (.-udt-due task) (System/currentTimeMillis))]
                    (if (pos? wait)
                      (locking pq
                        (try
                          (.wait pq wait)
                          (catch InterruptedException _)))
                      (do
                        (locking pq
                          (if (identical? task (.peek pq))
                            (.poll   pq)
                            (.remove pq task) ; O(n) but n usu ~1
                            ))
                        (task)))
                    (recur false))

                  (if stop-on-empty?
                    (.compareAndSet running? true false)
                    (do
                      (locking pq
                        (try
                          (.wait pq (long inactivity-timeout-msecs))
                          (catch InterruptedException _)))
                      (recur true))))))]

        (reify
          Object (toString [this] (str "encore.timer-service[" @this " " (Integer/toHexString (System/identityHashCode this)) "]"))
          clojure.lang.IDeref (deref [_] {:running? (.get running?) :queued (.size pq)})
          clojure.lang.IFn
          (invoke [_] (.compareAndSet running? true false)) ; Stop now, undocumented
          (invoke [self    msecs f] (timer-call-after self nil msecs f))
          (invoke [self id msecs f] (timer-call-after self id  msecs f))

          ITimers
          (timer-pending?   [_ id] (if-let   [^java.util.concurrent.atomic.AtomicBoolean ab (.get    by-id id)] (false?   (.get ab)) false))
          (timer-cancel     [_ id] (when-let [^java.util.concurrent.atomic.AtomicBoolean ab (.remove by-id id)] (.compareAndSet ab false true)))
          (timer-call-after [_ id msecs f]
            (let [ab:done? (java.util.concurrent.atomic.AtomicBoolean. false)
                  task
                  (TimerTask. (+ (System/currentTimeMillis) (long msecs))
                    (fn run-task []
                      (when (.compareAndSet ab:done? false true)
                        (when id (.remove by-id id ab:done?))
                        (truss/catching (f)))))]

              (when id
                (when-let [^java.util.concurrent.atomic.AtomicBoolean ab:old-done?
                           (loop []
                             (if-let [old  (.get by-id id)]
                               (if (.replace     by-id id old ab:done?) old (recur))
                               (if (.putIfAbsent by-id id     ab:done?) (recur) nil)))]
                  (.set ab:old-done? true)))

              (when-not         (.get running?)
                (when (.compareAndSet running? false true)
                  (thread-fn runner)))

              (locking   pq
                (.offer  pq task)
                (.notify pq))

              (fn cancel-task
                ([       ] (cancel-task false))
                ([remove?]
                 (when (.compareAndSet ab:done? false true)
                   (when id      (.remove by-id id ab:done?))
                   (when remove? (locking pq (.remove pq task))) ; O(n)
                   true))))))))))

#?(:cljs
   (defn ^:no-doc timer-service
     "Returns a simple timer service based on `js/setTimeout`."
     ([     ] (timer-service nil))
     ([_opts]
      (let [by-id_ (volatile! {})] ; {task-id done?_}
        (reify
          IFn
          (-invoke [self    msecs f] (timer-call-after self nil msecs f))
          (-invoke [self id msecs f] (timer-call-after self id  msecs f))

          ITimers
          (timer-pending?   [_ id] (if-let   [done?_ (get @by-id_ id)] (not      @done?_) false))
          (timer-cancel     [_ id] (when-let [done?_ (get @by-id_ id)] (when-not @done?_ (vreset! done?_ true))))
          (timer-call-after [_ id msecs f]
            (let [done?_ (volatile! false)
                  _
                  (when id
                    (when-let [old-done?_ (get @by-id_ id)]
                      (vreset! old-done?_ true))

                    (vswap! by-id_ assoc id done?_))

                  timeout-id
                  (js/setTimeout
                    (fn run-task []
                      (when-not @done?_
                        (vreset! done?_ true)
                        (when id (vswap! by-id_ dissoc id))
                        (f)))
                    msecs)]

              (fn cancel-task
                ([       ] (cancel-task false))
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
