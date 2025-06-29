(ns ^:no-doc taoensso.encore.timers
  "Experimental, subject to change without notice!"
  (:require
   [taoensso.truss :as truss]))

;;;; Timer service

#?(:clj
   (deftype ^:no-doc TimerTask [^long udt-due ^java.util.concurrent.atomic.AtomicBoolean done? f]
     clojure.lang.IFn
     (invoke [_]
       (when (.compareAndSet done? false true)
         (truss/catching (f)) true))

     Comparable
     (compareTo [_this other-task]
       (- udt-due (.-udt-due ^TimerTask other-task)))))

#?(:clj
   (defn ^:no-doc timer-service
     "Returns a lightweight timer service based on http-kit's timer code.
     Uses a single thread that'll auto start+stop as needed.

     Invoke service with [msecs f] to call task (f) after given msecs
     and return a (fn cancel [remove?]) to cancel task.

     Scheduling: O(log(num-tasks)
     Cancelling:     O(num-tasks)"

     ;; Benched ~20% faster than equivalent `java.util.concurrent.DelayQueue`,
     ;; and also supports thread auto start/stop

     ([] (timer-service nil))
     ([{:keys [inactivity-timeout-msecs thread-fn]
        :or
        {inactivity-timeout-msecs 60000
         thread-fn future-call}}]

      (let [running? (java.util.concurrent.atomic.AtomicBoolean. false)
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
          (invoke [_ msecs f]
            (let [done? (java.util.concurrent.atomic.AtomicBoolean. false)
                  task
                  (TimerTask.
                    (+ (System/currentTimeMillis) (long msecs))
                    done? f)]

              (when-not         (.get running?)
                (when (.compareAndSet running? false true)
                  (thread-fn runner)))

              (locking   pq
                (.offer  pq task)
                (.notify pq))

              (fn cancel
                ([       ] (cancel true))
                ([remove?]
                 (when (.compareAndSet done? false true)
                   (when remove? (locking pq (.remove pq task))) ; O(n)
                   true))))))))))

(comment
  (def ts (timer-service))
  (do
    (println "---")
    (println @ts)
    (ts 5000 (bound-fn [] (println "5s")))
    (ts 3000 (bound-fn [] (println "3s")))
    (ts 6000 (bound-fn [] (println "6s")))))

#?(:clj (def ^:no-doc default-timer-service (timer-service)))

(defn call-after
  "Calls (f) after given msecs.
  Returns (fn cancel []) which can be used to cancel calling.
  No automatic binding conveyance."
  [msecs f]
  #?(:clj (default-timer-service msecs f))
  #?(:cljs
     (let [done? (volatile! false)
           timeout-id
           (js/setTimeout
             (fn [] (when-not @done? (vreset! done? true) (f) true))
             msecs)]

       (fn cancel
         ([       ] (cancel true))
         ([remove?]
          (when-not @done?
            (vreset! done? true)
            (when remove? (js/clearTimeout timeout-id))
            true))))))

(comment ((call-after 500 (bound-fn [] (println "foo")))))

;; Benching

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
