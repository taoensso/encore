(ns ^:no-doc taoensso.encore.runner
  "Alpha, subject to change without notice!
  Runner util for easily configurable a/sync fn execution.

  Compared to agents:
    - Runners have (configurable) back-pressure mechanism.
    - Runners may be non-linear when n-threads > 1.
    - Runners take nullary fns rather than unary fns of state.
    - Runners have no validators or watches.
    - Runners auto shutdown their threads on JVM shutdown."

  {:added "v3.67.0 (2023-09-08)"}
  (:require
   [taoensso.encore :as enc :refer [have have?]]))

(comment
  (remove-ns 'taoensso.encore.runner)
  (:api (enc/interns-overview)))

#?(:clj
   (defn runner
     "Returns a new stateful (fn runner ([]) ([f])) such that:
      (runner f) => Runner should execute given nullary fn according to runner opts.
                    Returns:
                      nil   if runner has stopped accepting new execution requests.
                      true  if fn was accepted for execution *without* back pressure.
                      false if runner's back-pressure mechanism was engaged.

      (runner)   => Runner should stop accepting new execution requests.
                    Returns true iff runner's status changed with this call.

    Runners are a little like simplified agents with an explicit and configurable
    back-pressure mechanism.

    Options include:
      `mode`        - Mode of operation, ∈ #{:sync :blocking :dropping :sliding}.
      `buffer-size` - Size of buffer before back-pressure mechanism is engaged.
      `n-threads`   - Number of threads for asynchronously executing fns.
                      NB execution order may be non-sequential when n > 1."

     [{:keys [mode buffer-size n-threads thread-name daemon-threads?] :as opts
       :or
       {mode :sliding
        buffer-size 1024
        n-threads   1}}]

     (case mode
       :sync
       (let [stopped?_ (volatile! false)]
         (fn sync-runner
           ([ ] (when-not @stopped?_ (vreset! stopped?_ true)))
           ([f] (if       @stopped?_ nil (do (enc/catching (f)) true)))))

       (:blocking :dropping :sliding)
       (let [stopped?_ (volatile! false)
             abq (java.util.concurrent.ArrayBlockingQueue.
                   (enc/as-pos-int buffer-size) false)
             init_
             (delay
               (when-not daemon-threads?
                 (.addShutdownHook (Runtime/getRuntime)
                   (Thread. (fn stop-runner [] (vreset! stopped?_ true)))))

               (dotimes [n (enc/as-pos-int n-threads)]
                 (let [wfn
                       (fn worker-fn []
                         (loop []
                           (if-let [f (.poll abq 2000 java.util.concurrent.TimeUnit/MILLISECONDS)]
                             ;; Recur unconditionally to drain abq even when stopped
                             (do (enc/catching (f)) (recur))
                             (when-not @stopped?_   (recur)))))

                       thread-name (enc/str-join-once "-" [(or thread-name `runner) "loop" (inc n) "of" n-threads])
                       thread (Thread. wfn thread-name)]

                   (when daemon-threads?
                     (.setDaemon thread true))

                   ;; (println "Starting thread:" thread-name)
                   (.start thread))))

             run-fn
             (case mode
               :blocking (fn [f] (or (.offer abq f) (.put abq f) false))
               :dropping (fn [f] (or (.offer abq f)              false))
               :sliding
               (fn [f]
                 (or
                   (.offer abq f) ; Common case
                   (loop []
                     (.poll abq) ; Drop
                     (if (.offer abq f)
                       false ; Indicate that drop/s occurred
                       (recur))))))]

         (if-let [msecs (get opts :debug/init-after)]
           (do
             (future (Thread/sleep (int msecs)) @init_)
             (fn async-runner
               ([ ] (when-not @stopped?_ (vreset! stopped?_ true)))
               ([f] (if       @stopped?_ nil (run-fn f)))))

           (fn async-runner
             ([ ] (when-not @stopped?_ (vreset! stopped?_ true)))
             ([f] (if       @stopped?_ nil (do @init_ (run-fn f)))))))

       (enc/unexpected-arg! mode
         {:context  `runner
          :expected #{:sync :blocking :dropping :sliding}}))))

(comment
  (let [r1 (runner {:mode :sync})
        r2 (runner {:mode :blocking})]
    (enc/qb 1e6 ; [40.68 158.46]
      (r1 (fn []))
      (r2 (fn [])))))
