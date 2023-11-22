(ns ^:no-doc taoensso.encore.signals.api
  "Experimental, subject to change without notice!!
  Private utils to bootstrap a local copy of the signals API.
  Separate from `encore.signals` to avoid cyclic dependencies
  (`signals.api` needs `telemere.impl`, which needs `signals`)."
  {:added "Encore v3.68.0 (2023-09-25)"}
  (:require
   [clojure.string  :as str]
   [taoensso.encore :as enc :refer [have have?]]
   [taoensso.encore.signals :as sigs]))

(enc/require-telemere-if-present) ; Requires `telemere.impl`

(comment
  (remove-ns 'taoensso.encore.signals.api)
  (:api (enc/interns-overview)))

#?(:clj
   (defn- api-docstring [column purpose doc-template]
     (let [doc (apply format doc-template (repeat 10 purpose))
           [l1 & lines] (str/split-lines doc)
           left-trim (- (long column) 2)]
       (reduce
         (fn [acc in] (str acc "\n" (enc/get-substr-by-idx in left-trim)))
         l1 lines))))

(comment
  (enc/def* foo
    {:doc
     (api-docstring 7 nil
       "Line 1
       Line 2")}
    nil))

#?(:clj
   (defmacro def-filter-api
     "Defines signal filter API vars in current ns (`with-ns-filter`,
     `set-ns-filter!`, etc.)."
     [sf-arity *sig-filter*
      {:keys [purpose]
       :or   {purpose "signal"}}]

     ;; `purpose` e/o #{"signal" "profiling" "logging"}

     (let [sf-arity (int (or sf-arity -1))
           _
           (when-not (contains? #{1 2 3 4} sf-arity)
             (sigs/unexpected-sf-artity! sf-arity `def-filter-api))

           level-aliases `(enc/defalias sigs/level-aliases)

           filtering-help
           `(def ~'filtering-help
              ~(api-docstring 17 purpose
                 "Your filter config determines which %s calls will be enabled.

                 Filtering can occur at compile-time (=> elision), or runtime.
                 Both compile-time and runtime config can be specified via:

                   1. System values (JVM properties, environment variables, or
                      classpath resources). See library docs for details.

                   2. The filter API consting of the following:
                     `set-ns-filter!`,     `with-ns-filter`      - For filtering by namespace
                     `set-minimum-level!`, `with-minimum-level!` - For filtering by %s level
                     `set-id-filter!`,     `with-id-filter`      - For filtering by %s id   (when relevant)
                     `set-kind-filter!`,   `with-kind-filter`    - For filtering by %s kind (when relevant)

                     See the relevant docstrings for details.

                 Additional filtering can also be applied at the handler level,
                 see `add-handler!` docstring for details.

                 If anything is unclear, please ping me (@ptaoussanis) so that I can
                 improve these docs!")
              "See docstring")

           ns-filter
           `(do
              (defn ~'set-ns-filter!
                ~(api-docstring 19 purpose
                   "Sets %s namespace filter based on given `ns-filter` spec.

                   `ns-filter` may be:
                     - A regex pattern of namespace/s to allow.
                     - A str/kw/sym, in which \"*\"s act as wildcards.
                     - A vector or set of regex patterns or strs/kws/syms.
                     - {:allow <spec> :deny <spec>} with specs as above.")
                ~'[ns-filter]
                (enc/force-ref
                  (enc/update-var-root! ~*sig-filter*
                    (fn [old#] (sigs/update-sig-filter old# {:ns-filter ~'ns-filter})))))

              (defmacro ~'with-ns-filter
                ~(api-docstring 19 purpose
                   "Executes form with given %s namespace filter in effect.
                   See `set-ns-filter!` for details.")
                ~'[ns-filter form]
                `(binding [~'~*sig-filter* (sigs/update-sig-filter ~'~*sig-filter* {:ns-filter ~~'ns-filter})]
                   ~~'form)))

           kind-filter
           `(do
              (defn ~'set-kind-filter!
                ~(api-docstring 19 purpose
                   "Sets %s kind filter based on given `kind-filter` spec.

                   `kind-filter` may be:
                     - A regex pattern of kind/s to allow.
                     - A str/kw/sym, in which \"*\"s act as wildcards.
                     - A vector or set of regex patterns or strs/kws/syms.
                     - {:allow <spec> :deny <spec>} with specs as above.")
                ~'[kind-filter]
                (enc/force-ref
                  (enc/update-var-root! ~*sig-filter*
                    (fn [old#] (sigs/update-sig-filter old# {:kind-filter ~'kind-filter})))))

              (defmacro ~'with-kind-filter
                ~(api-docstring 19 purpose
                   "Executes form with given %s kind filter in effect.
                   See `set-kind-filter!` for details.")
                ~'[kind-filter form]
                `(binding [~'~*sig-filter* (sigs/update-sig-filter ~'~*sig-filter* {:kind-filter ~~'kind-filter})]
                   ~~'form)))

           id-filter
           `(do
              (defn ~'set-id-filter!
                ~(api-docstring 19 purpose
                   "Sets %s id filter based on given `id-filter` spec.

                   `id-filter` may be:
                     - A regex pattern of id/s to allow.
                     - A str/kw/sym, in which \"*\"s act as wildcards.
                     - A vector or set of regex patterns or strs/kws/syms.
                     - {:allow <spec> :deny <spec>} with specs as above.")
                ~'[id-filter]
                (enc/force-ref
                  (enc/update-var-root! ~*sig-filter*
                    (fn [old#] (sigs/update-sig-filter old# {:id-filter ~'id-filter})))))

              (defmacro ~'with-id-filter
                ~(api-docstring 19 purpose
                   "Executes form with given %s id filter in effect.
                   See `set-id-filter!` for details.")
                ~'[id-filter form]
                `(binding [~'~*sig-filter* (sigs/update-sig-filter ~'~*sig-filter* {:id-filter ~~'id-filter})]
                   ~~'form)))

           min-level
           (case sf-arity
             (2 3)
             `(do
                (defn ~'set-min-level!
                  ~(api-docstring 21 purpose
                     "Sets minimum %s level based on given `min-level` spec.

                     `min-level` may be:
                       - An integer.
                       - A level keyword (see `level-aliases` var for details).

                     If `ns-filter` is provided, then the given minimum level
                     will apply only for namespaces that match `ns-filter`.
                     See `set-ns-filter!` for details.")
                  (~'[          min-level] (~'set-min-level! nil ~'min-level))
                  (~'[ns-filter min-level]
                   (enc/force-ref
                     (enc/update-var-root! ~*sig-filter*
                       (fn [old-sf#]
                         (sigs/update-sig-filter old-sf#
                           {:min-level-fn
                            (fn [old-ml#]
                              (sigs/update-min-level old-ml# nil ~'ns-filter ~'min-level))}))))))

                (defmacro ~'with-min-level
                  ~(api-docstring 21 purpose
                     "Executes form with given minimum %s level in effect.
                     See `set-min-level!` for details.")
                  (~'[          min-level form] (list '~'with-min-level nil ~'min-level ~'form))
                  (~'[ns-filter min-level form]
                   `(binding [~'~*sig-filter*
                              (sigs/update-sig-filter ~'~*sig-filter*
                                {:min-level-fn
                                 (fn [~'old-ml#]
                                   (sigs/update-min-level ~'old-ml# nil ~~'ns-filter ~~'min-level))})]
                      ~~'form))))

             (4)
             `(do
                (defn ~'set-min-level!
                  ~(api-docstring 21 purpose
                     "Sets minimum %s level based on given `min-level` spec.

                      `min-level` may be:
                        - An integer.
                        - A level keyword (see `level-aliases` var for details).

                      If `ns-filter` is provided, then the given minimum level
                      will apply only for namespaces that match `ns-filter`.
                      See `set-ns-filter!` for details.

                      If non-nil `kind` is provided, then the given minimum level
                      will apply only for that %s kind.")
                  (~'[kind           min-level] (~'set-min-level! ~'kind nil ~'min-level))
                  (~'[kind ns-filter min-level]
                   (enc/force-ref
                     (enc/update-var-root! ~*sig-filter*
                       (fn [old-sf#]
                         (sigs/update-sig-filter old-sf#
                           {:min-level-fn
                            (fn [old-ml#]
                              (sigs/update-min-level old-ml# ~'kind ~'ns-filter ~'min-level))}))))))

                (defmacro ~'with-min-level
                  ~(api-docstring 21 purpose
                     "Executes form with given minimum %s level in effect.
                     See `set-min-level!` for details.")
                  (~'[kind           min-level form] (list '~'with-min-level ~'kind nil ~'min-level ~'form))
                  (~'[kind ns-filter min-level form]
                   `(binding [~'~*sig-filter*
                              (sigs/update-sig-filter ~'~*sig-filter*
                                {:min-level-fn
                                 (fn [~'old-ml#]
                                   (sigs/update-min-level ~'old-ml# ~~'kind ~~'ns-filter ~~'min-level))})]
                      ~~'form)))))]

       (case sf-arity
         2 `(do ~ns-filter                         ~min-level ~level-aliases ~filtering-help)
         3 `(do ~ns-filter              ~id-filter ~min-level ~level-aliases ~filtering-help)
         4 `(do ~ns-filter ~kind-filter ~id-filter ~min-level ~level-aliases ~filtering-help)))))

(comment
  (def ^:dynamic                  *sig-filter* nil)
  (macroexpand '(def-filter-api 3 *sig-filter* {}))
  (do           (def-filter-api 3 *sig-filter* {})))

;;;; Signal handling

(defn ^:no-doc -get-handlers [handlers] (when handlers (enc/map-vals meta handlers)))

(def ^:private default-dispatch-opts
  #?(:clj  {:async {:mode :dropping, :buffer-size 4096, :n-threads 1, :daemon-threads? false}}
     :cljs {}))

(defn ^:no-doc get-middleware-fn
  "Takes ?[<unary-fn> ... <unary-fn>] and returns nil, or a single unary fn
  that is the left->right composition of the others and that short-circuits if
  any returns nil."
  {:added "Encore vX.Y.Z (YYYY-MM-DD)"}
  [middleware]
  (enc/cond
    (empty?   middleware)    nil
    (= (count middleware) 1) (first middleware)
    :else
    (fn multi-middleware-fn [in]
      (reduce (fn [in mf] (or (mf in) (reduced nil)))
        in middleware))))

(comment
  ((get-middleware-fn [inc inc inc str]) 1)
  ((get-middleware-fn [inc (fn [_] nil) (fn [_] (throw (Exception. "Foo")))]) 1))

(defn wrap-handler
  "Wraps given handler-fn to add common handler-level functionality."
  [handler-id handler-fn
   {:as dispatch-opts
    :keys
    [#?(:clj async) sample rate-limit filter-fn middleware,
     ns-filter kind-filter id-filter min-level,
     rl-error rl-backup error-fn backp-fn]}]

  (let [sample-rate  (when sample (enc/as-pnum! sample))
        rl-handler   (when-let [spec rate-limit] (enc/rate-limiter {} spec))
        sig-filter*  (sigs/sig-filter ns-filter kind-filter id-filter min-level)
        stopped?_    (enc/latom false)

        rl-error (get dispatch-opts :rl-error  (enc/rate-limiter {} [[1 (enc/ms :mins 1)]]))
        rl-backp (get dispatch-opts :rl-backup (enc/rate-limiter {} [[1 (enc/ms :mins 1)]]))
        error-fn (get dispatch-opts :error-fn  ::default)
        backp-fn (get dispatch-opts :packp-fn  ::default)

        middleware-fn (get-middleware-fn middleware) ; (fn [signal-value]) => transformed signal-value
        wrapped-handler-fn
        (fn wrapped-handler-fn
          ([] ; Shutdown
           (when (enc/-cas!? stopped?_ false true)
             (enc/catching (handler-fn)) ; Notify handler-fn to shutdown
             true))

          ([signal]
           (when-not (stopped?_)
             (enc/try*
               (let [allow?
                     (and
                       (if sample-rate (< (Math/random) ^double sample-rate)   true)
                       (if sig-filter* (sigs/allow-signal? signal sig-filter*) true)
                       (if filter-fn   (filter-fn          signal)             true)
                       (if rl-handler  (if (rl-handler nil) false true)        true) ; Nb last (increments count)
                       )]

                 (when allow?
                   (when-let [sig-val (sigs/signal-value signal)]
                     (if middleware-fn
                       (when-let [sig-val (middleware-fn sig-val)] (handler-fn sig-val))
                       (do                                         (handler-fn sig-val)))))

                 nil)

               (catch :any t
                 (when error-fn
                   (when-not (and rl-error (rl-error handler-id))
                     (if-not (enc/identical-kw? error-fn ::default)
                       (error-fn {:handler-id handler-id, :error t})
                       (enc/signal!
                         {:level :error
                          :id    ::handler-error
                          :error t
                          :msg   "[taoensso/signals] Error executing wrapped handler fn"
                          :data
                          {:handler-id    handler-id
                           :handler-fn    handler-fn
                           :dispatch-opts dispatch-opts}}))))
                 nil)))))]

    #?(:cljs wrapped-handler-fn
       :clj
       (if-not async
         wrapped-handler-fn
         (let [runner (enc/runner (have map? async))]
           (fn wrapped-handler-fn* [signal]
             (when-let [back-pressure? (false? (runner (fn [] (wrapped-handler-fn signal))))]
               (when backp-fn
                 (when-not (and rl-backp (rl-backp handler-id))
                   (if-not (enc/identical-kw? backp-fn ::default)
                     (backp-fn {:handler-id handler-id})
                     (enc/signal!
                       {:level :warn
                        :id    ::handler-back-pressure
                        :msg   "[taoensso/signals] Back pressure on wrapped handler fn"
                        :data
                        {:handler-id    handler-id
                         :handler-fn    handler-fn
                         :dispatch-opts dispatch-opts}})))))))))))

#?(:clj
   (defmacro def-handler-api
     "Defines signal handler API vars in current ns (`add-handler!`,
     `remove-handler!`, `get-handlers`), and adds JVM hook to trigger handler
     shutdown on JVM shutdown."
     [sf-arity *sig-handlers*
      {:keys [purpose base-dispatch-opts]
       :or   {purpose "signal"}}]

     ;; `purpose` e/o #{"signal" "profiling" "logging"}

     (let [add-shutdown-hook
           (when-not (:ns &env)
             `(enc/defonce ~'_handler-shutdown-hook {:private true}
                (.addShutdownHook (Runtime/getRuntime)
                  (Thread. (fn ~'shutdown-signal-handlers []
                             (sigs/shutdown-handlers! ~*sig-handlers*))))))]

       `(do
          (def ~'handlers-help
            ~(api-docstring 15 purpose
               "The handler API consists of the following:
                 `get-handlers`    - Returns info on currently registered handlers
                 `add-handler!`    - Used to   register handlers
                 `remove-handler!` - Used to unregister handlers

               See the relevant docstrings for details.

               If anything is unclear, please ping me (@ptaoussanis) so that I can
               improve these docs!")

            "See docstring")

          (defn ~'get-handlers
            ~(api-docstring 0 purpose
               "Returns {<handler-id> <dispatch-opts>} for all registered %s handlers.")
            [] (-get-handlers ~*sig-handlers*))

          (defn ~'remove-handler!
            ~(api-docstring 15 purpose
               "Deregisters %s handler with given id, and returns {<handler-id> <disaptch-opts>}
                for all %s handlers still registered.")
            ~'[handler-id]
            (-get-handlers
              (enc/update-var-root! ~*sig-handlers*
                (fn [m#] (not-empty (dissoc m# ~'handler-id))))))

          (defn ~'add-handler!
            ~(api-docstring 15 purpose
               "Registers given %s handler and returns {<handler-id> <dispatch-opts>}
               for all handlers now registered.

               `handler-fn` should be a fn of 1-2 arities:
                 ([handler-arg]) => Handle the given argument (e.g. write to disk/db, etc.)
                 ([]) => Optional arity, called exactly once on system shutdown.
                         Provides an opportunity for handler to close/release
                         any resources that it may have opened/acquired.

               See the relevant docstring/s for `handler-arg` details.

               Handler ideas:
                 Save to a db, `tap>`, log, `put!` to an appropriate `core.async`
                 channel, filter, aggregate, use for a realtime analytics dashboard,
                 examine for outliers or unexpected data, etc.

               Dispatch options include:
                 `async` (Clj only)
                    Options for running handler asynchronously via `taoensso.encore/runner`,
                    {:keys [mode buffer-size n-threads daemon-threads? ...]}

                    Supports `:blocking`, `:dropping`, and `:sliding` back-pressure modes.
                    NB handling order may be non-sequential when `n-threads` > 1.

                 `sample`
                   Optional sample rate ∈ℝ[0,1].
                   When present, handle only this (random) proportion of args:
                     1.0 => handle every arg
                     0.0 => noop   every arg
                     0.5 => handle random 50%% of args

                 `ns-filter`   - Namespace filter as in `set-ns-filter!`
                 `kind-filter` - Kind      filter as in `set-kind-filter!` (when relevant)
                 `id-filter`   - Id        filter as in `set-id-filter!`   (when relevant)
                 `min-level`   - Minimum   level  as in `set-min-level!`

                 `filter-fn`
                   Optional (fn allow? [handler-arg]) that must return truthy
                   for `handler-fn` to be called for given `handler-arg`.

                   When present, called *after* sampling and other filters, but
                   before rate limiting.

                 `rate-limit`
                   Optional rate limit spec as provided to `taoensso.encore/rate-limiter`,
                   {<limit-id> [<n-max-calls> <msecs-window>]}.

                   Examples:
                     {\"1/sec\"  [1   1000]} => Max 1  call  per 1000 msecs
                     {\"1/sec\"  [1   1000]
                      \"10/min\" [10 60000]} => Max 1  call  per 1000 msecs,
                                              and 10 calls per 60   secs

                 `middleware`
                   Optional vector of unary middleware fns to apply (left-to-right/sequentially)
                   to handler-arg right before passing to handler-fn. If any middleware fn returns
                   nil, abort immediately without calling handler-fn.

                   Useful for transforming handler-args before handling.

                 `error-fn` - (fn [{:keys [handler-id error]}]) to call on handler error.
                 `backp-fn` - (fn [{:keys [handler-id      ]}]) to call on handler back pressure.

               Flow sequence:
                 Sample -> filters (sample -> ns -> kind -> id -> level -> filter-fn) ->
                   rate limit -> middleware -> handler-fn")

            (~'[handler-id handler-fn              ] (~'add-handler! ~'handler-id ~'handler-fn nil))
            (~'[handler-id handler-fn dispatch-opts]
             {:arglists
              '([handler-id handler-fn]
                [handler-id handler-fn
                 {:as   dispatch-opts
                  :keys [async sample rate-limit filter-fn middleware,
                         ns-filter kind-filter id-filter min-level,
                         error-fn backp-fn]}])}

             (when ~'handler-fn
               (let [dispatch-opts# (enc/nested-merge ~default-dispatch-opts ~base-dispatch-opts ~'dispatch-opts)
                     wrapped-handler-fn#
                     (with-meta (wrap-handler ~'handler-id ~'handler-fn dispatch-opts#)
                       dispatch-opts#)]

                 (-get-handlers
                   (enc/update-var-root! ~*sig-handlers*
                     (fn [m#] (not-empty (assoc m# ~'handler-id wrapped-handler-fn#)))))))))

          ~add-shutdown-hook))))

#?(:clj
   (defmacro with-handler
     "Low-level util. Executes form with ONLY the given handler-fn registered.
     Useful for tests/debugging. See also `with-handler+`."

     ;; Given pre-wrapped handler-fn
     ([*sig-handlers* handler-id pre-wrapped-handler-fn form]
      `(binding [~*sig-handlers* {~handler-id ~pre-wrapped-handler-fn}]
         ~form))

     ;; Given unwrapped handler-fn
     ([*sig-handlers* handler-id unwrapped-handler-fn dispatch-opts form]
      `(with-handler ~*sig-handlers* ~handler-id
         (wrap-handler ~handler-id ~unwrapped-handler-fn ~dispatch-opts)
         ~form))))

#?(:clj
   (defmacro with-handler+
     "Low-level util. Executes form with the given handler-fn registered.
     Useful for tests/debugging. See also `with-handler`."
     {:added "Encore vX.Y.Z (YYYY-MM-DD)"}

     ;; Given pre-wrapped handler-fn
     ([*sig-handlers* handler-id pre-wrapped-handler-fn form]
      `(binding [~*sig-handlers* (assoc ~*sig-handlers* ~handler-id ~pre-wrapped-handler-fn)]
         ~form))

     ;; Given unwrapped handler-fn
     ([*sig-handlers* handler-id unwrapped-handler-fn dispatch-opts form]
      `(with-handler+ ~*sig-handlers* ~handler-id
         (wrap-handler ~handler-id ~unwrapped-handler-fn ~dispatch-opts)
         ~form))))

(comment
  (def ^:dynamic                   *sig-handlers* nil)
  (macroexpand '(def-handler-api 3 *sig-handlers* {}))
  (do           (def-handler-api 3 *sig-handlers* {}))

  (add-handler!    :hid1 (fn [x]) {})
  (remove-handler! :hid1)
  (get-handlers))

;;;;

#?(:clj
   (defmacro def-api
     "Calls both `def-filter-api` and `def-handler-api`."
     [sf-arity *sig-filter* *sig-handlers* opts]
     `(do
        (def-filter-api  ~sf-arity ~*sig-filter*   ~opts)
        (def-handler-api ~sf-arity ~*sig-handlers* ~opts))))

(comment (def-api 3 *sig-filter* *sig-handlers* {:purpose "testing"}))
