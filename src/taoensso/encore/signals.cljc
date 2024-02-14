(ns ^:no-doc taoensso.encore.signals
  "Experimental, subject to change without notice!
  Private low-level signal toolkit for use by Telemere, Tufte, Timbre, etc.

  \"Signal\" is used here as an internal name for any
  abstract event/object/data that:
    - Originates in an ns (generated or received there, etc.)
    - Has a level (priority, significance, etc.)
    - May have an identifier
    - May have a kind (type, taxonomy, etc.)"

  {:added "Encore v3.68.0 (2023-09-25)"}
  (:require
   [clojure.string  :as str]
   [taoensso.encore :as enc :refer [have have?]])

  #?(:cljs
     (:require-macros
      [taoensso.encore.signals :refer
       [valid-level-int valid-level level>=]])))

(comment
  (remove-ns 'taoensso.encore.signals)
  (:api (enc/interns-overview)))

;;;; Levels

(def ^:const level-aliases
  "Map of {<level-keyword> <level-integer>} aliases."
  (enc/nested-merge
    {          :trace 10 :debug 20         :info 50 :warn 60 :error 70 :fatal  80 :report  90
     :low--- 0 :low-- 10 :low-  20 :low 30 :med  50 :high 60 :high+ 70 :high++ 80 :high+++ 90}
    (enc/get-env {:as :edn} :taoensso.encore.signals/level-aliases<.platform><.edn>)))

(let [expected (conj (set (keys level-aliases)) 'integer)]
  (defn ^:no-doc bad-level!
    "Throws an `ex-info` for given invalid level."
    [x]
    (throw
      (ex-info "[encore/signals] Invalid level"
        {:level    {:value x, :type (type x)}
         :expected expected}))))

(defn ^:no-doc get-level-int
  "Returns valid integer level, or nil."
  [x]
  (enc/cond
    (keyword? x) (get level-aliases x)
    (integer? x) (long              x)))

(comment (get-level-int :bad))

#?(:clj
   (do
     (defmacro ^:no-doc valid-level-int
       "Returns valid integer level, or throws."
       [x]
       (if (enc/const-form? x)
         (do           (or (get-level-int x)  (bad-level! x)))
         `(let [x# ~x] (or (get-level-int x#) (bad-level! x#)))))

     (defmacro ^:no-doc valid-level
       "Returns valid level, or throws."
       [x]
       (if (enc/const-form? x)
         (do           (if (get-level-int x)  x  (bad-level! x)))
         `(let [x# ~x] (if (get-level-int x#) x# (bad-level! x#)))))

     (defmacro ^:no-doc const-level>=
       "Returns true, false, or nil (inconclusive)."
       [x y]
       (when (and (enc/const-form? x) (enc/const-form? y))
         (>= (long (valid-level-int x)) (long (valid-level-int y)))))

     (defmacro ^:no-doc level>=
       "Returns true if valid level `x` has value >= valid level `y`.
       Throws if either level is invalid."
       [x y]
       (if (and (enc/const-form? x) (enc/const-form? y))
         (>= (long (valid-level-int x)) (long (valid-level-int y)))
         `(let [~(with-meta 'x-level {:tag 'long}) (valid-level-int ~x)
                ~(with-meta 'y-level {:tag 'long}) (valid-level-int ~y)]
            (>= ~'x-level ~'y-level))))))

(comment (level>= :info :bad))

;;;; Basic filtering

(let [nf-compile  (fn [nf-spec  ] (enc/name-filter (or nf-spec :any)))
      nf-conform? (fn [nf-spec n] ((nf-compile nf-spec) n))
      nf->min-level
      (fn [min-level nf-arg]
        (if (vector? min-level)
          ;; [[<nf-spec> <min-level>] ... [\"*\" <min-level>]]
          (enc/rsome
            (fn [[nf-spec min-level]]
              (when (nf-conform? nf-spec nf-arg)
                (valid-level-int min-level)))
            (do            min-level))
          (valid-level-int min-level)))]

  (defn ^:no-doc valid-nf-spec
    "Returns valid `encore/name-filter` spec, or throws."
    [x]
    (if-let [t (enc/catching (do (nf-compile x) nil) t t)]
      (throw
        (ex-info
          (if (fn? x)
            "[encore/signals] Invalid name filter (fn filters no longer supported)"
            "[encore/signals] Invalid name filter")
          {:name-filter {:value x, :type (type x)}}
          t))
      x))

  (defn ^:no-doc allow-name?
    "Low-level name filter."
    #?(:cljs {:tag boolean})
    [nf-spec nf-arg]
    (if ^boolean (nf-conform? nf-spec nf-arg) true false))

  (defn ^:no-doc allow-level?
    "Low-level level filter."
    #?(:cljs {:tag boolean})
    ([min-level        level] (if ^boolean (level>= level min-level) true false))
    ([min-level nf-arg level]
     (let [min-level (nf->min-level min-level nf-arg)]
       (if  ^boolean (level>= level min-level) true false)))

    ([min-level kind nf-arg level]
     (if-let [min-level*
              (if (map? min-level) ; {<kind> <min-level*>}
                (or
                  (when kind (when-let [min-level* (get min-level     kind)] (nf->min-level min-level* nf-arg)))
                  (do        (when-let [min-level* (get min-level :default)] (nf->min-level min-level* nf-arg))))
                (do                                                          (nf->min-level min-level  nf-arg)))]
       (allow-level? min-level* level)
       true))))

(defn ^:no-doc valid-min-level
  "Returns valid min level, or throws."
  [x]
  (if (vector? x)
    (do
      (enc/run!
        (fn [[nf-spec min-level]]
          (valid-nf-spec nf-spec)
          (valid-level   min-level))
        x)
      x)
    (valid-level x)))

(defn update-min-level
  "Low-level util to update given min level."
  [old kind nf-spec new]
  (enc/cond
    :if-let [old-map (when (map? old) old)] ; {<kind> <min-level*>}
    (let [kind    (or kind :default)
          old-val (get old-map kind)
          new-val (update-min-level old-val nil nf-spec new)
          new-map
          (if (nil? new-val)
            (not-empty (dissoc old-map kind))
            (do        (assoc  old-map kind new-val)))]

      (if-let [simplified ; {:default <x>} -> <x>
               (when (= (count new-map) 1)
                 (get new-map :default))]
        simplified
        new-map))

    kind
    (let [old-map (if old {:default old} {})]
      (update-min-level old-map kind nf-spec new))

    (nil? nf-spec) (when new (valid-min-level new))

    :else ; Update name-specific min-level
    (let [new     (when new (valid-level new))
          nf-spec (valid-nf-spec nf-spec)

          old-vec (if (vector? old) old (if old [["*" (valid-level old)]] []))
          new-vec
          (not-empty
            (reduce ; Remove any pre-existing [<str> _] or [#{<str>} _] entries
              (fn [acc [nf-spec* _min-level :as entry]]
                (if-let [exact-match?
                         (or
                           (= nf-spec*   nf-spec)
                           (= nf-spec* #{nf-spec}))]
                  (do   acc)             ; Remove entry
                  (conj acc entry)       ; Retain entry
                  ))

              (if new
                [[nf-spec new]] ; Insert new entry at head
                [])

              old-vec))]

      (if-let [simplified ; [["*" <x>]] -> <x>
               (when (= (count new-vec) 1)
                 (let [[[nf-spec min-level]] new-vec]
                   (when (contains? #{"*" :any} nf-spec)
                     min-level)))]
        simplified
        new-vec))))

;;;; SigFilter

(comment (enc/defonce ^:dynamic *sig-filter* "`SigFilter`, or nil." nil))

(deftype SigFilter [ns-filter kind-filter id-filter min-level filter-fn]
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    {:ns-filter ns-filter :kind-filter kind-filter
     :id-filter id-filter :min-level   min-level})

  #?(:clj clojure.lang.IFn :cljs IFn)
  (#?(:clj invoke :cljs -invoke) [_ ns kind id level] (filter-fn ns kind id level))
  (#?(:clj invoke :cljs -invoke) [_ ns      id level] (filter-fn ns      id level))
  (#?(:clj invoke :cljs -invoke) [_ ns         level] (filter-fn ns         level)))

(enc/def* sig-filter
  "Returns nil, or a stateful (caching) `SigFilter` with the given specs."
  {:arglists
   '([{:keys [ns-filter kind-filter id-filter min-level]}]
             [ns-filter kind-filter id-filter min-level])}

  (let [get-cached
        (enc/fmemoize ; Same specs -> share cache (ref. transparent)
          (fn sig-filter
            [         ns-filter kind-filter id-filter min-level]
            (when (or ns-filter kind-filter id-filter min-level)
              (do ; Validation
                (when   ns-filter (valid-nf-spec   ns-filter))
                (when kind-filter (valid-nf-spec kind-filter))
                (when   id-filter (valid-nf-spec   id-filter))
                (when   min-level
                  (if (map? min-level)
                    (enc/run-kv!
                      (fn [kind min-level] (valid-min-level min-level))
                      min-level)

                    (valid-min-level min-level))))

              (SigFilter. ns-filter kind-filter id-filter min-level
                (enc/fmemoize
                  (fn allow-signal?

                    ;; Used for compile-time filtering (not perf sensitive, ignore nils)
                    ([{:keys [ns kind id level]}]
                     (and
                       (if (and   ns-filter ns)    (allow-name?   ns-filter   ns)         true)
                       (if (and kind-filter kind)  (allow-name? kind-filter kind)         true)
                       (if (and   id-filter id)    (allow-name? kind-filter   id)         true)
                       (if (and   min-level level) (allow-level? min-level kind ns level) true)))

                    ([ns kind id level]
                     (and
                       (if   ns-filter (allow-name?   ns-filter   ns)         true)
                       (if kind-filter (allow-name? kind-filter kind)         true)
                       (if   id-filter (allow-name?   id-filter   id)         true)
                       (if   min-level (allow-level? min-level kind ns level) true)))

                    ([ns id level]
                     (and
                       (if ns-filter (allow-name?  ns-filter ns)       true)
                       (if id-filter (allow-name?  id-filter id)       true)
                       (if min-level (allow-level? min-level ns level) true)))

                    ([ns level]
                     (and
                       (if ns-filter (allow-name?  ns-filter ns)       true)
                       (if min-level (allow-level? min-level ns level) true)))))))))]

    (fn sig-filter
      ([        ns-filter kind-filter id-filter min-level]             (get-cached ns-filter kind-filter id-filter min-level))
      ([{:keys [ns-filter kind-filter id-filter min-level :as specs]}] (get-cached ns-filter kind-filter id-filter min-level)))))

(comment ; [69.31 83.4 93.7]
  [(let [sf (sig-filter "*" nil nil nil)] (enc/qb 1e6 (sf :ns           :info)))
   (let [sf (sig-filter "*" nil nil nil)] (enc/qb 1e6 (sf :ns       :id :info)))
   (let [sf (sig-filter "*" nil nil nil)] (enc/qb 1e6 (sf :ns :kind :id :info)))])

(deftype HandlerContext [sample-rate]) ; Using an object for future extensibility (extra fields, etc.)

(defprotocol IFilterableSignal
  "Protocol that app/library signal-like types must implement to support signal API."
  (allow-signal? [_ sig-filter]      "Returns true iff given signal is allowed by given `SigFilter`.")
  (signal-value  [_ handler-context] "Returns signal's user-facing value as given to handlers, etc."))

(let [nil-sf (SigFilter. nil nil nil nil nil)]
  (defn update-sig-filter
    "Returns nil, or updated stateful (caching) `SigFilter`."
    {:arglists '([old-sig-filter {:keys [ns-filter kind-filter id-filter min-level min-level-fn]}])}
    [old specs]
    (let [^SigFilter base (or old nil-sf)]
      (if (empty? specs)
        old
        (sig-filter
          (get specs   :ns-filter   (.-ns-filter base))
          (get specs :kind-filter (.-kind-filter base))
          (get specs   :id-filter   (.-id-filter base))
          (let [old-min-level       (.-min-level base)]
            (enc/cond
              :if-let [e (find specs :min-level)]    (update-min-level old-min-level nil nil (val e))
              :if-let [f (get  specs :min-level-fn)] (f                old-min-level)
              :else                                                    old-min-level)))))))

(comment (update-sig-filter nil {}))

;;;; Expansion filtering

#?(:clj (enc/defonce ^:no-doc callsite-counter (enc/counter)))

(let [rate-limiters_ (enc/latom {})]
  (defn ^:no-doc callsite-limit!?
    "Calls the identified stateful rate-limiter and returns true iff limited."
    #?(:cljs {:tag boolean})
    [rl-id spec req-id]
    (let [rl
          (or
            (get (rate-limiters_) rl-id) ; Common case
            (rate-limiters_       rl-id #(or % (enc/rate-limiter {} spec))))]
      (if (rl req-id) true false))))

(comment (enc/qb 1e6 (callsite-limit!? :limiter-id1 [[1 4000]] :req-id))) ; 165.35

#?(:clj
   (defn ^:no-doc unexpected-sf-artity! [sf-arity context]
     (enc/unexpected-arg! sf-arity
       {:context  context
        :param    'sf-arity
        :expected #{2 3 4}})))

#?(:clj
   (defn filterable-expansion
     "Low-level util for writing macros with compile-time and runtime filtering.
     Returns {:keys [callsite-id elide? allow?]}.

     `macro-opts`, `opts-arg` are both of form:
       {:keys [kind id level sample-rate rate-limit when]}.

     Caller is responsible for protecting against possible multiple eval of
     forms in `opts-arg`."

     [{:as   macro-opts
       :keys [location opts-arg sf-arity ct-sig-filter rt-sig-filter]}]

     (when-not (or (nil? opts-arg) (map? opts-arg))
       (throw
         (ex-info "[encore/signals] `filterable-expansion` `opts-arg` must be a compile-time map"
           {:location location, :opts-arg opts-arg})))

     (let [{:keys [ns line column file]} location

           ;; Note that while `opts-arg` must be a const (compile-time) map,
           ;; its vals are arb forms that may need eval.
           find-opt-form (fn [k] (or (find macro-opts k) (find opts-arg k)))
           get-opt-form  (fn [k] (when-let [e (find-opt-form k)] (val e)))

           kind-form    (get-opt-form :kind)
           id-form      (get-opt-form :id)
           level-form   (get-opt-form :level)
           _
           (when (enc/const-form? level-form)
             (valid-level         level-form))

           elide?
           (when-let [sf ct-sig-filter]
             (not (sf {:ns    (enc/const-form         ns)
                       :kind  (enc/const-form  kind-form)
                       :id    (enc/const-form    id-form)
                       :level (enc/const-form level-form)})))

           ;; Unique id for this callsite expansion, changes on every eval. Means rate limiter
           ;; will get reset on eval during REPL work, etc.
           callsite-id (callsite-counter)

           base-rv {:callsite-id callsite-id}]

       (if elide?
         (assoc base-rv :allow? false, :elide? true)
         (let [allow?-form
               (let [sample-rate-form
                     (when-let [sr-form (get-opt-form :sample-rate)]
                       (if (enc/const-form? sr-form)
                         (do                     `(< ~'(Math/random) ~(enc/as-pnum! sr-form)))
                         `(if-let [~'sr ~sr-form] (< ~'(Math/random)  (double     ~'sr)) true)))

                     sf-form
                     (case (int (or sf-arity -1))
                       2 `(if-let [~'sf ~rt-sig-filter] (~'sf ~ns                     ~level-form) true)
                       3 `(if-let [~'sf ~rt-sig-filter] (~'sf ~ns            ~id-form ~level-form) true)
                       4 `(if-let [~'sf ~rt-sig-filter] (~'sf ~ns ~kind-form ~id-form ~level-form) true)
                       (unexpected-sf-artity! sf-arity `callsite-filter))

                     filter-form
                     (when-let [filter-form-entry (or (find-opt-form :filter) (find-opt-form :when))]
                       `(let [~'this-callsite-id ~callsite-id] ~(val filter-form-entry)))

                     rl-form ; Nb last (increments count)
                     (when-let [spec-form   (get-opt-form :rate-limit)]
                       (let    [rl-rid-form (get-opt-form :rl-rid)] ; Advanced, undocumented
                         `(if (callsite-limit!? ~callsite-id ~spec-form ~rl-rid-form) false true)))]

                 `(and ~@(filter some? [sample-rate-form sf-form filter-form rl-form])))]

           (assoc base-rv :allow? allow?-form))))))

(comment
  (filterable-expansion
    {:sf-arity 2, :rt-sig-filter `*foo*, :location {:ns (str *ns*)}
     :opts-arg
     {:filter      'false
      :level       (do :info)
      :sample-rate 0.3
      :rate-limit  [[1 1000]]}}))

;;;; Signal handling

(comment (enc/defonce ^:dynamic *sig-handlers* "?{<handler-id> <wrapped-handler-fn>}" nil))

(defn ^:no-doc -get-handlers [handlers] (when handlers (enc/map-vals meta handlers)))

(defn call-handlers!
  "Calls given handlers with the given signal.
  Signal's type must implement `IFilterableSignal`."
  [handlers signal]
  (enc/run-kv! (fn [_handler-id handler-fn] (handler-fn signal)) handlers)
  nil)

(defn shutdown-handlers!
  "Shuts down given handlers by calling them with no args."
  [handlers]
  (enc/run-kv! (fn [_handler-id handler-fn] (enc/catching (handler-fn))) handlers)
  nil)

(defn get-middleware-fn
  "Takes ?[<unary-fn> ... <unary-fn>] and returns nil, or a single unary fn
  that is the left->right composition of the others and that short-circuits if
  any returns nil."
  {:added "Encore v3.75.0 (2024-01-29)"}
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

(def ^:private default-dispatch-opts
  #?(:clj  {:async {:mode :dropping, :buffer-size 4096, :n-threads 1, :daemon-threads? false}}
     :cljs {}))

;;; Telemere will set these when it's present
(enc/defonce ^:dynamic *default-handler-error-fn* nil)
(enc/defonce ^:dynamic *default-handler-backp-fn* nil)

(defn wrap-handler
  "Wraps given handler-fn to add common handler-level functionality."
  [handler-id handler-fn
   {:as dispatch-opts
    :keys
    [#?(:clj async) sample-rate rate-limit filter-fn middleware,
     ns-filter kind-filter id-filter min-level,
     rl-error rl-backup error-fn backp-fn]}]

  (let [[sample-rate sample-rate-fn]
        (when      sample-rate
          (if (fn? sample-rate)
            [nil                sample-rate] ; Dynamic rate (use dynamic binding, deref atom, etc.)
            [(enc/as-pnum! sample-rate) nil] ; Static  rate
            ))

        rl-handler   (when-let [spec rate-limit] (enc/rate-limiter {} spec))
        sig-filter*  (sig-filter ns-filter kind-filter id-filter min-level)
        stopped?_    (enc/latom false)

        rl-error (get dispatch-opts :rl-error  (enc/rate-limiter {} [[1 (enc/ms :mins 1)]]))
        rl-backp (get dispatch-opts :rl-backup (enc/rate-limiter {} [[1 (enc/ms :mins 1)]]))
        error-fn (get dispatch-opts :error-fn  ::default)
        backp-fn (get dispatch-opts :backp-fn  ::default)

        middleware-fn (get-middleware-fn middleware) ; (fn [signal-value]) => transformed ?signal-value*
        wrapped-handler-fn
        (fn wrapped-handler-fn
          ([] ; Shutdown
           (when (enc/-cas!? stopped?_ false true)
             (enc/try*
               (handler-fn) ; Notify handler-fn to shutdown
               (catch :any t
                 (when (and error-fn (not (enc/identical-kw? error-fn ::default)))
                   (enc/catching (error-fn {:handler-id handler-id, :error t}))))) ; No :raw-signal
             true))

          ([signal] ; Raw signal
           (when-not (stopped?_)
             (enc/try* ; Non-specific (global) trap for perf
               (let [sample-rate (or sample-rate (when-let [f sample-rate-fn] (f)))
                     allow?
                     (and
                       (if sample-rate (< (Math/random) (double sample-rate))  true)
                       (if sig-filter* (allow-signal? signal sig-filter*)      true)
                       (if filter-fn   (filter-fn #_signal)                    true)
                       (if rl-handler  (if (rl-handler nil) false true)        true) ; Nb last (increments count)
                       )]

                 (or
                   (when allow?
                     (when-let [sig-val ; Raw signal -> library-level handler-arg
                                (signal-value signal
                                  (when              sample-rate
                                    (HandlerContext. sample-rate)))]

                       (if middleware-fn ; Library-level handler-arg -> arb user-level value
                         (when-let [sig-val (middleware-fn sig-val)] (handler-fn sig-val) true)
                         (do                                         (handler-fn sig-val) true))))
                   false))

               (catch :any t
                 (when error-fn
                   (enc/catching
                     (when-not (and rl-error (rl-error handler-id)) ; error-fn rate-limited
                       (when-let [error-fn
                                  (if (enc/identical-kw? error-fn ::default)
                                    *default-handler-error-fn*
                                    error-fn)]
                         (error-fn {:handler-id handler-id, :raw-signal signal, :error t})))))
                 false)))))]

    #?(:cljs wrapped-handler-fn
       :clj
       (if-not async
         wrapped-handler-fn
         (let [runner (enc/runner (have map? async))]
           (fn wrapped-handler-fn* [signal]
             (when-let [back-pressure? (false? (runner (fn [] (wrapped-handler-fn signal))))]
               (when backp-fn
                 (enc/catching
                   (when-not (and rl-backp (rl-backp handler-id)) ; backp-fn rate-limited
                     (let [backp-fn
                           (if (enc/identical-kw? backp-fn ::default)
                             *default-handler-backp-fn*
                             backp-fn)]
                       (backp-fn {:handler-id handler-id}))))))))))))

;;;; Local API

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
             (unexpected-sf-artity! sf-arity `def-filter-api))

           level-aliases `(enc/defalias level-aliases)

           filtering-help
           `(def ~'filtering-help
              ~(api-docstring 17 purpose
                 "Your filter config determines which %s calls will be enabled.

                 Filtering can occur at compile-time (=> elision), or runtime.
                 Both compile-time and runtime config can be specified via:

                   1. System values (JVM properties, environment variables, or
                      classpath resources). See library docs for details.

                   2. The filter API consting of the following:
                     `set-ns-filter!`,     `with-ns-filter`      - For filtering calls by namespace
                     `set-minimum-level!`, `with-minimum-level!` - For filtering calls by %s level
                     `set-id-filter!`,     `with-id-filter`      - For filtering calls by %s id   (when relevant)
                     `set-kind-filter!`,   `with-kind-filter`    - For filtering calls by %s kind (when relevant)

                     See the relevant docstrings for details.

                 Additional filtering can also be applied on a per-handler basis, see
                 `add-handler!` for details.

                 If anything is unclear, please ping me (@ptaoussanis) so that I can
                 improve these docs!")
              "See docstring")

           ns-filter
           `(do
              (defn ~'set-ns-filter!
                ~(api-docstring 19 purpose
                   "Sets %s call namespace filter based on given `ns-filter` spec.

                   `ns-filter` may be:
                     - A regex pattern of namespace/s to allow.
                     - A str/kw/sym, in which \"*\"s act as wildcards.
                     - A vector or set of regex patterns or strs/kws/syms.
                     - {:allow <spec> :deny <spec>} with specs as above.")
                ~'[ns-filter]
                (enc/force-ref
                  (enc/update-var-root! ~*sig-filter*
                    (fn [old#] (update-sig-filter old# {:ns-filter ~'ns-filter})))))

              (defmacro ~'with-ns-filter
                ~(api-docstring 19 purpose
                   "Executes form with given %s call namespace filter in effect.
                   See `set-ns-filter!` for details.")
                ~'[ns-filter form]
                `(binding [~'~*sig-filter* (update-sig-filter ~'~*sig-filter* {:ns-filter ~~'ns-filter})]
                   ~~'form)))

           kind-filter
           `(do
              (defn ~'set-kind-filter!
                ~(api-docstring 19 purpose
                   "Sets %s call kind filter based on given `kind-filter` spec.

                   `kind-filter` may be:
                     - A regex pattern of kind/s to allow.
                     - A str/kw/sym, in which \"*\"s act as wildcards.
                     - A vector or set of regex patterns or strs/kws/syms.
                     - {:allow <spec> :deny <spec>} with specs as above.")
                ~'[kind-filter]
                (enc/force-ref
                  (enc/update-var-root! ~*sig-filter*
                    (fn [old#] (update-sig-filter old# {:kind-filter ~'kind-filter})))))

              (defmacro ~'with-kind-filter
                ~(api-docstring 19 purpose
                   "Executes form with given %s call kind filter in effect.
                   See `set-kind-filter!` for details.")
                ~'[kind-filter form]
                `(binding [~'~*sig-filter* (update-sig-filter ~'~*sig-filter* {:kind-filter ~~'kind-filter})]
                   ~~'form)))

           id-filter
           `(do
              (defn ~'set-id-filter!
                ~(api-docstring 19 purpose
                   "Sets %s call id filter based on given `id-filter` spec.

                   `id-filter` may be:
                     - A regex pattern of id/s to allow.
                     - A str/kw/sym, in which \"*\"s act as wildcards.
                     - A vector or set of regex patterns or strs/kws/syms.
                     - {:allow <spec> :deny <spec>} with specs as above.")
                ~'[id-filter]
                (enc/force-ref
                  (enc/update-var-root! ~*sig-filter*
                    (fn [old#] (update-sig-filter old# {:id-filter ~'id-filter})))))

              (defmacro ~'with-id-filter
                ~(api-docstring 19 purpose
                   "Executes form with given %s call id filter in effect.
                   See `set-id-filter!` for details.")
                ~'[id-filter form]
                `(binding [~'~*sig-filter* (update-sig-filter ~'~*sig-filter* {:id-filter ~~'id-filter})]
                   ~~'form)))

           min-level
           (case sf-arity
             (2 3)
             `(do
                (defn ~'set-min-level!
                  ~(api-docstring 21 purpose
                     "Sets minimum %s call level based on given `min-level` spec.

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
                         (update-sig-filter old-sf#
                           {:min-level-fn
                            (fn [old-ml#]
                              (update-min-level old-ml# nil ~'ns-filter ~'min-level))}))))))

                (defmacro ~'with-min-level
                  ~(api-docstring 21 purpose
                     "Executes form with given minimum %s call level in effect.
                     See `set-min-level!` for details.")
                  (~'[          min-level form] (list '~'with-min-level nil ~'min-level ~'form))
                  (~'[ns-filter min-level form]
                   `(binding [~'~*sig-filter*
                              (update-sig-filter ~'~*sig-filter*
                                {:min-level-fn
                                 (fn [~'old-ml#]
                                   (update-min-level ~'old-ml# nil ~~'ns-filter ~~'min-level))})]
                      ~~'form))))

             (4)
             `(do
                (defn ~'set-min-level!
                  ~(api-docstring 21 purpose
                     "Sets minimum %s call level based on given `min-level` spec.

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
                         (update-sig-filter old-sf#
                           {:min-level-fn
                            (fn [old-ml#]
                              (update-min-level old-ml# ~'kind ~'ns-filter ~'min-level))}))))))

                (defmacro ~'with-min-level
                  ~(api-docstring 21 purpose
                     "Executes form with given minimum %s call level in effect.
                     See `set-min-level!` for details.")
                  (~'[kind           min-level form] (list '~'with-min-level ~'kind nil ~'min-level ~'form))
                  (~'[kind ns-filter min-level form]
                   `(binding [~'~*sig-filter*
                              (update-sig-filter ~'~*sig-filter*
                                {:min-level-fn
                                 (fn [~'old-ml#]
                                   (update-min-level ~'old-ml# ~~'kind ~~'ns-filter ~~'min-level))})]
                      ~~'form)))))]

       (case sf-arity
         2 `(do ~ns-filter                         ~min-level ~level-aliases ~filtering-help)
         3 `(do ~ns-filter              ~id-filter ~min-level ~level-aliases ~filtering-help)
         4 `(do ~ns-filter ~kind-filter ~id-filter ~min-level ~level-aliases ~filtering-help)))))

(comment
  (def ^:dynamic                  *sig-filter* nil)
  (macroexpand '(def-filter-api 3 *sig-filter* {}))
  (do           (def-filter-api 3 *sig-filter* {})))

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
                             (shutdown-handlers! ~*sig-handlers*))))))]

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

                    Supports `:blocking`, `:dropping`, and `:sliding` back pressure modes.
                    NB handling order may be non-sequential when `n-threads` > 1.

                 `sample-rate`
                   Optional sample rate ∈ℝ[0,1], or (fn dyamic-sample-rate []) => ℝ[0,1].
                   When present, handle only this (random) proportion of args:
                     1.0 => handle every arg (same as `nil` rate, default)
                     0.0 => noop   every arg
                     0.5 => handle random 50%% of args

                 `ns-filter`   - Namespace filter as in `set-ns-filter!`
                 `kind-filter` - Kind      filter as in `set-kind-filter!` (when relevant)
                 `id-filter`   - Id        filter as in `set-id-filter!`   (when relevant)
                 `min-level`   - Minimum   level  as in `set-min-level!`

                 `filter-fn`
                   Optional nullary (fn allow? []) that must return truthy for handler to be
                   called. When present, called *after* sampling and other filters, but before
                   rate limiting.

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
                   to `handler-arg` before passing to `handler-fn`. If any middleware fn returns
                   nil, aborts immediately without calling `handler-fn`.

                   Useful for transforming `handler-arg` before handling.

                 `error-fn` - (fn [{:keys [handler-id handler-arg error]}]) to call on handler error.
                 `backp-fn` - (fn [{:keys [handler-id                  ]}]) to call on handler back pressure.

               Flow sequence:

                 1. Per call (n=1):
                   a. Sampling
                   b. Filtering (namespace, kind, id, level, filter-fn)
                   c. Rate limiting
                   d. Middleware

                 2. Per handler (n>=0):
                   a. Sampling
                   b. Filtering (namespace, kind, id, level, filter-fn)
                   c. Rate limiting
                   d. Middleware
                   e. Hander fn

                 Note: call filters should generally be at least as permissive as handler filters,
                 otherwise calls will be suppressed before reaching handlers.")

            (~'[handler-id handler-fn              ] (~'add-handler! ~'handler-id ~'handler-fn nil))
            (~'[handler-id handler-fn dispatch-opts]
             {:arglists
              '([handler-id handler-fn]
                [handler-id handler-fn
                 {:as   dispatch-opts
                  :keys [async sample-rate rate-limit filter-fn middleware,
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
     {:added "Encore v3.75.0 (2024-01-29)"}

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

#?(:clj
   (defmacro def-api
     "Calls both `def-filter-api` and `def-handler-api`."
     [sf-arity *sig-filter* *sig-handlers* opts]
     `(do
        (def-filter-api  ~sf-arity ~*sig-filter*   ~opts)
        (def-handler-api ~sf-arity ~*sig-handlers* ~opts))))

(comment (def-api 3 *sig-filter* *sig-handlers* {:purpose "testing"}))
