(ns ^:no-doc taoensso.encore.signals
  "Experimental, subject to change without notice!
  Private signal toolkit for use by Telemere, Tufte, Timbre, etc.

  \"Signal\" refers here to any abstract event/data/object (usu. map) that:
    - Originates in an ns (created or received there, etc.)
    - Has a level (priority/significance/etc.)
    - May have a kind (type/taxonomy/etc.)
    - May have an identifier"

  {:added "Encore v3.68.0 (2023-09-25)"}
  (:refer-clojure :exclude [binding])
  (:require
   [clojure.string  :as str]
   [taoensso.encore :as enc :refer [binding have have?]]
   [taoensso.encore.stats :as stats])

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
  (conj
    {          :trace 10 :debug 20         :info 50 :warn 60 :error 70 :fatal  80 :report  90
     :low--- 0 :low-- 10 :low-  20 :low 30 :med  50 :high 60 :high+ 70 :high++ 80 :high+++ 90}
    (enc/get-env {:as :edn} :taoensso.encore.signals/level-aliases<.platform><.edn>)))

(let [expected (conj (set (keys level-aliases)) 'integer)]
  (defn bad-level!
    "Throws an `ex-info` for given invalid level."
    [x]
    (throw
      (ex-info "[encore/signals] Invalid level"
        {:level    (enc/typed-val x)
         :expected expected}))))

(defn ^:no-doc -valid-level-int ^long [x]
  (enc/cond
    (keyword? x) (or (get level-aliases x) (bad-level! x))
    (integer? x) (long x)
    :else  (bad-level! x)))

(defn ^:no-doc -valid-level [x]
  (enc/cond
    (keyword? x) (if (get level-aliases x) x (bad-level! x))
    (integer? x)      x
    :else (bad-level! x)))

(comment (enc/qb 1e6 (-valid-level-int :info))) ; 52.88

#?(:clj
   (do
     (defmacro valid-level-int "Returns valid integer level, or throws."
       [x] (if (enc/const-form? x) (-valid-level-int x) `(-valid-level-int ~x)))

     (defmacro valid-level "Returns valid level, or throws."
       [x] (if (enc/const-form? x) (-valid-level x) `(-valid-level ~x)))

     (defmacro level>=
       "Returns true if valid level `x` has value >= valid level `y`.
       Throws if either level is invalid."
       [x y]
       (if (and (enc/const-form? x) (enc/const-form? y))
         (>= (long (valid-level-int x)) (long (valid-level-int y)))
         (let [x (if (enc/const-form? x) (valid-level-int x) `(-valid-level-int ~x))
               y (if (enc/const-form? y) (valid-level-int y) `(-valid-level-int ~y))]
           `(>= ~x ~y))))))

(comment (macroexpand '(level>= :info x)))

;;;; Basic filtering

(let [nf-compile (fn [nf-spec  ] (enc/name-filter (or nf-spec :any)))
      nf-match?  (fn [nf-spec n] ((nf-compile nf-spec) n))
      nf->min-level
      (fn [ml-spec nf-arg]
        (if (vector? ml-spec)
          ;; [[<nf-spec> <min-level>] ... [\"*\" <min-level>]]
          (enc/rsome
            (fn [[nf-spec min-level]]
              (when (nf-match? nf-spec nf-arg)
                min-level))
            ml-spec)
          ml-spec))]

  (defn valid-nf-spec
    "Returns valid `encore/name-filter` spec, or throws."
    [x]
    (if-let [t (enc/try* (do (nf-compile x) nil) (catch :all t t))]
      (throw
        (ex-info
          (if (fn? x)
            "[encore/signals] Invalid name filter (fn filters no longer supported)"
            "[encore/signals] Invalid name filter")
          {:name-filter (enc/typed-val x)}
          t))
      x))

  (defn allow-name?
    "Low-level name filter."
    #?(:cljs {:tag 'boolean})
    [nf-spec nf-arg]
    (if ^boolean (nf-match? nf-spec nf-arg) true false))

  (defn parse-min-level
    "Returns simple unvalidated ?min-level from {<kind> [[<nf-spec> <min-level>] ...]}, etc."
    [ml-spec kind nf-arg]
    (if (map? ml-spec) ; {<kind> <ml-spec*>}
      (or
        (when kind (when-let [ml-spec* (get ml-spec     kind)] (nf->min-level ml-spec* nf-arg)))
        (do        (when-let [ml-spec* (get ml-spec :default)] (nf->min-level ml-spec* nf-arg))))
      (do                                                      (nf->min-level ml-spec  nf-arg))))

  (let [parse-min-level parse-min-level]
    (defn allow-level?
      "Low-level level filter."
      #?(:cljs {:tag 'boolean})
      ([min-level      level] (if ^boolean (level>= level min-level) true false))
      ([ml-spec nf-arg level]
       (let [min-level (nf->min-level ml-spec nf-arg)]
         (if  ^boolean (level>= level min-level) true false)))

      ([ml-spec kind nf-arg level]
       (if ml-spec
         (allow-level? (parse-min-level ml-spec kind nf-arg) level)
         true)))))

(defn valid-min-level
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

;;;; Signal filtering

(comment (enc/defonce ^:dynamic *rt-sig-filter* "`SigFilter`, or nil." nil))

(deftype SigFilter [kind-filter ns-filter id-filter min-level filter-fn]
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    (enc/assoc-some nil
      :kind-filter kind-filter :ns-filter ns-filter
      :id-filter     id-filter :min-level min-level))

  #?(:clj clojure.lang.IFn :cljs IFn)
  (#?(:clj invoke :cljs -invoke) [_ kind ns id level] (filter-fn kind ns id level))
  (#?(:clj invoke :cljs -invoke) [_      ns id level] (filter-fn      ns id level))
  (#?(:clj invoke :cljs -invoke) [_      ns    level] (filter-fn      ns    level))
  (#?(:clj invoke :cljs -invoke) [_           ct-map] (filter-fn ct-map)))

(defn sig-filter?
  "Returns true iff given a `SigFilter`."
  #?(:cljs {:tag 'boolean})
  [x] (instance? SigFilter x))

(enc/def* sig-filter
  "Returns nil, or a stateful (caching) `SigFilter` with the given specs."
  {:arglists
   '([{:keys [kind-filter ns-filter id-filter min-level]}]
             [kind-filter ns-filter id-filter min-level])}

  (let [get-cached
        (enc/fmemoize ; Same specs -> share cache (ref. transparent)
          (fn sig-filter
            [         kind-filter ns-filter id-filter min-level]
            (when (or kind-filter ns-filter id-filter min-level)
              (do ; Validation
                (when kind-filter (valid-nf-spec kind-filter))
                (when   ns-filter (valid-nf-spec   ns-filter))
                (when   id-filter (valid-nf-spec   id-filter))
                (when   min-level
                  (if (map? min-level)
                    (enc/run-kv!
                      (fn [kind min-level] (valid-min-level min-level))
                      min-level)

                    (valid-min-level min-level))))

              (SigFilter. kind-filter ns-filter id-filter min-level
                (enc/fmemoize
                  (fn allow-signal?

                    ([{:keys [kind ns id level]}]
                     ;; Used for compile-time filtering (not perf sensitive, ignore nils)
                     (and
                       (if (and kind-filter kind)  (allow-name? kind-filter kind)         true)
                       (if (and   ns-filter ns)    (allow-name?   ns-filter   ns)         true)
                       (if (and   id-filter id)    (allow-name?   id-filter   id)         true)
                       (if (and   min-level level) (allow-level? min-level kind ns level) true)))

                    ([kind ns id level]
                     (and
                       (if kind-filter (allow-name? kind-filter kind)         true)
                       (if   ns-filter (allow-name?   ns-filter   ns)         true)
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
      ([        kind-filter ns-filter id-filter min-level]             (get-cached kind-filter ns-filter id-filter min-level))
      ([{:keys [kind-filter ns-filter id-filter min-level :as specs]}] (get-cached kind-filter ns-filter id-filter min-level)))))

(comment ; [188.41 189.56 188.09]
  (let [sf (sig-filter nil "*" nil nil)]
    (enc/qb 3e6
      (sf :kind :ns     :info)
      (sf       :ns :id :info)
      (sf       :ns :id :info))))

(defprotocol IFilterableSignal
  "Protocol that app/library signal-like types must implement to support signal API."
  (allow-signal? [_ sig-filter]          "Returns true iff given signal is allowed by given `SigFilter`.")
  (signal-value  [_ handler-sample-rate] "Returns signal's user-facing value as given to handlers, etc."))

(let [nil-sf (SigFilter. nil nil nil nil nil)]
  (defn update-sig-filter
    "Returns nil, or updated stateful (caching) `SigFilter`."
    {:arglists '([old-sig-filter {:keys [kind-filter ns-filter id-filter min-level min-level-fn]}])}
    [old specs]
    (let [^SigFilter base (or old nil-sf)]
      (if (empty? specs)
        old
        (sig-filter
          (get specs :kind-filter (.-kind-filter base))
          (get specs   :ns-filter   (.-ns-filter base))
          (get specs   :id-filter   (.-id-filter base))
          (let [old-min-level       (.-min-level base)]
            (enc/cond
              :if-let [e (find specs :min-level)]    (update-min-level old-min-level nil nil (val e))
              :if-let [f (get  specs :min-level-fn)] (f                old-min-level)
              :else                                                    old-min-level)))))))

(comment (update-sig-filter nil {}))

;;;; Filterable expansions

#?(:clj (enc/defonce expansion-counter (enc/counter)))

(let [rate-limiters_ (enc/latom {})]
  (defn expansion-limited!?
    "Calls the identified stateful rate-limiter and returns true iff limited."
    #?(:cljs {:tag 'boolean})
    [rl-id spec]
    (let [rl
          (or
            (get (rate-limiters_) rl-id) ; Common case
            (rate-limiters_       rl-id #(or % (enc/rate-limiter {:basic? true} spec))))]
      (if (rl) true false))))

(comment (enc/qb 1e6 (expansion-limited!? :limiter-id1 [[1 4000]]))) ; 54.02

#?(:clj
   (defn unexpected-sf-artity! [sf-arity context]
     (enc/unexpected-arg! sf-arity
       {:context  context
        :param    'sf-arity
        :expected #{2 3 4}})))

#?(:clj
   (defn- const-form! [param form]
     (if (enc/const-form? form)
       (do                form)
       (throw
         (ex-info "[encore/signals] `filterable-expansion` arg must be a const (compile-time) value"
           {:param param, :form form})))))

#?(:clj
   (defn filterable-expansion
     "Low-level util for writing macros with compile-time and runtime filtering.
     Returns {:keys [expansion-id location elide? allow?]}.

     Caller is responsible for protecting against possible multiple eval of
     forms in `opts`."

     {:arglists
      '([{:keys [macro-form macro-env, sf-arity ct-sig-filter *rt-sig-filter*]}]
        [{:keys
          [elide? allow? expansion-id,
           elidable? location sample-rate kind ns id level filter/when rate-limit rl-rid]}])}

     [{:as core-opts
       :keys [macro-form macro-env, sf-arity ct-sig-filter *rt-sig-filter*]}
      call-opts]

     (const-form! 'call-opts      call-opts) ; Must be const map, though vals may be arb forms
     (enc/have? [:or nil? map?]   call-opts)
     (const-form! 'elide?    (get call-opts :elide?))
     (const-form! 'elidable? (get call-opts :elidable?))

     (enc/have? #(contains? core-opts %) :macro-form :macro-env :ct-sig-filter :*rt-sig-filter*)
     (enc/have? [:or nil? sig-filter?] ct-sig-filter)
     (enc/have? qualified-symbol?     *rt-sig-filter*)

     (let [opts call-opts
           location ; {:keys [ns line column file]} forms
           (let [location-form (get opts :location (enc/get-source macro-form macro-env))
                 location-map  (when (map?    location-form) location-form) ; At least keys const
                 location-sym  (when (symbol? location-form) location-form)]

             (enc/assoc-some nil
               :ns     (get opts :ns     (get location-map :ns     (when location-sym `(get ~location-sym :ns))))      ;   Documented override
               :line   (get opts :line   (get location-map :line   (when location-sym `(get ~location-sym :line))))    ; Undocumented override
               :column (get opts :column (get location-map :column (when location-sym `(get ~location-sym :column))))  ; ''
               :file   (get opts :file   (get location-map :file   (when location-sym `(get ~location-sym :file))))))  ; ''

           kind-form*  (get opts     :kind)
           ns-form*    (get location :ns)
           id-form*    (get opts     :id)
           level-form* (get opts     :level)

           bound-forms (get opts :bound-forms) ; {:kind '__kind, etc.}
           kind-form   (or (get bound-forms :kind)  kind-form*)
           ns-form     (or (get bound-forms :ns)    ns-form*)
           id-form     (or (get bound-forms :id)    id-form*)
           level-form  (or (get bound-forms :level) level-form*)
           _
           (when (enc/const-form? level-form*)
             (valid-level         level-form*))

           elide?
           (and
             (get opts :elidable? true)
             (get opts :elide?
               (when-let [sf ct-sig-filter]
                 (not (sf {:kind  (enc/const-form  kind-form*)
                           :ns    (enc/const-form    ns-form*)
                           :id    (enc/const-form    id-form*)
                           :level (enc/const-form level-form*)})))))

           ;; Unique id for this expansion, changes on every eval.
           ;; So rate limiter will get reset on eval during REPL work, etc.
           expansion-id (get opts :expansion-id (expansion-counter))
           base-rv {:expansion-id  expansion-id, :location location}]

       (if elide?
         (assoc base-rv :elide? true)
         (let [allow?-form
               (get opts :allow?
                 (let [sample-rate-form
                       (when-let [sr-form (get opts :sample-rate)]
                         (if (enc/const-form? sr-form)
                           (do                     `(< ~'(Math/random) ~(enc/as-pnum! sr-form)))
                           `(if-let [~'sr ~sr-form] (< ~'(Math/random)  (double     ~'sr)) true)))

                       sf-form
                       (case (int (or sf-arity -1))
                         2 `(let [~'sf ~*rt-sig-filter*] (if ~'sf (~'sf            ~ns-form          ~level-form) true))
                         3 `(let [~'sf ~*rt-sig-filter*] (if ~'sf (~'sf            ~ns-form ~id-form ~level-form) true))
                         4 `(let [~'sf ~*rt-sig-filter*] (if ~'sf (~'sf ~kind-form ~ns-form ~id-form ~level-form) true))
                         (unexpected-sf-artity! sf-arity `expansion-filter))

                       when-form
                       (when-let [when-form (get opts :when)]
                         `(let [~'this-expansion-id ~expansion-id] ~when-form))

                       rl-form ; Nb last (increments count)
                       (when-let [spec-form (get opts :rate-limit)]
                         `(if (expansion-limited!? ~expansion-id ~spec-form) false true))]

                   `(enc/and* ~@(filter some? [sample-rate-form sf-form when-form rl-form]))))]

           (assoc base-rv :allow? allow?-form))))))

(comment
  (filterable-expansion
    {:macro-form nil, :macro-env nil,
     :sf-arity 2, :ct-sig-filter nil, :*rt-sig-filter* `*rt-sf*}
    {:location    {:ns (str *ns*)}
     :line        42
     :filter      'false
     ;; :elide?   true
     ;; :allow?   false
     :level       (do :info)
     :sample-rate 0.3
     :rate-limit  [[1 1000]]}))

;;;; Signal handling

(comment (enc/defonce ^:dynamic *sig-handlers* "?[<wrapped-handler-fn>]" nil))

(defn get-handlers-map
  "Returns non-empty ?{<handler-id> {:keys [dispatch-opts handler-fn ...]}}."
  ([handlers-vec     ] (get-handlers-map handlers-vec false))
  ([handlers-vec raw?]
   (when handlers-vec
     (reduce
       (fn [m wrapped-handler-fn]
         (let [whm (meta wrapped-handler-fn)]
           (assoc m (get whm :handler-id)
             (if raw?
               whm
               (let [info (select-keys  whm [:dispatch-opts :handler-fn])]
                 (if-let [stats-fn (get whm :stats-fn)]
                   (assoc info :handler-stats_ (delay (stats-fn)))
                   (do    info)))))))
       nil handlers-vec))))

(defn get-handlers-stats
  "Returns non-empty ?{<handler-id> ?{:keys [handling-nsecs counts]}}."
  [handlers-vec]
  (when-let [handlers-map (get-handlers-map handlers-vec :raw)]
    (reduce-kv
      (fn [m handler-id {:keys [stats-fn]}]
        (if stats-fn
          (assoc m handler-id (stats-fn))
          (do    m)))
      nil handlers-map)))

(defn call-handlers!
  "Calls given handlers with the given signal.
  Signal's type must implement `IFilterableSignal`."
  [handlers-vec signal]
  (run! (fn [wrapped-handler-fn] (wrapped-handler-fn signal)) handlers-vec))

(defn stop-handlers!
  "Stops handlers in parallel and returns
  {<handler-id> {:keys [okay error drained?]}}."
  [handlers-vec]
  (when-let [handlers-map (get-handlers-map handlers-vec :raw)]
    (let [results ; {<handler-id> <result-or-promise>}
          (reduce-kv
            (fn [m handler-id {:keys [wrapped-handler-fn]}]
              (assoc m handler-id
                #?(:clj (enc/promised :daemon (wrapped-handler-fn))
                   :cljs                      (wrapped-handler-fn))))
            nil handlers-map)]

      #?(:cljs results
         :clj
         (reduce-kv
           (fn [    m handler-id  result_]
             (assoc m handler-id @result_))
           results results)))))

(defn get-wrapped-handler-fn
  "Returns wrapped-handler-fn with given handler-id, or nil."
  [handlers-vec handler-id]
  (enc/rfirst #(= (get (meta %) :handler-id) handler-id) handlers-vec))

(defn remove-handler
  "Returns updated, non-empty handlers vec."
  [handlers-vec handler-id]
  (not-empty
    (filterv #(not= (get (meta %) :handler-id) handler-id)
      handlers-vec)))

(comment
  (let [handlers
        [(with-meta (fn []) {:handler-id :hid1})
         (with-meta (fn []) {:handler-id :hid2})]]

    [(get-handler    handlers :hid2)
     (remove-handler handlers :hid2)]))

(declare wrap-handler default-handler-priority)
(defn add-handler
  "Returns updated, non-empty handlers vec."

  ;; Given pre-wrapped handler-fn
  ([handlers-vec handler-id pre-wrapped-handler-fn]
   (if-not pre-wrapped-handler-fn
     handlers-vec
     (enc/sortv #(get-in (meta %) [:dispatch-opts :priority] default-handler-priority) enc/rcompare
       (conj (or (remove-handler handlers-vec handler-id) []) pre-wrapped-handler-fn))))

  ;; Given unwrapped handler-fn
  ([handlers-vec handler-id unwrapped-handler-fn, lib-dispatch-opts dispatch-opts]
   (if-not unwrapped-handler-fn
     handlers-vec
     (if (get dispatch-opts :no-wrap?) ; Undocumented
       (add-handler handlers-vec handler-id unwrapped-handler-fn)
       (add-handler handlers-vec handler-id
         (wrap-handler handler-id unwrapped-handler-fn, lib-dispatch-opts dispatch-opts))))))

;;; Telemere will set these when it's present
(enc/defonce ^:dynamic *default-handler-error-fn* nil)
(enc/defonce ^:dynamic *default-handler-backp-fn* nil)

(defn as-middleware-fn
  "Returns (composed) unary middleware fn, or nil."
  [fn-or-fns]
  (when                    fn-or-fns
    (if (vector?           fn-or-fns)
      (enc/comp-middleware fn-or-fns)
      (if (fn? fn-or-fns)
        (do    fn-or-fns)
        (throw
          (ex-info "[encore/signals] Unexpected middleware value"
            {:given    (enc/typed-val fn-or-fns)
             :expected '#{nil fn [f1 f2 ...]}}))))))

(def ^:private default-handler-priority 100)
(def           default-handler-dispatch-opts
  "Default handler dispatch options, see
  `help:handler-dispatch-options` for details."
  #?(:cljs
     {:priority default-handler-priority
      :track-stats? false}

     :clj
     {:priority default-handler-priority,
      :track-stats? true
      :async
      {:mode :blocking
       :buffer-size      1024
       :n-threads        1
       :drain-msecs      6000
       :convey-bindings? true}}))

(defn wrap-handler
  "Wraps given handler-fn to add common handler-level functionality."
  [handler-id handler-fn, lib-dispatch-opts user-dispatch-opts]
  (let [dispatch-opts
        (enc/nested-merge
          default-handler-dispatch-opts ; Encore  defaults
          lib-dispatch-opts             ; Library defaults
          (when-let [m (meta handler-fn)]
            (get m :dispatch-opts))     ; Unwrapped handler-fn defaults
          user-dispatch-opts)

        {:keys
         [#?(:clj async) priority sample-rate rate-limit when-fn middleware,
          kind-filter ns-filter id-filter min-level,
          rl-error rl-backp error-fn backp-fn, track-stats?]}
        dispatch-opts

        [sample-rate sample-rate-fn]
        (when      sample-rate
          (if (fn? sample-rate)
            [nil                sample-rate] ; Dynamic rate (use dynamic binding, deref atom, etc.)
            [(enc/as-pnum! sample-rate) nil] ; Static  rate
            ))

        rl-handler    (when-let [spec rate-limit] (enc/rate-limiter {:basic? true} spec))
        sig-filter*   (sig-filter kind-filter ns-filter id-filter min-level)

        middleware    (as-middleware-fn    middleware) ; Deprecated, kept temporarily for back compatibility
        ;; middleware (have [:or nil? fn?] middleware) ; (fn [signal-value]) => ?modified-signal-value

        rl-error (get dispatch-opts :rl-error (enc/rate-limiter-once-per (enc/ms :mins 1)))
        rl-backp (get dispatch-opts :rl-backp (enc/rate-limiter-once-per (enc/ms :mins 1)))
        backp-fn (get dispatch-opts :backp-fn ::default)
        error-fn (get dispatch-opts :error-fn ::default)
        error-fn*
        (when error-fn
          (fn [signal error]
            (enc/catching
              (if (and rl-error (rl-error)) ; error-fn rate-limited
                nil ; noop
                (when-let [error-fn
                           (if (enc/identical-kw? error-fn ::default)
                             *default-handler-error-fn*
                             error-fn)]
                  (if signal
                    (error-fn {:handler-id handler-id, :signal signal, :error error})
                    (error-fn {:handler-id handler-id,                 :error error})))))))

        runner
        #?(:cljs nil
           :clj
           (when async
             (enc/runner
               (assoc (have map? async)
                 :auto-stop?  false
                 :drain-msecs 0))))

        stopped?_ (enc/latom false)
        stop-fn ; Block => {:keys [okay error drained?]}
        (fn []
          ;; Called by `remove-handler!`, `with-handler/+`, `stop-handlers!`
          (if-not (compare-and-set! stopped?_ false true) ; Stop accepting new signals
            #?(:cljs                 {:okay :stopped}
               :clj  (enc/assoc-some {:okay :stopped}
                       :drained? (when runner (boolean (deref @runner 0 nil)))))

            (let [drained? ; Block <= `:drain-msecs` to finish handling current signals
                  #?(:cljs nil
                     :clj
                     (when runner
                       (boolean
                         (when-let [drained_ @runner]
                           (if-let [drain-msecs (get-in dispatch-opts [:async :drain-msecs])]
                             (deref drained_ drain-msecs nil)
                             (deref drained_))))))

                  handler-result
                  (enc/try*
                    (handler-fn) ; Give hander-fn opportunity to finalize, etc.
                    {:okay :stopped}

                    ;; Note that Cljs doesn't enforce runtime arity.
                    ;; ((fn f1 [x])) will actually trigger (f1 nil), so it's best for
                    ;; handlers to always include both (0, 1) arities.

                    #?(:clj (catch clojure.lang.ArityException _ {:okay :stopped}))
                    (catch :all t
                      (when error-fn* (error-fn* nil t))
                      {:error t})

                    (finally (when runner (runner))))]

              (enc/assoc-some handler-result :drained? drained?))))

        ssb (when track-stats? (stats/summary-stats-buffered-fast 1e5 nil))

        [cnt-allowed cnt-disallowed cnt-handled cnt-errors cnt-backp,
         cnt-sampled cnt-filtered cnt-rate-limited cnt-suppressed cnt-dropped]
        (when track-stats? (repeatedly 10 enc/counter))

        handle-signal! ; Block => truthy
        (fn [sig-raw]
          (let [ns0 (when track-stats? (enc/now-nano))
                result
                (or
                  (enc/try* ; Non-specific (global) trap
                    (let [sample-rate (or sample-rate (when-let [f sample-rate-fn] (f)))
                          allow?
                          (if track-stats?
                            (enc/and*
                              (if sample-rate (if (< (Math/random) (double sample-rate)) true (do (cnt-sampled)  false)) true)
                              (if sig-filter* (if (allow-signal? sig-raw sig-filter*)    true (do (cnt-filtered) false)) true)
                              (if when-fn     (if (when-fn #_sig-raw)                    true (do (cnt-filtered) false)) true)
                              (if rl-handler  (if (rl-handler) (do (cnt-rate-limited) false) true) true) ; Nb last (increments count)
                              )

                            (enc/and*
                              (if sample-rate (< (Math/random) (double sample-rate))  true)
                              (if sig-filter* (allow-signal? sig-raw sig-filter*)     true)
                              (if when-fn     (when-fn #_sig-raw)                     true)
                              (if rl-handler  (if (rl-handler) false true)            true) ; Nb last (increments count)
                              ))]

                      (when track-stats? (if allow? (cnt-allowed) (cnt-disallowed)))

                      (when allow?
                        (when-let [sig-val (signal-value sig-raw sample-rate)]
                          (enc/try*
                            (enc/if-not
                                [sig-val*
                                 (if middleware
                                   (middleware sig-val) ; Apply handler middleware
                                   (do         sig-val))]

                              (do (when track-stats? (cnt-suppressed)) nil)
                              (enc/try*
                                (handler-fn sig-val*)
                                (when track-stats? (cnt-handled))
                                true
                                (catch :all t (when track-stats? (cnt-errors)) (when error-fn* (error-fn* sig-val* t)))))
                            (catch     :all t (when track-stats? (cnt-errors)) (when error-fn* (error-fn* sig-val  t)))))))
                    (catch             :all t (when track-stats? (cnt-errors)) (when error-fn* (error-fn* sig-raw  t))))
                  false)]

            (when track-stats? (ssb (- (enc/now-nano) ^long ns0)))
            result))

        wrapped-handler-fn
        (or
          #?(:clj
             (when runner
               (fn async-wrapped-handler-fn
                 ([       ] (stop-fn))
                 ([sig-raw]
                  (if (stopped?_)
                    (do (when track-stats? (cnt-dropped)) nil)
                    ;; Note that fn passed to runner isn't itself subject to (stopped?_)
                    (when-let [back-pressure? (false? (runner (fn [] (handle-signal! sig-raw))))]
                      (when track-stats? (cnt-backp))
                      (when backp-fn
                        (enc/catching
                          (if (and rl-backp (rl-backp)) ; backp-fn rate-limited
                            nil                         ; noop
                            (let [backp-fn
                                  (if (enc/identical-kw? backp-fn ::default)
                                    *default-handler-backp-fn*
                                    backp-fn)]
                              (backp-fn {:handler-id handler-id})))))))))))

          (fn sync-wrapped-handler-fn
            ([       ] (stop-fn))
            ([sig-raw]
             (if (stopped?_)
               (do (when track-stats? (cnt-dropped)) nil)
               (handle-signal! sig-raw)))))

        stats-fn
        (when track-stats?
          (fn
            ([action] (throw (ex-info "Not currently implemented" {})))
            ([]
             {:handling-nsecs (when-let [sstats @ssb] @sstats)
              :counts ; Chronologically
              {:dropped       @cnt-dropped
               :back-pressure @cnt-backp

               :sampled       @cnt-sampled
               :filtered      @cnt-filtered
               :rate-limited  @cnt-rate-limited
               :disallowed    @cnt-disallowed
               :allowed       @cnt-allowed

               :suppressed    @cnt-suppressed
               :handled       @cnt-handled
               :errors        @cnt-errors}})))]

    (with-meta wrapped-handler-fn
      {:handler-id         handler-id
       :handler-fn         handler-fn
       :dispatch-opts      dispatch-opts
       :wrapped-handler-fn wrapped-handler-fn
       :stats-fn           stats-fn})))

;;;; Local API

(comment
  [level-aliases

   help:filters
   help:handlers
   help:handler-dispatch-options

   get-filters get-min-levels

   get-handlers
   get-handlers-stats

   without-filters
   set-kind-filter! with-kind-filter
   set-ns-filter!   with-ns-filter
   set-id-filter!   with-id-filter
   set-min-level!   with-min-level

   with-handler with-handler+
   add-handler! remove-handler! stop-handlers!

   *ctx*        set-ctx!        with-ctx
   *middleware* set-middleware! with-middleware])

;;;

#?(:clj
   (defn- api:help:filters []
     `(def  ~'help:filters
        "A signal will be provided to a handler iff ALL of the following are true:

    1. Signal creation is allowed by \"signal filters\":
      a. Compile-time: sample rate, kind, ns, id, level, when form, rate limit
      b. Runtime:      sample rate, kind, ns, id, level, when form, rate limit

    2. Signal handling is allowed by \"handler filters\":
      a. Compile-time: not applicable
      b. Runtime:      sample rate, kind, ns, id, level, when fn, rate limit

    3. Signal  middleware (fn [signal]) => ?modified-signal does not return nil
    4. Handler middleware (fn [signal]) => ?modified-signal does not return nil

  Note that middleware provides a flexible way to filter signals by arbitrary
  signal data/content conditions (return nil to filter signal).

  Config:

    To set signal filters (1a, 1b):

      Use:
        `set-kind-filter!`, `with-kind-filter`
        `set-ns-filter!`,   `with-ns-filter`
        `set-id-filter!`,   `with-id-filter`
        `set-min-level!`,   `with-min-level`
        or see `help:environmental-config`.

    To set handler filters (2b) or handler middleware (4):

      Provide relevant opts when calling `add-handler!` or `with-handler/+`.
      See `help:handler-dispatch-options` for details.

      Note: signal filters (1a, 1b) should generally be AT LEAST as permissive as
      handler filters (2b), otherwise signals will be filtered before even
      reaching handlers.

    To set signal middleware (3): use `set-middleware!`, `with-middleware`

  Compile-time vs runtime filters:

    Compile-time filters are an advanced feature that can be tricky to set
    and use correctly. Most folks will want ONLY runtime filters.

    Compile-time filters works by eliding (completely removing the code for)
    disallowed signals. This means zero performance cost for these signals,
    but also means that compile-time filters are PERMANENT once applied.

    So if you set `:info` as the compile-time minimum level, that'll REMOVE
    CODE for every signal below `:info` level. To decrease that minimum level,
    you'll need to rebuild.

    Compile-time filters can be set ONLY with environmental config
    (see `help:environmental-config` for details).

  Signal and handler sampling is multiplicative:

    Both signals and handlers can have independent sample rates, and these
    MULTIPLY! If a signal is created with 20% sampling and a handler
    handles 50% of received signals, then 10% of possible signals will be
    handled (50% of 20%).

    The final (multiplicative) rate is helpfully reflected in each signal's
    `:sample-rate` value.

  For more info:

    - On signal  filters, see: `help:filters`
    - On handler filters, see: `help:handler-dispatch-options`

  If anything is unclear, please ping me (@ptaoussanis) so that I can
  improve these docs!"
        "See docstring")))

(comment (api:help:filters))

#?(:clj
   (defn- api:help:handlers []
     `(def  ~'help:handlers
        "Signal handlers process created signals to do something with them (analyse them,
        write them to console/file/queue/db, etc.).

  Manage handlers with:

    `get-handlers`       - Returns info  on  registered handlers (dispatch options, etc.)
    `get-handlers-stats` - Returns stats for registered handlers (handling times,   etc.)

    `add-handler!`       - Registers   given handler
    `remove-handler!`    - Unregisters given handler

    `with-handler`       - Executes form with ONLY the given handler        registered
    `with-handler+`      - Executes form with      the given handler (also) registered

    `stop-handlers!`     - Stops registered handlers
      NB you should always call `stop-handlers!` somewhere appropriate - usually
      near the end of your `-main` or shutdown procedure, AFTER all other code has
      completed that could create signals.

  See the relevant docstrings for details.
  See `help:handler-dispatch-options` for handler filters, etc.

  If anything is unclear, please ping me (@ptaoussanis) so that I can
  improve these docs!"
        "See docstring")))

#?(:clj
   (defn- api:help:handler-dispatch-options []
     `(def  ~'help:handler-dispatch-options
        "Dispatch options can be provided for each signal handler when calling
  `add-handler!` or `with-handler/+`. These options will be merged over the
  defaults specified by `default-handler-dispatch-opts`.

  All handlers support the same dispatch options, including:

    `:async` (Clj only) options include:

      `:buffer-size` (default 1024)
        Size of request buffer, and the max number of pending requests before
        configured back-pressure behaviour is triggered (see `:mode`).

      `:mode` (default `:blocking`)
        Back-pressure mode ∈ #{:blocking :dropping :sliding}.
        Controls what happens when a new request is made while request buffer is full:
          `:blocking` => Blocks caller until buffer space is available
          `:dropping` => Drops the newest request (noop)
          `:sliding`  => Drops the oldest request

      `:n-threads` (default 1)
        Number of threads to use for executing fns (servicing request buffer).
        NB execution order may be non-sequential when n > 1.

      `:drain-msecs` (default 6000 msecs)
        Maximum time (in milliseconds) to try allow pending execution requests to
        complete when stopping handler. nil => no maximum.

    `:priority` (default 100)
      Optional handler priority ∈ℤ.
      Handlers will be called in descending priority order (larger ints first).

    `:sample-rate` (default nil => no sampling)
      Optional sample rate ∈ℝ[0,1], or (fn dyamic-sample-rate []) => ℝ[0,1].
      When present, handle only this (random) proportion of args:
        1.0 => handle every arg (same as nil rate, default)
        0.0 => noop   every arg
        0.5 => handle random 50% of args

    `:kind-filter` - Kind      filter as in `set-kind-filter!` (when relevant)
    `:ns-filter`   - Namespace filter as in `set-ns-filter!`
    `:id-filter`   - Id        filter as in `set-id-filter!`   (when relevant)
    `:min-level`   - Minimum   level  as in `set-min-level!`

    `:when-fn` (default nil => always allow)
      Optional nullary (fn allow? []) that must return truthy for handler to be
      called. When present, called *after* sampling and other filters, but before
      rate limiting. Useful for filtering based on external state/context.

      See `:middleware` for an alternative that takes a signal argument.

    `:rate-limit` (default nil => no rate limit)
      Optional rate limit spec as provided to `taoensso.encore/rate-limiter`,
      {<limit-id> [<n-max-calls> <msecs-window>]}.

      Examples:
        {\"1/sec\"  [1   1000]} => Max 1  call  per 1000 msecs
        {\"1/sec\"  [1   1000]
         \"10/min\" [10 60000]} => Max 1  call  per 1000 msecs,
                                 and 10 calls per 60   secs

    `:middleware` (default nil => no middleware)
      Optional (fn [signal]) => ?modified-signal to apply before
      handling signal. When middleware returns nil, skips handler.

      Compose multiple middleware fns together with `comp-middleware`.

    `:error-fn` - (fn [{:keys [handler-id signal error]}]) to call on handler error.
    `:backp-fn` - (fn [{:keys [handler-id             ]}]) to call on handler back-pressure.

  If anything is unclear, please ping me (@ptaoussanis) so that I can
  improve these docs!"
        "See docstring")))

;;;

#?(:clj
   (defn- api:get-filters [*rt-sig-filter* ct-sig-filter]
     `(defn ~'get-filters
        "Returns current ?{:keys [compile-time runtime]} filter config."
        []
        (enc/assoc-some nil
          :compile-time (enc/force-ref  ~ct-sig-filter)
          :runtime      (enc/force-ref ~*rt-sig-filter*)))))

(comment (api:get-filters `my-ct-sig-filter `*my-rt-sig-filter*))

#?(:clj
   (defn- api:get-min-levels [sf-arity *rt-sig-filter* ct-sig-filter]
     (case (int sf-arity)
       (4)
       `(defn ~'get-min-levels
          "Returns current ?{:keys [compile-time runtime]} minimum signal levels."
          (~'[       ] (~'get-min-levels nil    (str *ns*)))
          (~'[kind   ] (~'get-min-levels ~'kind (str *ns*)))
          (~'[kind ns]
           (enc/assoc-some nil
             :runtime      (parse-min-level (get (enc/force-ref ~*rt-sig-filter*) :min-level) ~'kind ~'ns)
             :compile-time (parse-min-level (get (enc/force-ref  ~ct-sig-filter)  :min-level) ~'kind ~'ns))))

       (2 3)
       `(defn ~'get-min-levels
          "Returns current ?{:keys [compile-time runtime]} minimum signal levels."
          (~'[  ] (~'get-min-levels nil (str *ns*)))
          (~'[ns]
           (enc/assoc-some nil
             :runtime      (parse-min-level (get (enc/force-ref ~*rt-sig-filter*) :min-level) nil ~'ns)
             :compile-time (parse-min-level (get (enc/force-ref  ~ct-sig-filter)  :min-level) nil ~'ns)))))))

(comment (api:get-min-levels 4 `*my-rt-sig-filter* `my-ct-sig-filter))

;;;

#?(:clj
   (defn- api:get-handlers [*sig-handlers*]
     `(defn ~'get-handlers
        "Returns ?{<handler-id> {:keys [dispatch-opts handler-fn handler-stats_]}}
  for all registered signal handlers."
        [] (get-handlers-map ~*sig-handlers*))))

(comment (api:get-handlers `*my-sig-handlers*))

#?(:clj
   (defn- api:get-handlers-stats [*sig-handlers*]
     `(defn ~'get-handlers-stats
        "Alpha, subject to change.
  Returns ?{<handler-id> {:keys [handling-nsecs counts]}} for all registered
  signal handlers that have the `:track-stats?` dispatch option enabled
  (it is by default).

  Stats include:

    `:handling-nsecs` - Summary stats of nanosecond handling times, keys:
      `:min`  - Minimum handling time
      `:max`  - Maximum handling time
      `:mean` - Arithmetic mean handling time
      `:mad`  - Mean absolute deviation of handling time (measure of dispersion)
      `:var`  - Variance                of handling time (measure of dispersion)
      `:p50`  - 50th percentile of handling time (50% of times <= this)
      `:p90`  - 90th percentile of handling time (90% of times <= this)
      `:p99`  - 99th percentile of handling time
      `:last` - Most recent        handling time
      ...

    `:counts` - Integer counts for handler outcomes, keys (chronologically):

      `:dropped`       - Noop handler calls due to stopped handler
      `:back-pressure` - Handler calls that experienced (async) back-pressure
                         (possible noop, depending on back-pressure mode)

       `:sampled`      - Noop  handler calls due to sample rate
       `:filtered`     - Noop  handler calls due to kind/ns/id/level/when filtering
       `:rate-limited` - Noop  handler calls due to rate limit
       `:disallowed`   - Noop  handler calls due to sampling/filtering/rate-limiting
       `:allowed`      - Other handler calls    (no sampling/filtering/rate-limiting)

       `:suppressed`   - Noop handler calls due to nil middleware result
       `:handled`      - Handler calls that completed successfully
       `:errors`       - Handler calls that threw an error

       Note that for performance reasons returned counts are not mutually atomic,
       e.g. `:sampled` count may be incremented before `:disallowed` count is.

  Useful for understanding/debugging how your handlers behave in practice,
  especially when they're under stress (high-volumes, etc.).

  Handler stats are tracked from the time each handler is last registered
  (e.g. with an `add-handler!` call)."
        [] (get-handlers-stats ~*sig-handlers*))))

(comment (api:get-handlers-stats `*my-sig-handlers*))

;;;

#?(:clj
   (defn-     api:without-filters [*rt-sig-filter*]
     `(defmacro ~'without-filters
        "Executes form without any runtime signal filters."
        ~'[form] `(binding [~'~*rt-sig-filter* nil] ~~'form))))

(comment (api:without-filters `*my-rt-sig-filter*))

#?(:clj
   (defn-     api:with-kind-filter [*rt-sig-filter*]
     `(defmacro ~'with-kind-filter
        "Executes form with given signal kind filter in effect.
  See `set-kind-filter!` for details."
        ~'[kind-filter form]
        `(binding [~'~*rt-sig-filter* (update-sig-filter ~'~*rt-sig-filter* {:kind-filter ~~'kind-filter})]
           ~~'form))))

(comment (api:with-kind-filter `*my-rt-sig-filter*))

#?(:clj
   (defn- api:set-kind-filter! [*rt-sig-filter*]
     `(defn ~'set-kind-filter!
        "Sets signal kind filter based on given `kind-filter` spec.
  `kind-filter` may be:

    - A regex pattern of kind/s to allow
    - A str/kw/sym to allow, with \"*\" and \"(.*)\" as wildcards:
      \"foo.*\"   will allow \"foo.bar\"
      \"foo(.*)\" will allow \"foo.bar\" and \"foo\"

    - A set/vector of above (allow on any match)
    - A map, {:allow <spec> :disallow <spec>} with specs as above:
      If present, `:allow`    spec MUST     match, AND
      If present, `:disallow` spec MUST NOT match."
        ~'[kind-filter]
        (enc/force-ref
          (enc/update-var-root! ~*rt-sig-filter*
            (fn [old#] (update-sig-filter old# {:kind-filter ~'kind-filter})))))))

(comment (api:set-ns-filter! `*my-rt-sig-filter*))

#?(:clj
   (defn-     api:with-ns-filter [*rt-sig-filter*]
     `(defmacro ~'with-ns-filter
        "Executes form with given signal namespace filter in effect.
  See `set-ns-filter!` for details."
        ~'[ns-filter form]
        `(binding [~'~*rt-sig-filter* (update-sig-filter ~'~*rt-sig-filter* {:ns-filter ~~'ns-filter})]
           ~~'form))))

(comment (api:with-ns-filter `*my-rt-sig-filter*))

#?(:clj
   (defn- api:set-ns-filter! [*rt-sig-filter*]
     `(defn ~'set-ns-filter!
        "Sets signal namespace filter based on given `ns-filter` spec.
  `ns-filter` may be:

    - A namespace.
    - A regex pattern of namespaces/s to allow
    - A str/kw/sym to allow, with \"*\" and \"(.*)\" as wildcards:
      \"foo.*\"   will allow \"foo.bar\"
      \"foo(.*)\" will allow \"foo.bar\" and \"foo\"

    - A set/vector of above (allow on any match)
    - A map, {:allow <spec> :disallow <spec>} with specs as above:
      If present, `:allow`    spec MUST     match, AND
      If present, `:disallow` spec MUST NOT match."
        ~'[ns-filter]
        (enc/force-ref
          (enc/update-var-root! ~*rt-sig-filter*
            (fn [old#] (update-sig-filter old# {:ns-filter ~'ns-filter})))))))

(comment (api:set-ns-filter! `*my-rt-sig-filter*))

#?(:clj
   (defn-     api:with-id-filter [*rt-sig-filter*]
     `(defmacro ~'with-id-filter
        "Executes form with given signal id filter in effect.
  See `set-id-filter!` for details."
        ~'[id-filter form]
        `(binding [~'~*rt-sig-filter* (update-sig-filter ~'~*rt-sig-filter* {:id-filter ~~'id-filter})]
           ~~'form))))

(comment (api:with-id-filter `*my-rt-sig-filter*))

#?(:clj
   (defn- api:set-id-filter! [*rt-sig-filter*]
     `(defn ~'set-id-filter!
        "Sets signal id filter based on given `id-filter` spec.
  `id-filter` may be:

    - A regex pattern of id/s to allow
    - A str/kw/sym to allow, with \"*\" and \"(.*)\" as wildcards:
      \"foo.*\"   will allow \"foo.bar\"
      \"foo(.*)\" will allow \"foo.bar\" and \"foo\"

    - A set/vector of above (allow on any match)
    - A map, {:allow <spec> :disallow <spec>} with specs as above:
      If present, `:allow`    spec MUST     match, AND
      If present, `:disallow` spec MUST NOT match."
        ~'[id-filter]
        (enc/force-ref
          (enc/update-var-root! ~*rt-sig-filter*
            (fn [old#] (update-sig-filter old# {:id-filter ~'id-filter})))))))

(comment (api:set-id-filter! `*my-rt-sig-filter*))

#?(:clj
   (defn- api:with-min-level [sf-arity *rt-sig-filter*]
     (let [self (symbol (str *ns*) "with-min-level")]
       (case (int sf-arity)
         (4)
         `(defmacro ~'with-min-level
            "Executes form with given minimum signal level in effect.
  See `set-min-level!` for details."
            (~'[               min-level form] (list '~self nil    nil ~'min-level ~'form))
            (~'[kind           min-level form] (list '~self ~'kind nil ~'min-level ~'form))
            (~'[kind ns-filter min-level form]
             `(binding [~'~*rt-sig-filter*
                        (update-sig-filter ~'~*rt-sig-filter*
                          {:min-level-fn
                           (fn [~'old-ml#]
                             (update-min-level ~'old-ml# ~~'kind ~~'ns-filter ~~'min-level))})]
                ~~'form)))

         (2 3)
         `(defmacro ~'with-min-level
            "Executes form with given minimum signal level in effect.
  See `set-min-level!` for details."
            (~'[          min-level form] (list '~self nil ~'min-level ~'form))
            (~'[ns-filter min-level form]
             `(binding [~'~*rt-sig-filter*
                        (update-sig-filter ~'~*rt-sig-filter*
                          {:min-level-fn
                           (fn [~'old-ml#]
                             (update-min-level ~'old-ml# nil ~~'ns-filter ~~'min-level))})]
                ~~'form)))))))

(comment (api:with-min-level 4 `*my-rt-sig-filter*))

#?(:clj
   (defn- api:set-min-level! [sf-arity *rt-sig-filter*]
     (case (int sf-arity)
       (4)
       `(defn ~'set-min-level!
          "Sets minimum signal level based on given `min-level` spec.
  `min-level` may be:

    - nil (=> no minimum level).
    - A level keyword (see `level-aliases` var for details).
    - An integer.

  If `ns-filter` is provided, then the given minimum level
  will apply only for the namespace/s that match `ns-filter`.
  See `set-ns-filter!` for details.

  If non-nil `kind` is provided, then the given minimum level
  will apply only for that signal kind.

  Examples:
    (set-min-level! nil)   ; Disable        minimum level
    (set-min-level! :info) ; Set `:info` as minimum level
    (set-min-level! 100)   ; Set 100     as minimum level

    ;; Set `:debug` as minimum level for current namespace
    ;; (nil `kind` => all kinds)
    (set-min-level! nil *ns* :debug)"
          (~'[               min-level] (~'set-min-level! nil    nil ~'min-level))
          (~'[kind           min-level] (~'set-min-level! ~'kind nil ~'min-level))
          (~'[kind ns-filter min-level]
           (enc/force-ref
             (enc/update-var-root! ~*rt-sig-filter*
               (fn [old-sf#]
                 (update-sig-filter old-sf#
                   {:min-level-fn
                    (fn [old-ml#]
                      (update-min-level old-ml# ~'kind ~'ns-filter ~'min-level))}))))))

       (2 3)
       `(defn ~'set-min-level!
          "Sets minimum signal level based on given `min-level` spec.
`min-level` may be:

    - nil (=> no minimum level).
    - A level keyword (see `level-aliases` var for details).
    - An integer.

  If `ns-filter` is provided, then the given minimum level
  will apply only for the namespace/s that match `ns-filter`.
  See `set-ns-filter!` for details.

  Examples:
    (set-min-level! nil)   ; Disable        minimum level
    (set-min-level! :info) ; Set `:info` as minimum level
    (set-min-level! 100)   ; Set 100     as minimum level

    ;; Set `:debug` as minimum level for current namespace
    (set-min-level! *ns* :debug)"
          (~'[          min-level] (~'set-min-level! nil ~'min-level))
          (~'[ns-filter min-level]
           (enc/force-ref
             (enc/update-var-root! ~*rt-sig-filter*
               (fn [old-sf#]
                 (update-sig-filter old-sf#
                   {:min-level-fn
                    (fn [old-ml#]
                      (update-min-level old-ml# nil ~'ns-filter ~'min-level))})))))))))

(comment (api:set-min-level! 4 `*my-rt-sig-filter*))

;;;

#?(:clj
   (defn- api:with-handler [*sig-handlers* lib-dispatch-opts]
     (let [self (symbol (str *ns*) "with-handler")]
       `(defmacro ~'with-handler
          "Executes form with ONLY the given signal handler registered.
  Stops handler after use. Useful for tests/debugging.

  See `help:handler-dispatch-options` for handler filters, etc.
  See also `with-handler+`."
          (~'[handler-id handler-fn               form] (list '~self ~'handler-id ~'handler-fn nil ~'form))
          (~'[handler-id handler-fn dispatch-opts form]
           `(let [~'wrapped-handler-fn# (wrap-handler ~~'handler-id ~~'handler-fn ~~lib-dispatch-opts ~~'dispatch-opts)]
              (enc/try*
                (binding [~'~*sig-handlers* (add-handler {} ~~'handler-id ~'wrapped-handler-fn#)] ~~'form)
                (finally (~'wrapped-handler-fn#)))))))))

(comment (api:with-handler `*my-sig-handlers* {:my-opt :foo}))

#?(:clj
   (defn- api:with-handler+ [*sig-handlers* lib-dispatch-opts]
     (let [self (symbol (str *ns*) "with-handler+")]
       `(defmacro ~'with-handler+
          "Executes form with the given signal handler (also) registered.
  Stops handler after use. Useful for tests/debugging.

  See `help:handler-dispatch-options` for handler filters, etc.
  See also `with-handler`."
          (~'[handler-id handler-fn               form] (list '~self ~'handler-id ~'handler-fn nil ~'form))
          (~'[handler-id handler-fn dispatch-opts form]
           `(let [~'wrapped-handler-fn# (wrap-handler ~~'handler-id ~~'handler-fn ~~lib-dispatch-opts ~~'dispatch-opts)]
              (enc/try*
                (binding [~'~*sig-handlers* (add-handler ~'~*sig-handlers* ~~'handler-id ~'wrapped-handler-fn#)] ~~'form)
                (finally (~'wrapped-handler-fn#)))))))))

(comment (api:with-handler+ `*my-sig-handlers* {:my-opt :foo}))

#?(:clj
   (defn- api:add-handler! [*sig-handlers* lib-dispatch-opts]
     `(defn ~'add-handler!
        "Registers given signal handler and returns
  {<handler-id> {:keys [dispatch-opts handler-fn]}} for all handlers
  now registered. If an old handler already existed under the same id, stop it.

  `handler-fn` should be a fn of exactly 2 arities:

    [signal] ; Single argument
      Called asynchronously or synchronously (depending on dispatch options)
      to do something useful with the given signal.

      Example actions:
        Save data to disk or db, `tap>`, log, `put!` to an appropriate
        `core.async` channel, filter, aggregate, use for a realtime analytics
        dashboard, examine for outliers or unexpected data, etc.

    [] ; No arguments
      Called exactly once when stopping handler to provide an opportunity
      for handler to flush buffers, close files, etc. May just noop.

  NB you should always call `stop-handlers!` somewhere appropriate - usually
  near the end of your `-main` or shutdown procedure, AFTER all other code has
  completed that could create signals.

  See `help:handler-dispatch-options` for handler filters, etc."
        (~'[handler-id handler-fn              ] (~'add-handler! ~'handler-id ~'handler-fn nil))
        (~'[handler-id handler-fn dispatch-opts]
         {:arglists
          '([handler-id handler-fn]
            [handler-id handler-fn
             {:as   dispatch-opts
              :keys [async priority sample-rate rate-limit when-fn middleware,
                     kind-filter ns-filter id-filter min-level,
                     error-fn backp-fn]}])}

         (let [removed-handler# (get-wrapped-handler-fn ~*sig-handlers* ~'handler-id)
               new-handlers-vec#
               (enc/update-var-root! ~*sig-handlers*
                 (fn [m#]
                   (add-handler m# ~'handler-id ~'handler-fn,
                     ~lib-dispatch-opts ~'dispatch-opts)))]

           (when removed-handler# (removed-handler#))
           (get-handlers-map new-handlers-vec#))))))

(comment (api:add-handler! `*my-sig-handlers* 'lib-dispatch-opts))

#?(:clj
   (defn- api:remove-handler! [*sig-handlers*]
     `(defn ~'remove-handler!
        "Stops and deregisters signal handler with given id, and returns
  ?{<handler-id> {:keys [dispatch-opts handler-fn]}} for all handlers
  still registered."
        ~'[handler-id]
        (let [removed-handler#  (get-wrapped-handler-fn ~*sig-handlers* ~'handler-id)
              new-handlers-vec# (enc/update-var-root!   ~*sig-handlers*
                                  (fn [m#] (remove-handler m# ~'handler-id)))]

          (when removed-handler# (removed-handler#))
          (get-handlers-map new-handlers-vec#)))))

(comment (api:remove-handler! `*my-sig-handlers*))

#?(:clj
   (defn- api:stop-handlers! [*sig-handlers*]
     `(defn ~'stop-handlers!
        "Stops registered signal handlers in parallel by calling each
  handler-fn with no arguments. This gives each handler the opportunity
  to flush buffers, close files, etc.

  Each handler will immediately stop accepting new signals, nooping if called.

  Blocks to return ?{<handler-id> {:keys [okay error]}}, honouring each
  handler's `:drain-msecs` value (see `help:handler-dispatch-options`).

  NB you should always call `stop-handlers!` somewhere appropriate - usually
  near the end of your `-main` or shutdown procedure, AFTER all other code has
  completed that could create signals."
        [] (stop-handlers! ~*sig-handlers*))))

(comment (api:stop-handlers! `*my-sig-handlers*))

;;;;

#?(:clj
   (defn- api:*ctx* []
     `(enc/def* ~'*ctx* {:dynamic true}
        "Optional context (state) attached to all signals.
  Value may be any type, but is usually nil or a map. Default (root) value is nil.

  Useful for dynamically attaching arbitrary app-level state to signals.

  Re/bind dynamic        value using `with-ctx`, `with-ctx+`, or `binding`.
  Modify  root (default) value using `set-ctx!`.

  As with all dynamic Clojure vars, \"binding conveyance\" applies when using
  futures, agents, etc.

  Tips:
    - Value may be (or may contain) an atom if you want mutable semantics.
    - Value may be of form {<scope-id> <data>} for custom scoping, etc.
    - Use `get-env` to set default (root) value based on environmental config."
        nil)))

#?(:clj
   (defn- api:*middleware* []
     `(enc/def* ~'*middleware* {:dynamic true}
        "Optional (fn [signal]) => ?modified-signal to apply to all signals.
  When middleware returns nil, skips all handlers. Default (root) value is nil.

  Useful for dynamically transforming signals and/or filtering signals
  by signal data/content/etc.

  Re/bind dynamic        value using `with-middleware`, `binding`.
  Modify  root (default) value using `set-middleware!`.

  As with all dynamic Clojure vars, \"binding conveyance\" applies when using
  futures, agents, etc.

  Examples:

    ;; Filter signals by returning nil:
    (t/set-middleware! (fn [signal] (when-not (:skip-me? signal) signal)))

    ;; Remove key/s from signals:
    (t/set-middleware! (fn [signal] (dissoc signal :unwanted-key1 ...)))

    ;; Remove key/s from signals to specific handler:
    (t/add-handler! ::my-handler my-handler
      {:middleware (fn [signal] (dissoc signal :unwanted-key1 ...))})

  Tips:
    - Compose multiple middleware fns together with `comp-middleware`.
    - Use `get-env` to set default (root) value based on environmental config."
        nil)))

(defn update-ctx
  "Returns `new-ctx` given `old-ctx` and an update map or fn."
  [old-ctx update-map-or-fn]
  (enc/cond
    (nil? update-map-or-fn)              old-ctx
    (map? update-map-or-fn) (enc/merge   old-ctx update-map-or-fn) ; Before ifn
    (ifn? update-map-or-fn) (update-map-or-fn old-ctx)
    :else
    (enc/unexpected-arg! update-map-or-fn
      {:context  `update-ctx
       :param    'update-map-or-fn
       :expected '#{nil map fn}})))

#?(:clj
   (defn- api:set-ctx! []
     `(defn ~'set-ctx!
        "Set `*ctx*` var's default (root) value. See `*ctx*` for details."
        ~'[root-ctx-val] (enc/set-var-root! ~'*ctx* ~'root-ctx-val))))

#?(:clj
   (defn- api:set-middleware! []
     `(defn ~'set-middleware!
        "Set `*middleware*` var's default (root) value. See `*middleware*` for details."
        ~'[?root-middleware-fn]
        (enc/set-var-root! ~'*middleware* ~'?root-middleware-fn))))

#?(:clj
   (defn- api:with-ctx []
     (let [*ctx* (symbol (str *ns*) "*ctx*")]
       `(defmacro ~'with-ctx
          "Evaluates given form with given `*ctx*` value. See `*ctx*` for details."
          ~'[ctx-val form] `(binding [~'~*ctx* ~~'ctx-val] ~~'form)))))

#?(:clj
   (defn- api:with-middleware []
     (let [*middleware* (symbol (str *ns*) "*middleware*")]
       `(defmacro ~'with-middleware
          "Evaluates given form with given `*middleware*` value.
  See `*middleware*` for details."
          ~'[?middleware-fn form]
          `(binding [~'~*middleware* ~~'?middleware-fn] ~~'form)))))

#?(:clj
   (defn- api:with-ctx+ []
     (let [*ctx* (symbol (str *ns*) "*ctx*")]
       `(defmacro ~'with-ctx+
          "Evaluates given form with updated `*ctx*` value.

  `update-map-or-fn` may be:
    - A map to merge with    current `*ctx*` value, or
    - A unary fn to apply to current `*ctx*` value

  See `*ctx*` for details."
          ~'[update-map-or-fn form]
          `(binding [~'~*ctx* (update-ctx ~'~*ctx* ~~'update-map-or-fn)] ~~'form)))))

;;;;

#?(:clj
   (defmacro def-api
     "Defines signal API vars in current ns.
  NB Cljs ns will need appropriate `:require-macros`."
     [{:as opts
       :keys
       [sf-arity lib-dispatch-opts
        ct-sig-filter *rt-sig-filter* *sig-handlers*]}]

     (enc/have? [:ks>= #{:sf-arity :ct-sig-filter :*rt-sig-filter* :*sig-handlers*}] opts)
     (enc/have? [:or nil? symbol?]  ct-sig-filter  *rt-sig-filter*  *sig-handlers*)

     (when-not (contains? #{2 3 4} sf-arity)
       (unexpected-sf-artity! sf-arity `def-api))

     (let [clj?            (not (:ns &env))
           sf-arity        (int sf-arity)
           ct-sig-filter   (enc/resolve-sym &env  ct-sig-filter)
           *rt-sig-filter* (enc/resolve-sym &env *rt-sig-filter*)
           *sig-handlers*  (enc/resolve-sym &env *sig-handlers*)]

       `(do
          (enc/defalias level-aliases)

          ~(api:help:filters)
          ~(api:help:handlers)
          ~(api:help:handler-dispatch-options)

          ~(api:get-filters             *rt-sig-filter* ct-sig-filter)
          ~(api:get-min-levels sf-arity *rt-sig-filter* ct-sig-filter)

          ~(api:get-handlers       *sig-handlers*)
          ~(api:get-handlers-stats *sig-handlers*)

          ~(when clj? (api:without-filters *rt-sig-filter*))

          ~(when (and (>= sf-arity 4) clj?) (api:with-kind-filter *rt-sig-filter*))
          ~(when      (>= sf-arity 4)       (api:set-kind-filter! *rt-sig-filter*))

          ~(when clj? (api:with-ns-filter *rt-sig-filter*))
          ~(do        (api:set-ns-filter! *rt-sig-filter*))

          ~(when (and (>= sf-arity 3) clj?) (api:with-id-filter *rt-sig-filter*))
          ~(when      (>= sf-arity 3)       (api:set-id-filter! *rt-sig-filter*))

          ~(when clj? (api:with-min-level sf-arity *rt-sig-filter*))
          ~(do        (api:set-min-level! sf-arity *rt-sig-filter*))

          ~(when clj? (api:with-handler  *sig-handlers* lib-dispatch-opts))
          ~(when clj? (api:with-handler+ *sig-handlers* lib-dispatch-opts))

          ~(api:add-handler!    *sig-handlers* lib-dispatch-opts)
          ~(api:remove-handler! *sig-handlers*)
          ~(api:stop-handlers!  *sig-handlers*)

          ~(api:*ctx*)
          ~(api:*middleware*)

          ~(api:set-ctx!)
          ~(api:set-middleware!)

          ~(api:with-ctx)
          ~(api:with-ctx+)
          ~(api:with-middleware)))))

(comment
  ;; See `taoensso.encore-tests.signal-api` ns
  (macroexpand
    '(def-api
       {:sf-arity 4
        :ct-sig-filter    my-ct-sig-filter
        :*rt-sig-filter* *my-rt-sig-filter*
        :*sig-handlers*  *my-sig-handlers*})))
