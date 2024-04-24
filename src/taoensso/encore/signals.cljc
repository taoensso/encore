(ns ^:no-doc taoensso.encore.signals
  "Experimental, subject to change without notice!
  Private low-level signal toolkit for use by Telemere, Tufte, Timbre, etc.

  \"Signal\" is used here as an internal name for any
  abstract event/object/data that:
    - May have a kind (type/taxonomy/etc.)
    - Originates in an ns (generated or received there, etc.)
    - May have an identifier
    - Has a level (priority/significance/etc.)"

  {:added "Encore v3.68.0 (2023-09-25)"}

  (:refer-clojure :exclude [binding])
  (:require
   [clojure.string  :as str]
   [taoensso.encore :as enc :refer [binding have have?]])

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
        {:level    {:value x, :type (type x)}
         :expected expected}))))

(defn get-level-int
  "Returns valid integer level, or nil."
  [x]
  (enc/cond
    (keyword? x) (get level-aliases x)
    (integer? x) (long              x)))

(comment (get-level-int :bad))

#?(:clj
   (do
     (defmacro valid-level-int
       "Returns valid integer level, or throws."
       [x]
       (if (enc/const-form? x)
         (do           (or (get-level-int x)  (bad-level! x)))
         `(let [x# ~x] (or (get-level-int x#) (bad-level! x#)))))

     (defmacro valid-level
       "Returns valid level, or throws."
       [x]
       (if (enc/const-form? x)
         (do           (if (get-level-int x)  x  (bad-level! x)))
         `(let [x# ~x] (if (get-level-int x#) x# (bad-level! x#)))))

     (defmacro const-level>=
       "Returns true, false, or nil (inconclusive)."
       [x y]
       (when (and (enc/const-form? x) (enc/const-form? y))
         (>= (long (valid-level-int x)) (long (valid-level-int y)))))

     (defmacro level>=
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
      (fn [ml-spec nf-arg]
        (if (vector? ml-spec)
          ;; [[<nf-spec> <min-level>] ... [\"*\" <min-level>]]
          (enc/rsome
            (fn [[nf-spec min-level]]
              (when (nf-conform? nf-spec nf-arg)
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
          {:name-filter {:value x, :type (type x)}}
          t))
      x))

  (defn allow-name?
    "Low-level name filter."
    #?(:cljs {:tag 'boolean})
    [nf-spec nf-arg]
    (if ^boolean (nf-conform? nf-spec nf-arg) true false))

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

;;;; SigFilter

(comment (enc/defonce ^:dynamic *rt-sig-filter* "`SigFilter`, or nil." nil))

(deftype SigFilter [kind-filter ns-filter id-filter min-level filter-fn]
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    {:kind-filter kind-filter :ns-filter ns-filter 
     :id-filter     id-filter :min-level min-level})

  #?(:clj clojure.lang.IFn :cljs IFn)
  (#?(:clj invoke :cljs -invoke) [_ kind ns id level] (filter-fn kind ns id level))
  (#?(:clj invoke :cljs -invoke) [_      ns id level] (filter-fn      ns id level))
  (#?(:clj invoke :cljs -invoke) [_      ns    level] (filter-fn      ns    level)))

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

                    ;; Used for compile-time filtering (not perf sensitive, ignore nils)
                    ([{:keys [kind ns id level]}]
                     (and
                       (if (and kind-filter kind)  (allow-name? kind-filter kind)         true)
                       (if (and   ns-filter ns)    (allow-name?   ns-filter   ns)         true)
                       (if (and   id-filter id)    (allow-name? kind-filter   id)         true)
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

(comment ; [62.11 61.44 65.25]
  (let [sf (sig-filter nil "*" nil nil)]
    (enc/qb 1e6
      (sf :kind :ns     :info)
      (sf       :ns :id :info)
      (sf       :ns :id :info))))

(deftype HandlerContext [sample-rate]) ; Using an object for future extensibility (extra fields, etc.)

(defprotocol IFilterableSignal
  "Protocol that app/library signal-like types must implement to support signal API."
  (allow-signal? [_ sig-filter]      "Returns true iff given signal is allowed by given `SigFilter`.")
  (signal-value  [_ handler-context] "Returns signal's user-facing value as given to handlers, etc."))

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

;;;; Expansion filtering

#?(:clj (enc/defonce expansion-counter (enc/counter)))

(let [rate-limiters_ (enc/latom {})]
  (defn expansion-limit!?
    "Calls the identified stateful rate-limiter and returns true iff limited."
    #?(:cljs {:tag 'boolean})
    [rl-id spec]
    (let [rl
          (or
            (get (rate-limiters_) rl-id) ; Common case
            (rate-limiters_       rl-id #(or % (enc/rate-limiter {:basic? true} spec))))]
      (if (rl) true false))))

(comment (enc/qb 1e6 (expansion-limit!? :limiter-id1 [[1 4000]]))) ; 54.02

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

     [{:keys [macro-form macro-env, sf-arity ct-sig-filter *rt-sig-filter*] :as core-opts} call-opts]

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

           kind-form  (get opts     :kind)
           ns-form    (get location :ns)
           id-form    (get opts     :id)
           level-form (get opts     :level)
           _
           (when (enc/const-form? level-form)
             (valid-level         level-form))

           elide?
           (and
             (get opts :elidable? true)
             (get opts :elide?
               (when-let [sf ct-sig-filter]
                 (not (sf {:kind  (enc/const-form  kind-form)
                           :ns    (enc/const-form    ns-form)
                           :id    (enc/const-form    id-form)
                           :level (enc/const-form level-form)})))))

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
                         2 `(if-let [~'sf ~*rt-sig-filter*] (~'sf            ~ns-form          ~level-form) true)
                         3 `(if-let [~'sf ~*rt-sig-filter*] (~'sf            ~ns-form ~id-form ~level-form) true)
                         4 `(if-let [~'sf ~*rt-sig-filter*] (~'sf ~kind-form ~ns-form ~id-form ~level-form) true)
                         (unexpected-sf-artity! sf-arity `expansion-filter))

                       when-form
                       (when-let [when-form (get opts :when)]
                         `(let [~'this-expansion-id ~expansion-id] ~when-form))

                       rl-form ; Nb last (increments count)
                       (when-let [spec-form (get opts :rate-limit)]
                         `(if (expansion-limit!? ~expansion-id ~spec-form) false true))]

                   `(and ~@(filter some? [sample-rate-form sf-form when-form rl-form]))))]

           (assoc base-rv :allow? allow?-form))))))

(comment
  (filterable-expansion
    {:sf-arity 2, :*rt-sig-filter* `*foo*}
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

(defn get-handlers
  "Returns non-empty ?{<handler-id> <handler-meta>}."
  [handlers]
  (when handlers
    (reduce
      (fn [m wrapped-handler-fn]
        (let [handler-meta (meta wrapped-handler-fn)]
          (assoc m
            (get    handler-meta :handler-id)
            (dissoc handler-meta :handler-id))))
      nil
      handlers)))

(defn call-handlers!
  "Calls given handlers with the given signal.
  Signal's type must implement `IFilterableSignal`."
  [handlers signal]
  (run! (fn [wrapped-handler-fn] (wrapped-handler-fn signal)) handlers))

(defn shut-down-handlers!
  "Shuts down given handlers in parallel by calling each one with no args.
  Returns {<handler-id> {:keys [okay error]}}."
  {:added "Encore v3.99.0 (2024-04-10)"}
  ([handlers]
   #?(:clj (shut-down-handlers! handlers 5000)
      :cljs
      (reduce ; ?{<handler-id> <result>}
        (fn [acc wrapped-handler-fn]
          (let [handler-meta (meta wrapped-handler-fn)
                handler-id   (get handler-meta :handler-id)]
            (assoc acc handler-id (wrapped-handler-fn))))
        nil handlers)))

  #?(:clj
     ([handlers timeout-msecs]
      (when-let [async-results
                 (reduce ; ?{<handler-id> [<result_> <thread>]}
                   (fn [acc wrapped-handler-fn]
                     (let [handler-meta (meta wrapped-handler-fn)
                           handler-id   (get handler-meta :handler-id)
                           result_      (volatile! {:error :timeout})
                           thread
                           (enc/threaded :daemon
                             (let [result (wrapped-handler-fn)]
                               ;; Block for handler's runner to shut down
                               (when-let [runner_  (get result :runner_)] (deref runner_))
                               (vreset! result_ (dissoc result :runner_))))]
                       (assoc acc handler-id [result_ thread])))
                   nil handlers)]

        (let [joining-thread
              (enc/threaded :daemon
                (enc/run-kv!
                  (fn [_handler-id [result_ thread]] (.join ^Thread thread))
                  async-results))]

          ;; Wait for all handlers to complete shutdown
          (if timeout-msecs
            (.join ^Thread joining-thread (int timeout-msecs))
            (.join ^Thread joining-thread))

          (reduce-kv
            (fn [acc handler-id [result_ thread]]
              (assoc acc handler-id @result_))
            async-results
            async-results))))))

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

(def ^:private default-handler-priority 100)
(def ^:private default-dispatch-opts
  #?(:cljs {:priority default-handler-priority}
     :clj  {:priority default-handler-priority,
            :async
            {:mode :dropping, :buffer-size 1024, :n-threads 1,
             :daemon-threads? true, :shutdown-drain-msecs 5000}}))

;;; Telemere will set these when it's present
(enc/defonce ^:dynamic *default-handler-error-fn* nil)
(enc/defonce ^:dynamic *default-handler-backp-fn* nil)

(defn wrap-handler
  "Wraps given handler-fn to add common handler-level functionality."
  [handler-id handler-fn, base-dispatch-opts dispatch-opts]
  (let [dispatch-opts (enc/nested-merge default-dispatch-opts base-dispatch-opts dispatch-opts)
        {:keys
         [#?(:clj async) priority sample-rate rate-limit when-fn middleware,
          kind-filter ns-filter id-filter min-level,
          rl-error rl-backp error-fn backp-fn]}
        dispatch-opts

        [sample-rate sample-rate-fn]
        (when      sample-rate
          (if (fn? sample-rate)
            [nil                sample-rate] ; Dynamic rate (use dynamic binding, deref atom, etc.)
            [(enc/as-pnum! sample-rate) nil] ; Static  rate
            ))

        rl-handler   (when-let [spec rate-limit] (enc/rate-limiter {:basic? true} spec))
        sig-filter*  (sig-filter kind-filter ns-filter id-filter min-level)
        stopped?_    (enc/latom false)

        middleware-fn (get-middleware-fn middleware) ; (fn [signal-value]) => transformed ?signal-value*

        rl-error (get dispatch-opts :rl-error (enc/rate-limiter-once-per (enc/ms :mins 1)))
        rl-backp (get dispatch-opts :rl-backp (enc/rate-limiter-once-per (enc/ms :mins 1)))
        error-fn (get dispatch-opts :error-fn ::default)
        backp-fn (get dispatch-opts :backp-fn ::default)

        signal-error-fn
        (when  error-fn
          (fn [signal error]
            (enc/catching
              (if (and rl-error (rl-error)) ; error-fn rate-limited
                nil ; no-op
                (when-let [error-fn
                           (if (enc/identical-kw? error-fn ::default)
                             *default-handler-error-fn*
                             error-fn)]
                  (error-fn {:handler-id handler-id, :signal signal, :error error}))))))

        wrapped-handler-fn
        (fn wrapped-handler-fn
          ([] ; Shut down => {:keys [okay error]}
           (if-not (enc/-cas!? stopped?_ false true)
             {:okay :previously-shut-down}
             (enc/try*
               (handler-fn) ; Notify handler-fn to shut down
               {:okay :shut-down}
               (catch :all t
                 (when (and error-fn (not (enc/identical-kw? error-fn ::default)))
                   (enc/catching (error-fn {:handler-id handler-id, :error t}))) ; No :signal key
                 {:error t}))))

          ([sig-raw]
           (if (stopped?_)
             nil
             (or
               (enc/try* ; Non-specific (global) trap
                 (let [sample-rate (or sample-rate (when-let [f sample-rate-fn] (f)))
                       allow?
                       (and
                         (if sample-rate (< (Math/random) (double sample-rate))  true)
                         (if sig-filter* (allow-signal? sig-raw sig-filter*)     true)
                         (if when-fn     (when-fn #_sig-raw)                     true)
                         (if rl-handler  (if (rl-handler) false true)            true) ; Nb last (increments count)
                         )]

                   (when allow?
                     (when-let [sig-val ; Raw signal -> library-level handler-arg
                                (signal-value sig-raw
                                  (when              sample-rate
                                    (HandlerContext. sample-rate)))]

                       (enc/try*
                         (when-let [sig-val* ; Library-level handler-arg -> arb user-level value
                                    (if middleware-fn
                                      (middleware-fn sig-val)
                                      (do            sig-val))]

                           (enc/try*
                             (handler-fn sig-val*) true
                             (catch :all t (when signal-error-fn (signal-error-fn sig-val* t)))))
                         (catch     :all t (when signal-error-fn (signal-error-fn sig-val  t)))))))
                 (catch             :all t (when signal-error-fn (signal-error-fn sig-raw  t))))
               false))))

        wrapped-handler-fn*
        #?(:cljs wrapped-handler-fn
           :clj
           (if-not async
             wrapped-handler-fn
             (let [runner (enc/runner (have map? async))]
               (fn wrapped-handler-fn*
                 ([       ] (enc/assoc-some (wrapped-handler-fn) :runner_ (runner))) ; Shut down
                 ([sig-raw]
                  (when-let [back-pressure? (false? (runner (fn [] (wrapped-handler-fn sig-raw))))]
                    (when backp-fn
                      (enc/catching
                        (if (and rl-backp (rl-backp)) ; backp-fn rate-limited
                          nil ; no-op
                          (let [backp-fn
                                (if (enc/identical-kw? backp-fn ::default)
                                  *default-handler-backp-fn*
                                  backp-fn)]
                            (backp-fn {:handler-id handler-id})))))))))))]

    (with-meta wrapped-handler-fn*
      {:handler-id    handler-id
       :handler-fn    handler-fn ; Unwrapped
       :dispatch-opts dispatch-opts})))

(defn remove-handler
  "Returns updated, non-empty handlers vec."
  [handlers handler-id]
  (not-empty
    (filterv #(not= (get (meta %) :handler-id) handler-id)
      handlers)))

(comment (remove-handler [(with-meta (fn []) {:handler-id :hid1})
                          (with-meta (fn []) {:handler-id :hid2})] :hid1))

(defn add-handler
  "Returns updated, non-empty handlers vec."

  ;; Given pre-wrapped handler-fn
  ([handlers handler-id pre-wrapped-handler-fn]
   (if-not pre-wrapped-handler-fn
     handlers
     (enc/sortv #(get-in (meta %) [:dispatch-opts :priority] default-handler-priority) enc/rcompare
       (conj (or (remove-handler handlers handler-id) []) pre-wrapped-handler-fn))))

  ;; Given unwrapped handler-fn
  ([handlers handler-id unwrapped-handler-fn, base-dispatch-opts dispatch-opts]
   (if-not unwrapped-handler-fn
     handlers
     (if (get dispatch-opts :no-wrap?) ; Undocumented
       (add-handler handlers handler-id unwrapped-handler-fn)
       (add-handler handlers handler-id
         (wrap-handler handler-id unwrapped-handler-fn, base-dispatch-opts dispatch-opts))))))

;;;; Local API

;; #?(:clj
;;    (defmacro api:debug [outer]
;;      (let [code `(defmacro ~'api-debug ~'[inner] `(vector ~~outer ~~'inner))]
;;        ;; (spit "debug.txt" (str code "\n") :append true)
;;        code)))

;; (comment
;;   (macroexpand '(api:debug "o1"))
;;   (macroexpand '(api-debug "i1"))
;;   (api:debug "o1")
;;   (api-debug "i1"))

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
   (defn- api:help:filters
     [purpose system-vals-info]
     `(def ~'help:filters
        ~(str
           (api-docstring 13 purpose
             "Your filter config determines which %s calls will be allowed.

             Filtering can occur at compile-time (=> elision), or runtime.
             Both compile-time and runtime config can be specified with:

               System values (JVM properties, environment variables, or
               classpath resources) [1].

             Runtime config can also be specified with:

               `set-kind-filter!`, `with-kind-filter` - for filtering calls by %s kind (when relevant)
               `set-ns-filter!`,   `with-ns-filter`   - for filtering calls by namespace
               `set-id-filter!`,   `with-id-filter`   - for filtering calls by %s id   (when relevant)
               `set-min-level!`,   `with-min-level!`  - for filtering calls by %s level

               See the relevant docstrings for details.

             Filtering can also be applied per handler, see `add-handler!` for details.

             See also:

               `get-filters`     - to see current filter config
               `get-min-level`   - to see current minimum level
               `without-filters` - to disable all runtime filtering

             If anything is unclear, please ping me (@ptaoussanis) so that I can
             improve these docs!

             [1] ")
           (or system-vals-info "See library docs for details."))
        "See docstring")))

(comment (api:help:filters "purpose" nil))

#?(:clj
   (defn- api:get-filters
     [purpose *rt-sig-filter* ct-sig-filter]
     `(defn ~'get-filters
        ~(api-docstring 0 purpose "Returns current ?{:keys [compile-time runtime]} filter config.")
        []
        (enc/assoc-some nil
          :compile-time (enc/force-ref  ~ct-sig-filter)
          :runtime      (enc/force-ref ~*rt-sig-filter*)))))

(comment (api:get-filters "purpose" `my-ct-sig-filter `*my-rt-sig-filter*))

#?(:clj
   (defn- api:without-filters
     [purpose *rt-sig-filter* clj?]
     (when clj?
       `(defmacro ~'without-filters
          ~(api-docstring 0 purpose "Executes form without any runtime filters.")
          ~'[form] `(binding [~'~*rt-sig-filter* nil] ~~'form)))))

(comment (api:without-filters "purpose" `*my-rt-sig-filter* :clj))

#?(:clj
   (defn- api:set-kind-filter!
     [purpose *rt-sig-filter*]
     `(defn ~'set-kind-filter!
        ~(api-docstring 11 purpose
           "Sets %s call kind filter based on given `kind-filter` spec.
           `kind-filter` may be:

             - A str/kw/sym, in which \"*\"s act as wildcards.
             - A regex pattern of kind/s to allow.
             - A vector or set of regex patterns or strs/kws/syms.
             - {:allow <spec> :deny <spec>} with specs as above.
               If present, `:allow` spec MUST     match, AND
               If present, `:deny`  spec MUST NOT match.")

        ~'[kind-filter]
        (enc/force-ref
          (enc/update-var-root! ~*rt-sig-filter*
            (fn [old#] (update-sig-filter old# {:kind-filter ~'kind-filter})))))))

(comment (api:set-ns-filter! "purpose" `*my-rt-sig-filter*))

#?(:clj
   (defn- api:with-kind-filter
     [purpose *rt-sig-filter* clj?]
     (when clj?
       `(defmacro ~'with-kind-filter
          ~(api-docstring 13 purpose
             "Executes form with given %s call kind filter in effect.
             See `set-kind-filter!` for details.")
          ~'[kind-filter form]
          `(binding [~'~*rt-sig-filter* (update-sig-filter ~'~*rt-sig-filter* {:kind-filter ~~'kind-filter})]
             ~~'form)))))

(comment (api:with-kind-filter "purpose" `*my-rt-sig-filter* :clj))

#?(:clj
   (defn- api:set-ns-filter!
     [purpose *rt-sig-filter*]
     `(defn ~'set-ns-filter!
        ~(api-docstring 11 purpose
           "Sets %s call namespace filter based on given `ns-filter` spec.
           `ns-filter` may be:

             - A namespace.
             - A str/kw/sym, in which \"*\"s act as wildcards.
             - A regex pattern of namespace/s to allow.
             - A vector or set of regex patterns or strs/kws/syms.
             - {:allow <spec> :deny <spec>} with specs as above.
               If present, `:allow` spec MUST     match, AND
               If present, `:deny`  spec MUST NOT match.")

        ~'[ns-filter]
        (enc/force-ref
          (enc/update-var-root! ~*rt-sig-filter*
            (fn [old#] (update-sig-filter old# {:ns-filter ~'ns-filter})))))))

(comment (api:set-ns-filter! "purpose" `*my-rt-sig-filter*))

#?(:clj
   (defn- api:with-ns-filter
     [purpose *rt-sig-filter* clj?]
     (when clj?
       `(defmacro ~'with-ns-filter
          ~(api-docstring 13 purpose
             "Executes form with given %s call namespace filter in effect.
             See `set-ns-filter!` for details.")
          ~'[ns-filter form]
          `(binding [~'~*rt-sig-filter* (update-sig-filter ~'~*rt-sig-filter* {:ns-filter ~~'ns-filter})]
             ~~'form)))))

(comment (api:with-ns-filter "purpose" `*my-rt-sig-filter* :clj))

#?(:clj
   (defn- api:with-id-filter
     [purpose *rt-sig-filter* clj?]
     (when clj?
       `(defmacro ~'with-id-filter
          ~(api-docstring 13 purpose
             "Executes form with given %s call id filter in effect.
             See `set-id-filter!` for details.")
          ~'[id-filter form]
          `(binding [~'~*rt-sig-filter* (update-sig-filter ~'~*rt-sig-filter* {:id-filter ~~'id-filter})]
             ~~'form)))))

(comment (api:with-id-filter "purpose" `*my-rt-sig-filter* :clj))

#?(:clj
   (defn- api:set-id-filter!
     [purpose *rt-sig-filter*]
     `(defn ~'set-id-filter!
        ~(api-docstring 11 purpose
           "Sets %s call id filter based on given `id-filter` spec.
           `id-filter` may be:

             - A str/kw/sym, in which \"*\"s act as wildcards.
             - A regex pattern of id/s to allow.
             - A vector or set of regex patterns or strs/kws/syms.
             - {:allow <spec> :deny <spec>} with specs as above.
               If present, `:allow` spec MUST     match, AND
               If present, `:deny`  spec MUST NOT match.")

        ~'[id-filter]
        (enc/force-ref
          (enc/update-var-root! ~*rt-sig-filter*
            (fn [old#] (update-sig-filter old# {:id-filter ~'id-filter})))))))

(comment (api:set-id-filter! "purpose" `*my-rt-sig-filter*))

#?(:clj
   (defn- api:set-min-level!
     [purpose sf-arity *rt-sig-filter*]
     (case (int sf-arity)
       (4)
       `(defn ~'set-min-level!
          ~(api-docstring 13 purpose
             "Sets minimum %s call level based on given `min-level` spec.
             `min-level` may be:

               - `nil` (=> no minimum level).
               - A level keyword (see `level-aliases` var for details).
               - An integer.

             If `ns-filter` is provided, then the given minimum level
             will apply only for the namespace/s that match `ns-filter`.
             See `set-ns-filter!` for details.

             If non-nil `kind` is provided, then the given minimum level
             will apply only for that %s kind.

             Examples:

               (set-min-level! nil)   ; Disable        minimum level
               (set-min-level! :info) ; Set `:info` as minimum level
               (set-min-level! 100)   ; Set 100     as minimum level

               ;; Set `:debug` as minimum level for current namespace
               ;; (nil `kind` => apply to all kinds)
               (set-min-level! nil *ns* :debug)")

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
          ~(api-docstring 13 purpose
             "Sets minimum %s call level based on given `min-level` spec.
             `min-level` may be:

               - `nil` (=> no minimum level).
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
               (set-min-level! *ns* :debug)")

          (~'[          min-level] (~'set-min-level! nil ~'min-level))
          (~'[ns-filter min-level]
           (enc/force-ref
             (enc/update-var-root! ~*rt-sig-filter*
               (fn [old-sf#]
                 (update-sig-filter old-sf#
                   {:min-level-fn
                    (fn [old-ml#]
                      (update-min-level old-ml# nil ~'ns-filter ~'min-level))})))))))))

(comment (api:set-min-level! "purpose" 4 `*my-rt-sig-filter*))

#?(:clj
   (defn- api:with-min-level
     [purpose sf-arity *rt-sig-filter* clj?]
     (let [self (symbol (str *ns*) "with-min-level")]
       (when clj?
         (case (int sf-arity)
           (4)
           `(defmacro ~'with-min-level
              ~(api-docstring 17 purpose
                 "Executes form with given minimum %s call level in effect.
                 See `set-min-level!` for details.")
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
              ~(api-docstring 17 purpose
                 "Executes form with given minimum %s call level in effect.
                 See `set-min-level!` for details.")
              (~'[          min-level form] (list '~self nil ~'min-level ~'form))
              (~'[ns-filter min-level form]
               `(binding [~'~*rt-sig-filter*
                          (update-sig-filter ~'~*rt-sig-filter*
                            {:min-level-fn
                             (fn [~'old-ml#]
                               (update-min-level ~'old-ml# nil ~~'ns-filter ~~'min-level))})]
                  ~~'form))))))))

(comment (api:with-min-level "purpose" 4 `*my-rt-sig-filter* :clj))

#?(:clj
   (defn- api:get-min-level
     [purpose sf-arity *rt-sig-filter* ct-sig-filter]
     (case (int sf-arity)
       (4)
       `(defn ~'get-min-level
          ~(api-docstring 0 purpose "Returns current ?{:keys [compile-time runtime]} minimum levels.")
          (~'[       ] (~'get-min-level nil    (str *ns*)))
          (~'[kind   ] (~'get-min-level ~'kind (str *ns*)))
          (~'[kind ns]
           (enc/assoc-some nil
             :runtime      (parse-min-level (get (enc/force-ref ~*rt-sig-filter*) :min-level) ~'kind ~'ns)
             :compile-time (parse-min-level (get (enc/force-ref  ~ct-sig-filter)  :min-level) ~'kind ~'ns))))

       (2 3)
       `(defn ~'get-min-level
          ~(api-docstring 0 purpose "Returns current ?{:keys [compile-time runtime]} minimum levels.")
          (~'[  ] (~'get-min-level nil (str *ns*)))
          (~'[ns]
           (enc/assoc-some nil
             :runtime      (parse-min-level (get (enc/force-ref ~*rt-sig-filter*) :min-level) nil ~'ns)
             :compile-time (parse-min-level (get (enc/force-ref  ~ct-sig-filter)  :min-level) nil ~'ns)))))))

(comment (api:get-min-level "purpose" 4 `*my-rt-sig-filter* `my-ct-sig-filter))

#?(:clj
   (defmacro def-filter-api
     "Defines signal filter API vars in current ns.
     NB: Cljs ns will need appropriate `:require-macros`."
     [{:keys [purpose sf-arity ct-sig-filter *rt-sig-filter* sig-filter-system-vals-info]
       :or   {purpose "signal"}
       :as   opts}]

     ;; `purpose` ∈ #{"signal" "profiling" "logging" ...}
     (enc/have? #(contains? opts %) :purpose :sf-arity :ct-sig-filter :*rt-sig-filter* #_:*sig-handlers*)
     (enc/have? [:or nil? symbol?] ct-sig-filter *rt-sig-filter* #_*sig-handlers*)

     (let [sf-arity        (int (or sf-arity -1))
           clj?            (not (:ns &env))
           ct-sig-filter   (enc/resolve-sym &env  ct-sig-filter)
           *rt-sig-filter* (enc/resolve-sym &env *rt-sig-filter*)]

       (when-not (contains? #{1 2 3 4} sf-arity)
         (unexpected-sf-artity! sf-arity `def-filter-api))

       `(do
          (enc/defalias level-aliases)
          ~(api:help:filters    purpose sig-filter-system-vals-info)
          ~(api:get-filters     purpose *rt-sig-filter* ct-sig-filter)
          ~(api:without-filters purpose *rt-sig-filter* clj?)

          ~(when (>= sf-arity 4)
             `(do
                ~(api:set-kind-filter! purpose *rt-sig-filter*)
                ~(api:with-kind-filter purpose *rt-sig-filter* clj?)))

          ~(api:set-ns-filter! purpose *rt-sig-filter*)
          ~(api:with-ns-filter purpose *rt-sig-filter* clj?)

          ~(when (>= sf-arity 3)
             `(do
                ~(api:set-id-filter! purpose *rt-sig-filter*)
                ~(api:with-id-filter purpose *rt-sig-filter* clj?)))

          ~(api:set-min-level! purpose sf-arity *rt-sig-filter*)
          ~(api:with-min-level purpose sf-arity *rt-sig-filter* clj?)
          ~(api:get-min-level  purpose sf-arity *rt-sig-filter* ct-sig-filter)))))

(comment
  (macroexpand '(def-filter-api {:purpose nil, :sf-arity 3, :ct-sig-filter my-ct-sig-filter, :*rt-sig-filter* *my-rt-sig-filter*}))
  (do           (def-filter-api {:purpose nil, :sf-arity 3, :ct-sig-filter my-ct-sig-filter, :*rt-sig-filter* *my-rt-sig-filter*})))

#?(:clj
   (defn- api:help:handlers
     [purpose]
     `(def ~'help:handlers
        ~(api-docstring 11 purpose
           "Manage handlers with:

             `get-handlers`        - Returns info on registered handlers
             `shut-down-handlers!` - Shuts down      registered handlers

             `add-handler!`        - Registers   given handler
             `remove-handler!`     - Unregisters given handler

           See the relevant docstrings for details.
           Clj only: `shut-down-handlers!` is called automatically on JVM shutdown.

           If anything is unclear, please ping me (@ptaoussanis) so that I can
           improve these docs!")

        "See docstring")))

(comment (api:help:handlers "purpose"))

#?(:clj
   (defn- api:get-handlers
     [purpose *sig-handlers*]
     `(defn ~'get-handlers
        ~(api-docstring 11 purpose
           "Returns ?{<handler-id> {:keys [dispatch-opts handler-fn]}} for all
           registered %s handlers.")
        [] (get-handlers ~*sig-handlers*))))

(comment (api:get-handlers "purpose" `*my-sig-handlers*))

#?(:clj
   (defn- api:remove-handler!
     [purpose *sig-handlers*]
     `(defn ~'remove-handler!
        ~(api-docstring 11 purpose
           "Deregisters %s handler with given id, and returns
           ?{<handler-id> {:keys [dispatch-opts handler-fn]}} for all %s handlers
           still registered.")
        ~'[handler-id]
        (get-handlers
          (enc/update-var-root! ~*sig-handlers*
            (fn [m#] (remove-handler m# ~'handler-id)))))))

(comment (api:remove-handler! "purpose" `*my-sig-handlers*))

#?(:clj
   (defn- api:add-handler!
     [purpose *sig-handlers* base-dispatch-opts]
     `(defn ~'add-handler!
        ~(api-docstring 11 purpose
           "Registers given %s handler and returns
           {<handler-id> {:keys [dispatch-opts handler-fn]}} for all %s handlers
           now registered.

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

                Default:
                  {:mode :dropping, :buffer-size 4096, :n-threads 1, :daemon-threads? false}

                Options:
                  `mode`        - Mode of operation, ∈ #{:sync :blocking :dropping :sliding}.
                  `buffer-size` - Size of buffer before back-pressure mechanism is engaged.
                  `n-threads`   - Number of threads for asynchronously executing fns.
                                  NB execution order may be non-sequential when n > 1.

             `priority`
               Optional handler priority ∈ℤ (default 100). Handlers will be called in
               descending priority order.

             `sample-rate`
               Optional sample rate ∈ℝ[0,1], or (fn dyamic-sample-rate []) => ℝ[0,1].
               When present, handle only this (random) proportion of args:
                 1.0 => handle every arg (same as `nil` rate, default)
                 0.0 => noop   every arg
                 0.5 => handle random 50%% of args

             `kind-filter` - Kind      filter as in `set-kind-filter!` (when relevant)
             `ns-filter`   - Namespace filter as in `set-ns-filter!`
             `id-filter`   - Id        filter as in `set-id-filter!`   (when relevant)
             `min-level`   - Minimum   level  as in `set-min-level!`

             `when-fn`
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
             `backp-fn` - (fn [{:keys [handler-id                  ]}]) to call on handler back-pressure.

           Flow sequence:

             1. Per call (n=1)
               a. Sampling
               b. Filtering (kind, namespace, id, level, when-form)
               c. Rate limiting
               d. Middleware

             2. Per handler (n>=0)
               a. Sampling
               b. Filtering (kind, namespace, id, level, when-fn)
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
              :keys [async priority sample-rate rate-limit when-fn middleware,
                     kind-filter ns-filter id-filter min-level,
                     error-fn backp-fn]}])}

         (get-handlers
           (enc/update-var-root! ~*sig-handlers*
             (fn [m#]
               (add-handler m# ~'handler-id ~'handler-fn,
                 ~base-dispatch-opts ~'dispatch-opts))))))))

(comment (api:add-handler! "purpose" `*my-sig-handlers* 'base-dispatch-opts))

#?(:clj
   (defn- api:shut-down-handlers!
     [purpose *sig-handlers* clj?]
     (let [docstring
           (api-docstring 13 purpose
             "Shuts down all registered %s handlers in parallel, and returns
             ?{<handler-id> {:keys [okay error]}}.

             Future calls to handlers will no-op.
             Clj only: `shut-down-handlers!` is called automatically on JVM shutdown.")]

       (if-not clj?
         `(defn ~'shut-down-handlers! ~docstring [] (shut-down-handlers! ~*sig-handlers*))
         `(defn ~'shut-down-handlers! ~docstring
            ([              ] (shut-down-handlers! ~*sig-handlers*))
            ([timeout-msecs#] (shut-down-handlers! ~*sig-handlers* timeout-msecs#)))))))

(comment (api:shut-down-handlers! "purpose" `*my-sig-handlers* :clj))

#?(:clj
   (defn- api:add-shutdown-hook!
     [*sig-handlers*]
     `(enc/defonce ~'_handler-shutdown-hook {:private true}
        (.addShutdownHook (Runtime/getRuntime)
          (Thread.
            (fn ~'shut-down-signal-handlers []
              (shut-down-handlers! ~*sig-handlers*)))))))

(comment (api:add-shutdown-hook! `*my-sig-handlers*))

#?(:clj
   (defn- api:with-handler
     [purpose *sig-handlers* base-dispatch-opts clj?]
     (when clj?
       `(defmacro ~'with-handler
          ~(api-docstring 11 purpose
             "Executes form with ONLY the given handler-fn registered.
             Useful for tests/debugging. See also `with-handler+`.")
          ~'[handler-id handler-fn dispatch-opts form]
          `(binding [~'~*sig-handlers* (add-handler {} ~~'handler-id ~~'handler-fn ~~base-dispatch-opts ~~'dispatch-opts)]
             ~~'form)))))

(comment (api:with-handler "purpose" `*my-sig-handlers* {:my-opt :foo} :clj))

#?(:clj
   (defn- api:with-handler+
     [purpose *sig-handlers* base-dispatch-opts clj?]
     (when clj?
       `(defmacro ~'with-handler+
          ~(api-docstring 11 purpose
             "Executes form with the given handler-fn registered.
             Useful for tests/debugging. See also `with-handler`.")
          ~'[handler-id handler-fn dispatch-opts form]
          `(binding [~'~*sig-handlers* (add-handler ~'~*sig-handlers* ~~'handler-id ~~'handler-fn ~~base-dispatch-opts ~~'dispatch-opts)]
             ~~'form)))))

(comment (api:with-handler+ "purpose" `*my-sig-handlers* {:my-opt :foo} :clj))

#?(:clj
   (defmacro def-handler-api
     "Defines signal handler API vars in current ns, and adds JVM hook to trigger
     handler shutdown on JVM shutdown.

     NB: Cljs ns will need appropriate `:require-macros`."
     [{:keys [purpose sf-arity *rt-sig-filter* *sig-handlers* base-dispatch-opts sig-filter-system-vals-info]
       :or   {purpose "signal"}
       :as   opts}]

     ;; `purpose` ∈ #{"signal" "profiling" "logging" ...}
     (enc/have? #(contains? opts %) :purpose :sf-arity #_:ct-sig-filter :*rt-sig-filter* :*sig-handlers*)
     (enc/have? [:or nil? symbol?] #_ct-sig-filter *rt-sig-filter* *sig-handlers*)

     (let [clj?            (not (:ns &env))
           *rt-sig-filter* (enc/resolve-sym &env *rt-sig-filter*)
           *sig-handlers*  (enc/resolve-sym &env *sig-handlers*)]
      `(do
         ~(api:help:handlers       purpose)
         ~(api:get-handlers        purpose *sig-handlers*)
         ~(api:remove-handler!     purpose *sig-handlers*)
         ~(api:add-handler!        purpose *sig-handlers* base-dispatch-opts)
         ~(api:with-handler        purpose *sig-handlers* base-dispatch-opts clj?)
         ~(api:with-handler+       purpose *sig-handlers* base-dispatch-opts clj?)
         ~(api:shut-down-handlers! purpose *sig-handlers* clj?)
         ~(when clj?
            (api:add-shutdown-hook! *sig-handlers*))))))

(comment
  (macroexpand '(def-handler-api {:purpose nil, :sf-arity 3, :*rt-sig-filter* *my-rt-sig-filter*, :*sig-handlers* *my-sig-handlers*}))
  (do           (def-handler-api {:purpose nil, :sf-arity 3, :*rt-sig-filter* *my-rt-sig-filter*, :*sig-handlers* *my-sig-handlers*}))

  (add-handler!    :hid1 (fn [x]) {})
  (remove-handler! :hid1)
  (get-handlers))

#?(:clj
   (defmacro def-api
     "Calls both `def-filter-api` and `def-handler-api` with given opts.
     NB: Cljs ns will need appropriate `:require-macros`."
     [opts]
     `(do
        (def-filter-api  ~opts)
        (def-handler-api ~opts))))

(comment
  (do
    (def            my-ct-sig-filter  nil)
    (def ^:dynamic *my-rt-sig-filter* nil)
    (def ^:dynamic *my-sig-handlers*  nil))

  (def-filter-api
    {:purpose         "testing"
     :sf-arity        3
     :ct-sig-filter    my-ct-sig-filter
     :*rt-sig-filter* *my-rt-sig-filter*
     :*sig-handlers*  *my-sig-handlers*}))
