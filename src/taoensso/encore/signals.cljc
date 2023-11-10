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
    (enc/read-sys-val* :taoensso.encore.signals/level-aliases)))

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

(defprotocol IFilterableSignal
  (allow-signal? [_ sig-filter] "Returns true iff given signal is allowed by given `SigFilter`.")
  (signal-value  [_]            "Returns signal's user-facing value as given to handlers, etc."))

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
     Returns {:keys [callsite elide? allow?]}.

     `macro-opts`, `opts-arg` are both of form:
       {:keys [kind id level sample rate-limit when]}.

     Caller is responsible for protecting against possible multiple eval of
     forms in `opts-arg`."

     [{:as   macro-opts
       :keys [loc opts-arg sf-arity ct-sig-filter rt-sig-filter]}]

     (when-not (or (nil? opts-arg) (map? opts-arg))
       (throw
         (ex-info "[encore/signals] `filterable-expansion` `opts-arg` must be a compile-time map"
           {:loc loc, :opts-arg opts-arg})))

     (let [callsite (callsite-counter) ; Unique expansion id
           {:keys [ns line column file]} loc

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

           base-rv {:callsite callsite}]

       (if elide?
         (assoc base-rv :allow? false, :elide? true)
         (let [allow?-form
               (let [sample-form
                     (when-let [rate-form (get-opt-form :sample)]
                       (if (enc/const-form? rate-form)
                         `(< (Math/random) ~(enc/as-pnum! rate-form))
                         `(< (Math/random) (double       ~rate-form))))

                     sf-form
                     (case (int (or sf-arity -1))
                       2 `(if-let [~'sf ~rt-sig-filter] (~'sf ~ns                     ~level-form) true)
                       3 `(if-let [~'sf ~rt-sig-filter] (~'sf ~ns            ~id-form ~level-form) true)
                       4 `(if-let [~'sf ~rt-sig-filter] (~'sf ~ns ~kind-form ~id-form ~level-form) true)
                       (unexpected-sf-artity! sf-arity `callsite-filter))

                     filter-form
                     (when-let [filter-form-entry (or (find-opt-form :filter) (find-opt-form :when))]
                       `(let [~'this-callsite ~callsite] ~(val filter-form-entry)))

                     rl-form ; Nb last (increments count)
                     (when-let [spec-form (get-opt-form :rate-limit)]
                       `(if (callsite-limit!? ~callsite ~spec-form nil) false true))]

                 `(and ~@(filter some? [sample-form sf-form filter-form rl-form])))]

           (assoc base-rv :allow? allow?-form))))))

(comment
  (filterable-expansion
    {:sf-arity 2, :rt-sig-filter `*foo*, :loc {:ns (str *ns*)}
     :opts-arg
     {:filter     'false
      :level      (do :info)
      :sample     0.3
      :rate-limit [[1 1000]]}}))

;;;; Signal handling

(comment (enc/defonce ^:dynamic *sig-handlers* "?{<handler-id> <wrapped-handler-fn>}" nil))

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
