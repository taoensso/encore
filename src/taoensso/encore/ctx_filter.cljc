(ns taoensso.encore.ctx-filter
  "Alpha, subject to change without notice!
  Low-level toolkit for building context filters.
  Used by Telemere, Timbre, Tufte, etc."
  {:added "v3.67.0 (2023-09-08)"}
  (:require
   [clojure.string  :as str]
   [taoensso.encore :as enc :refer [have have?]])

  #?(:cljs
     (:require-macros
      [taoensso.encore.ctx-filter :as cf-macros :refer
       [valid-level-int valid-level level>=]])))

;;;; Levels

(def level-aliases
  "Map of {<level-keyword> <level-integer>} aliases.
  Configurable via the `taoensso/level-aliases` edn system config."
  (enc/nested-merge
    {:min 0 :trace 10 :debug 20 :info 50 :warn 60 :error 70 :fatal 80 :report 90 :max 100}
    (enc/read-sys-val* :taoensso/level-aliases)))

(let [expected (conj (set (keys level-aliases)) 'integer)]
  (defn ^:no-doc bad-level!
    "Throws an `ex-info` for given invalid level."
    [x]
    (throw
      (ex-info "[encore/ctx-filter] Invalid level"
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

     (defmacro ^:no-doc const-level>=
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

;;;; Filter

(let [nf-compile  (fn [nf-spec] (enc/name-filter (or nf-spec :any)))
      nf-conform? (fn [nf-spec n] ((nf-compile nf-spec) n))
      nf->level
      (fn [level-spec s]
        (if (vector? level-spec)
          ;; Spec: [[<nf-spec> <level>] ... [\"*\" <level>]]
          (enc/rsome
            (fn [[nf-spec level]]
              (when (nf-conform? nf-spec s)
                (valid-level-int level)))
            level-spec)
          (valid-level-int level-spec)))]

  (defn valid-nf-spec
    "Returns valid string filter spec, or throws."
    [x]
    (if-let [t (enc/catching (do (nf-compile x) nil) t t)]
      (throw
        (ex-info
          (if (fn? x)
            "[encore/ctx-filter] Invalid name filter spec (fn filters no longer supported)"
            "[encore/ctx-filter] Invalid name filter spec")
          {:nf-spec {:value x, :type (type x)}}
          t))
      x))

  (defn ^:no-doc -filter-name?
    "Low-level uncached util."
    #?(:cljs {:tag boolean})
    [nf-spec n]
    (if (nil? nf-spec) false (if ^boolean (nf-conform? nf-spec n) false true)))

  (defn ^:no-doc -filter-level?
    "Low-level uncached util."
    #?(:cljs {:tag boolean})

    ([min-level level]
     (if (nil? min-level)
       false
       (if ^boolean (level>= level min-level) false true)))

    ([level-spec nf-name level]
     (if (nil? level-spec)
       false
       (let [min-level (nf->level level-spec nf-name)]
         (if ^boolean (level>= level min-level) false true)))))

  (defn ^:no-doc -filter?
    "Low-level uncached util."
    #?(:cljs {:tag boolean})
    [ns-spec level-spec ns level]
    (or
      ^boolean (-filter-name?  ns-spec    ns)
      ^boolean (-filter-level? level-spec ns level))))

(enc/def* filter?
  "Returns true iff `ns` and `level` are filtered (disallowed) by given criteria."
  {:arglists #_(:arglists (meta #'-filter?)) '([ns-spec level-spec ns level])
   :tag      #?(:cljs boolean :clj nil)}
  (enc/fmemoize -filter?))

(comment
  (do         (filter? nil [["my-ns" 5]] "my-ns" 10))
  (enc/qb 1e6 (filter? nil [["my-ns" 5]] "my-ns" :info)) 98.14)

;;;; Utils

(defn valid-level-spec
  "Returns valid level-spec, or throws."
  [x]
  (if (vector? x)
    (do
      (enc/run!
        (fn [[nf-spec level]]
          (valid-nf-spec nf-spec)
          (valid-level level))
        x)
      x)
    (valid-level x)))

(defn update-level-spec
  "TODO Docstring
  ns, old, new may be nil"
  ([old-spec         new-spec] (update-level-spec old-spec nil new-spec))
  ([old-spec nf-spec new-spec]
   (if (nil? nf-spec)
     (when new-spec
       (valid-level-spec new-spec))

     ;; Update nf-specific level-spec
     (let [new-spec (when new-spec (valid-level new-spec))
           nf-spec  (valid-nf-spec nf-spec)

           old-vec (if (vector? old-spec) old-spec (if old-spec [["*" (valid-level old-spec)]] []))
           new-vec
           (reduce ; Remove any pre-existing [<s> _] or [#{<s>} _] entries
             (fn [acc [nf-spec* _level :as entry]]
               (if-let [exact-match?
                        (or
                          (= nf-spec*   nf-spec)
                          (= nf-spec* #{nf-spec}))]
                 (do   acc)       ; Remove entry
                 (conj acc entry) ; Retain entry
                 ))

             (if new-spec
               [[nf-spec new-spec]] ; Insert new-spec entry at head
               [])

             old-vec)]

       (if-let [simplified ; [["*" :info]] -> :info
                (when (= (count new-vec) 1)
                  (let [[[nf-spec level]] new-vec]
                    (when (contains? #{"*" :any} nf-spec)
                      level)))]
         simplified
         (not-empty new-vec))))))

(comment :see-tests)
