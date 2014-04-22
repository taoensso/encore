(ns taoensso.encore
  "The utils you want, in the package you deserve™.
  Subset of the commonest Ensso utils w/o external dependencies."
  {:author "Peter Taoussanis"}
  #+clj  (:refer-clojure :exclude [format])
  #+clj  (:require [clojure.string      :as str]
                   [clojure.java.io     :as io]
                   ;; [clojure.core.async  :as async]
                   [clojure.tools.reader.edn :as edn])
  ;; #+clj  (:import [org.apache.commons.codec.binary Base64])
  #+clj  (:import  [java.util Date Locale TimeZone]
                   [java.text SimpleDateFormat])
  ;;;
  #+cljs (:require [clojure.string    :as str]
                   ;; [cljs.core.async   :as async]
                   [cljs.reader       :as edn]
                   ;;[goog.crypt.base64 :as base64]
                   [goog.string         :as gstr]
                   [goog.string.format]
                   [goog.string.StringBuffer]
                   [goog.events         :as gevents]
                   [goog.net.XhrIo      :as gxhr]
                   [goog.net.XhrIoPool  :as gxhr-pool]
                   ;; [goog.net.XhrManager :as xhrm]
                   [goog.Uri.QueryData  :as gquery-data]
                   [goog.structs        :as gstructs]
                   [goog.net.EventType]
                   [goog.net.ErrorCode])
  #+cljs (:require-macros [taoensso.encore :as encore-macros]))

;;;; Core

#+clj
(defn compiling-cljs?*
  "Returns true iff called within the context of the ClojureScript compiler's
  environment. Useful for writing macros that can produce different Clj/Cljs
  code (this isn't something Cljx currently provides support for).
  Stolen from Prismatic code, Ref. http://goo.gl/DhhhSN."
  ;; TODO Is this idiomatic? Checking &env for cljs.env/*compiler* may be an
  ;; alternative?
  []
  (boolean
   (when-let [n (find-ns 'cljs.analyzer)]
     (when-let [v (ns-resolve n '*cljs-file*)]
       @v))))

(defmacro compiling-cljs? [] (compiling-cljs?*))

(defn name-with-attrs
  "Stolen from `clojure.tools.macro`.
  Handles optional docstrings & attr maps for a macro def's name."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
        [attr macro-args] (if (map? (first macro-args))
                            [(first macro-args) (next macro-args)]
                            [{} macro-args])
        attr (if docstring   (assoc attr :doc docstring) attr)
    attr (if (meta name) (conj (meta name) attr)     attr)]
    [(with-meta name attr) macro-args]))

(defmacro defonce*
  "Like `clojure.core/defonce` but supports optional docstring and attributes
  map for name symbol."
  {:arglists '([name expr])}
  [name & sigs]
  (let [[name [expr]] (name-with-attrs name sigs)]
    `(clojure.core/defonce ~name ~expr)))

(defmacro declare-remote
  "Declares the given ns-qualified names, preserving symbol metadata. Useful for
  circular dependencies."
  [& names]
  (let [original-ns (str *ns*)]
    `(do ~@(map (fn [n]
                  (let [ns (namespace n)
                        v  (name n)
                        m  (meta n)]
                    `(do (in-ns  '~(symbol ns))
                         (declare ~(with-meta (symbol v) m))))) names)
         (in-ns '~(symbol original-ns)))))

(defmacro defalias
  "Defines an alias for a var, preserving metadata. Adapted from
  clojure.contrib/def.clj, Ref. http://goo.gl/xpjeH"
  [name target & [doc]]
  `(let [^clojure.lang.Var v# (var ~target)]
     (alter-meta! (def ~name (.getRawRoot v#))
                  #(merge % (apply dissoc (meta v#) [:column :line :file :test :name])
                     (when-let [doc# ~doc] {:doc doc#})))
     (var ~name)))

(defmacro cond-throw "Like `cond` but throws on no-match like `case`, `condp`."
  [& clauses] `(cond ~@clauses :else (throw (ex-info "No matching clause" {}))))

(comment (cond false "false") (cond-throw false "false"))

(defmacro doto-cond "Diabolical cross between `doto`, `cond->` and `as->`."
  [[name x] & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test-expr step]] `(when-let [~name ~test-expr]
                                       (-> ~g ~step)))]
    `(let [~g ~x]
       ~@(map pstep (partition 2 clauses))
       ~g)))

(defmacro case-eval
  "Like `case` but evaluates test constants for their compile-time value."
  [e & clauses]
  (let [;; Don't evaluate default expression!
        default (when (odd? (count clauses)) (last clauses))
        clauses (if default (butlast clauses) clauses)]
    `(case ~e
       ~@(map-indexed (fn [i# form#] (if (even? i#) (eval form#) form#))
                      clauses)
       ~(when default default))))

(defmacro if-lets
  "Like `if-let` but binds multiple values iff all tests are true."
  ([bindings then] `(if-lets ~bindings ~then nil))
  ([bindings then else]
     (let [[b1 b2 & bnext] bindings]
       (if bnext
         `(if-let [~b1 ~b2] (if-lets ~(vec bnext) ~then ~else) ~else)
         `(if-let [~b1 ~b2] ~then ~else)))))

(comment (if-lets [a :a]  a)
         (if-lets [a nil] a)
         (if-lets [a :a b :b]  [a b])
         (if-lets [a :a b nil] [a b]))

(defmacro when-lets
  "Like `when-let` but binds multiple values iff all tests are true."
  [bindings & body]
  (let [[b1 b2 & bnext] bindings]
    (if bnext
      `(when-let [~b1 ~b2] (when-lets ~(vec bnext) ~@body))
      `(when-let [~b1 ~b2] ~@body))))

(comment (when-lets [a :a b nil] "foo"))

(def  nnil?   (complement nil?))
(def  nblank? (complement str/blank?))
(defn nblank-str? [x] (and (string? x) (nblank? x)))

(comment (map nblank-str? ["foo" "" 5]))

(defn first-nth
  ([coll]           (nth coll 0))
  ([coll not-found] (nth coll 0 not-found)))

#+clj (def format clojure.core/format) ; For easier encore/format portability
#+cljs
(defn format "Removed from cljs.core 0.0-1885, Ref. http://goo.gl/su7Xkj"
  [fmt & args] (apply gstr/format fmt args))

;;;; Coercions
;; `parse-x` => success, or nil
;;    `as-x` => success, (sometimes nil arg), or throw

(defn parse-bool
  "Returns x as a unambiguous Boolean, or nil on failure. Requires more
  explicit truthiness than (boolean x)."
  [x]
  (when x
    (cond (or (true? x) (false? x)) x
          (or (= x "false") (= x "FALSE") (= x "0") (= x 0)) false
          (or (= x "true")  (= x "TRUE")  (= x "1") (= x 1)) true
          :else nil)))

(defn as-bool [x] "Like `parse-bool` but throws on unparseable non-nil."
  (when x
    (let [p (parse-bool x)]
      (if-not (nil? p) p
        (throw (ex-info (format "as-bool failed: %s" x) {:type (type x)}))))))

(comment (parse-bool "foo")
         (as-bool    "foo"))

(defn parse-int "Returns x as Long (or JavaScript integer), or nil on failure."
  [x]
  (when x
    #+clj
    (cond (number? x) (long x)
          (string? x) (try (Long/parseLong x)
                           (catch NumberFormatException _
                             (try (long (Float/parseFloat x))
                                  (catch NumberFormatException _ nil))))
          :else       nil)

    #+cljs
    (cond (number? x) (long x)
          (string? x) (let [x (js/parseInt x)]
                        (when-not (js/isNaN x) x))
          :else        nil)))

(defn as-int [x] "Like `parse-int` but throws on unparseable non-nil."
  (when x
    (or (parse-int x)
        (throw (ex-info (format "as-int failed: %s" x) {:type (type x)})))))

(comment (parse-int "122.5h")
         (as-int    "122.5h"))

(defn parse-float "Returns x as Double (or JavaScript float), or nil on failure."
  [x]
  (when x
    #+clj
    (cond (number? x) (double x)
          (string? x) (try (Double/parseDouble x)
                           (catch NumberFormatException _ nil))
          :else       nil)

    #+cljs
    (cond (number? x) (double x)
          (string? x) (let [x (js/parseFloat x)]
                        (when-not (js/isNan x) x))
          :else       nil)))

(defn as-float [x] "Like parse-float` but throws on unparseable non-nil."
  (or (parse-float x)
      (throw (ex-info (format "as-float failed: %s" x) {:type (type x)}))))

(comment (parse-float "122.5h")
         (as-float    "122.5h"))

;;;; Keywords

(defn stringy? [x] (or (keyword? x) (string? x)))
(defn fq-name "Like `name` but includes namespace in string when present."
  [x] (if (string? x) x
          (let [n (name x)]
            (if-let [ns (namespace x)] (str ns "/" n) n))))

(comment (map fq-name ["foo" :foo :foo.bar/baz]))

(defn explode-keyword [k] (str/split (fq-name k) #"[\./]"))
(comment (explode-keyword :foo.bar/baz))

(defn merge-keywords [ks & [as-ns?]]
  (let [parts (->> ks (filterv identity) (mapv explode-keyword) (reduce into []))]
    (when-not (empty? parts)
      (if as-ns? ; Don't terminate with /
        (keyword (str/join "." parts))
        (let [ppop (pop parts)]
          (keyword (when-not (empty? ppop) (str/join "." ppop))
                   (peek parts)))))))

(comment (merge-keywords [:foo.bar nil :baz.qux/end nil])
         (merge-keywords [:foo.bar nil :baz.qux/end nil] true)
         (merge-keywords [:a.b.c "d.e/k"])
         (merge-keywords [:a.b.c :d.e/k])
         (merge-keywords [nil :k])
         (merge-keywords [nil]))

;;;; Bytes

#+clj
(do
  (def ^:const bytes-class (Class/forName "[B"))
  (defn bytes? [x] (instance? bytes-class x))
  (defn ba= [^bytes x ^bytes y] (java.util.Arrays/equals x y))

  (defn ba-concat ^bytes [^bytes ba1 ^bytes ba2]
    (let [s1  (alength ba1)
          s2  (alength ba2)
          out (byte-array (+ s1 s2))]
      (System/arraycopy ba1 0 out 0  s1)
      (System/arraycopy ba2 0 out s1 s2)
      out))

  (defn ba-split [^bytes ba ^Integer idx]
    (let [s (alength ba)]
      (when (> s idx)
        [(java.util.Arrays/copyOf      ba idx)
         (java.util.Arrays/copyOfRange ba idx s)])))

  (comment (String. (ba-concat (.getBytes "foo") (.getBytes "bar")))
           (let [[x y] (ba-split (.getBytes "foobar") 5)]
             [(String. x) (String. y)])))

;;;; Types

#+clj (defn throwable? [x] (instance? Throwable x))
#+clj (defn exception? [x] (instance? Exception x))
(defn error? [x]
  #+clj  (exception? x)
  #+cljs (or (ex-data x) (instance? js/Error x)))

;; (defn- chan? [x]
;;   #+clj  (instance? clojure.core.async.impl.channels.ManyToManyChannel x)
;;   #+cljs (instance? cljs.core.async.impl.channels.ManyToManyChannel    x))

;;; Often useful for assertions, etc.
(defn pos-int?  [x] (and (integer? x) (pos? x)))
(defn nneg-int? [x] (and (integer? x) (not (neg? x))))

(comment (nneg-int? 0))

;;;; Math

(defn pow [n exp] (Math/pow n exp))

(defn round
  [n & [type nplaces]]
  (let [modifier (when nplaces (Math/pow 10.0 nplaces))
        n* (if-not modifier n (* n modifier))
        rounded
        (case (or type :round)
          ;;; Note same API for both #+clj and #+cljs:
          :round (Math/round (double n*))        ; Round to nearest int or nplaces
          :floor (long (Math/floor (double n*))) ; Round down to -inf
          :ceil  (long (Math/ceil  (double n*))) ; Round up to +inf
          :trunc (long n*)                       ; Round up/down toward zero
          (throw (ex-info "Unknown round type" {:type type})))]
    (if-not modifier rounded
      (/ rounded modifier))))

(def round* round) ; Alias for ns refers
(defn round2 "Optimized common case." [n] (/ (Math/round (* n 1000.0)) 1000.0))

(comment
  (round -1.5 :floor)
  (round -1.5 :trunc)
  (round 1.1234567 :floor 5)
  (round 1.1234567 :round 5))

(defn uuid-str
  "Returns a UUIDv4 string of form \"xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx\",
  Ref. http://www.ietf.org/rfc/rfc4122.txt,
       https://gist.github.com/franks42/4159427"
  []
  #+clj (str (java.util.UUID/randomUUID))
  #+cljs
  (let [fs (fn [n] (apply str (repeatedly n (fn [] (.toString (rand-int 16) 16)))))
        g  (fn [] (.toString (bit-or 0x8 (bit-and 0x3 (rand-int 15))) 16))
        sb (.append (goog.string.StringBuffer.)
             (fs 8) "-" (fs 4) "-4" (fs 3) "-" (g) (fs 3) "-" (fs 12))]
    ;;(UUID. sb) ; Equality fails on roundtrips
    (.toString sb)))

(defn exp-backoff "Returns binary exponential backoff value."
  [nattempt & [{:keys [factor] min' :min max' :max :or {factor 1000}}]]
  (let [binary-exp (Math/pow 2 (dec nattempt))
        time (* (+ binary-exp (rand binary-exp)) 0.5 factor)]
    ;; (cond-> time
    ;;         min' (max min')
    ;;         max' (min max'))
    (long (let [time (if min' (max min' time) time)
                time (if max' (min max' time) time)]
            time))))

;;;; Date & time

(defn now-udt []
  #+clj  (System/currentTimeMillis)
  #+cljs (.valueOf (js/Date.)))

(defn now-udt-mock-fn "Useful for testing."
  [& [mock-udts]]
  (let [mock-udts (or mock-udts (range))
        idx       (atom -1)]
    (fn [] (nth mock-udts (swap! idx inc)))))

(comment (with-redefs [now-udt (now-udt-mock-fn)] (repeatedly 10 now-udt)))

(defn secs->ms [secs] (*    secs  1000))
(defn ms->secs [ms]   (quot ms    1000))
(defn ms
  "Returns number of milliseconds in period defined by given args."
  [& {:as opts :keys [years months weeks days hours mins secs msecs ms]}]
  {:pre [(every? #{:years :months :weeks :days :hours :mins :secs :msecs :ms}
                 (keys opts))]}
  (round
   (+ (if years  (* years  1000 60 60 24 365)   0)
      (if months (* months 1000 60 60 24 29.53) 0)
      (if weeks  (* weeks  1000 60 60 24 7)     0)
      (if days   (* days   1000 60 60 24)       0)
      (if hours  (* hours  1000 60 60)          0)
      (if mins   (* mins   1000 60)             0)
      (if secs   (* secs   1000)                0)
      (if msecs  msecs                          0)
      (if ms     ms                             0))))

(def secs (comp ms->secs ms))
(comment (ms   :years 88 :months 3 :days 33)
         (secs :years 88 :months 3 :days 33))

(defmacro thread-local-proxy
  "Thread-safe proxy wrapper, Ref. http://goo.gl/CEBJnQ (instant.clj)."
  [& body] `(proxy [ThreadLocal] [] (initialValue [] (do ~@body))))

(comment
  (.get (thread-local-proxy (println "Foo")))
  (let [p (thread-local-proxy (println "Foo"))]
    (println "---")
    (dotimes [_ 100] (.get p))) ; Prints once

  (let [p (thread-local-proxy (println "Foo"))]
    (println "---")
    (.get p)
    (.get p)
    (future (.get p))
    (.get p)) ; Prints twice (2 threads)
  )

#+clj
(def ^:private simple-date-format*
  "Returns a SimpleDateFormat ThreadLocal proxy."
  (memoize
   (fn [^String pattern & [{:keys [^Locale locale ^TimeZone timezone]}]]
     (thread-local-proxy
      (let [^SimpleDateFormat sdformat
            (if locale
              (SimpleDateFormat. pattern locale)
              (SimpleDateFormat. pattern))]
        (when timezone (.setTimeZone sdformat timezone))
        sdformat)))))

#+clj
(defn simple-date-format
  "Returns a thread-local java.text.SimpleDateFormat for simple date formatting
  and parsing. Uses JVM's default locale + timezone when unspecified.

  (.format (simple-date-format \"yyyy-MMM-dd\") (Date.)) => \"2014-Mar-07\"
  Ref. http://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html

  Prefer the java.time (Java 8) or Joda-Time (external lib) APIs when available.
  Tower also offers facilities built on DateFormat (rather than the more
  restrictive SimpleDateFormat)."
  ;; Note fully qualified type hint!
  ^java.text.SimpleDateFormat [pattern & [{:keys [locale timezone] :as opts}]]
  (.get ^ThreadLocal (simple-date-format* pattern opts)))

(comment (time (dotimes [_ 10000] (.format (simple-date-format "yyyy-MMM-dd")
                                           (Date.)))))

;;;; Collections

(defrecord Swapped [new-val return-val])
(defn      swapped [new-val return-val] (->Swapped new-val return-val))
(defn- as-swapped [x] (if (instance? Swapped x) x {:new-val x :return-val x}))

(defn swap-in!
  "More powerful version of `swap!`:
    * Supports optional `update-in` semantics.
    * Swap fn can return `(swapped <new-val> <return-val>)` rather than just
      <new-val>. This is useful when writing atomic pull fns, etc."
  [atom_ ks f & args]
  (let [ks (if (or (nil? ks) (empty? ks)) nil ks)]
    (loop []
      (let [old-val @atom_
            {:keys [new-val return-val]}
            (if-not ks
              (as-swapped (apply f old-val args))
              (let [old-val-in (get-in old-val ks)
                    {new-val-in :new-val
                     return-val :return-val}
                    (as-swapped (apply f old-val-in args))]
                {:new-val    (assoc-in old-val ks new-val-in)
                 :return-val return-val}))]
        ;; Ref. http://goo.gl/rFG8mW:
        (if-not (compare-and-set! atom_ old-val new-val)
          (recur)
          return-val)))))

;; Actually uses CAS semantics to support `update-in` capability:
(defn reset-in! [atom_ korks newval] (swap-in! atom_ korks (constantly newval)))

(comment
  (let [a_ (atom {:a :A :b :B})] ; Returns new-val (default)
    [(swap-in! a_ [] (fn [m] (assoc m :c :C))) @a_])
  (let [a_ (atom {:a :A :b :B})] ; Returns old-val
    [(swap-in! a_ [] (fn [m] (swapped (assoc m :c :C) m))) @a_])
  (let [a_ (atom {:a {:b :B}})] ; Returns new-val-in (default)
    [(swap-in! a_ [:a] (fn [m] (assoc m :c :C))) @a_])
  (let [a_ (atom {:a {:b :B}})] ; Returns old-val-in
    [(swap-in! a_ [:a] (fn [m] (swapped (assoc m :c :C) m))) @a_]))

(defn dissoc-in [m ks & dissoc-ks] (apply update-in m ks dissoc dissoc-ks))
(defn contains-in? [coll ks] (contains? (get-in coll (butlast ks)) (last ks)))

(comment (dissoc-in    {:a {:b {:c :C :d :D :e :E}}} [:a :b] :c :e)
         (contains-in? {:a {:b {:c :C :d :D :e :E}}} [:a :b :c])
         (contains-in? {:a {:b {:c :C :d :D :e :E}}} [:a]))

(defn assoc-some "Assocs each kv iff its value is not nil."
  [m & kvs] {:pre [(even? (count kvs))]}
  (into (or m {}) (for [[k v] (partition 2 kvs) :when (not (nil? v))] [k v])))

(defn assoc-when "Assocs each kv iff its val is truthy."
  [m & kvs] {:pre [(even? (count kvs))]}
  (into (or m {}) (for [[k v] (partition 2 kvs) :when v] [k v])))

(comment (assoc-some {:a :A} :b nil :c :C :d nil :e :E))

#+clj (defn queue? [x] (instance? clojure.lang.PersistentQueue x))
#+clj
(defn queue "Returns a PersistentQueue containing the args."
  [& items]
  (if-not items clojure.lang.PersistentQueue/EMPTY
    (into clojure.lang.PersistentQueue/EMPTY items)))

(def seq-kvs
  "(seq     {:a :A}) => ([:a :A])
   (seq-kvs {:a :A}) => (:a :A)"
  (partial reduce concat))

(comment (seq-kvs {:a :A :b :B}))

(defn mapply
  "Like `apply` but assumes last arg is a map whose elements should be applied
  to `f` as an unpaired seq:
    (mapply (fn [x & {:keys [y z]}] (str x y z)) 1 {:y 2 :z 3})
      where fn will receive args as: `(1 :y 2 :z 3)`."
  [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn map-kvs [kf vf m]
  (when m
    (let [kf (if-not (identical? kf :keywordize) kf (fn [k _] (keyword k)))
          vf (if-not (identical? vf :keywordize) vf (fn [_ v] (keyword v)))]
      (persistent! (reduce-kv (fn [m k v] (assoc! m (if kf (kf k v) k)
                                                   (if vf (vf v v) v)))
                              (transient {}) (or m {}))))))

(defn map-keys [f m] (map-kvs (fn [k _] (f k)) nil m))
(defn map-vals [f m] (map-kvs nil (fn [_ v] (f v)) m))

(defn filter-kvs [predk predv m]
  (when m
    (reduce-kv (fn [m k v] (if (and (predk k) (predv v)) m (dissoc m k)))
               (or m {}) (or m {}))))

(defn filter-keys [pred m] (filter-kvs pred (constantly true) m))
(defn filter-vals [pred m] (filter-kvs (constantly true) pred m))

(comment (filter-vals (complement nil?) {:a :A :b :B :c false :d nil}))

(defn remove-vals
  "Smaller, common-case version of `filter-vals`. Esp useful with `nil?`/`blank?`
  pred when constructing maps: {:foo (when _ <...>) :bar (when _ <...>)} in a
  way that preservers :or semantics."
  [pred m] (reduce-kv (fn [m k v] (if (pred v) (dissoc m k) m )) m m))

(comment (remove-vals nil? {:a :A :b false :c nil :d :D}))

;; (def keywordize-map #(map-kvs :keywordize nil %))
(defn keywordize-map [m]
  (when m (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} m)))

(comment (keywordize-map nil)
         (keywordize-map {"akey" "aval" "bkey" "bval"}))

(defn as-map "Cross between `hash-map` & `map-kvs`."
  [coll & [kf vf]]
  {:pre  [(coll? coll) (or (nil? kf) (fn? kf) (identical? kf :keywordize))
                       (or (nil? vf) (fn? vf))]
   :post [(or (nil? %) (map? %))]}
  (when-let [s' (seq coll)]
    (let [kf (if-not (identical? kf :keywordize) kf
               (fn [k _] (keyword k)))]
      (loop [m (transient {}) [k v :as s] s']
        (let [k (if-not kf k (kf k v))
              v (if-not vf v (vf k v))
              new-m (assoc! m k v)]
          (if-let [n (nnext s)]
            (recur new-m n)
            (persistent! new-m)))))))

(comment (as-map ["a" "A" "b" "B" "c" "C"] :keywordize
           (fn [k v] (case k (:a :b) (str "boo-" v) v))))

(defn into-all "Like `into` but supports multiple \"from\"s."
  ([to from] (into to from))
  ([to from & more] (reduce into (into to from) more)))

(defn interleave-all
  "Greedy version of `interleave`.
  Ref. https://groups.google.com/d/msg/clojure/o4Hg0s_1Avs/rPn3P4Ig6MsJ"
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (cond
         (and s1 s2)
         (cons (first s1) (cons (first s2)
                                (interleave-all (rest s1) (rest s2))))
         s1 s1
         s2 s2))))

  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (filter identity (map seq (conj colls c2 c1)))]
        (concat (map first ss)
                (apply interleave-all (map rest ss)))))))

(comment (interleave-all [:a :b :c] [:A :B :C :D :E] [:1 :2]))

(defn distinctv "Prefer `set` when order doesn't matter (much faster)."
  ([coll] ; `distinctv`
     (-> (reduce (fn [[v seen] in]
                   (if-not (contains? seen in)
                     [(conj! v in) (conj seen in)]
                     [v seen]))
                 [(transient []) #{}]
                 coll)
         (nth 0)
         persistent!))
  ([keyfn coll] ; `distinctv-by`
     (-> (reduce (fn [[v seen] in]
                   (let [in* (keyfn in)]
                     (if-not (contains? seen in*)
                       [(conj! v in) (conj seen in*)]
                       [v seen])))
                 [(transient []) #{}]
                 coll)
         (nth 0)
         persistent!)))

(comment (distinctv        [[:a 1] [:a 1] [:a 2] [:b 1] [:b 3]])
         (distinctv second [[:a 1] [:a 1] [:a 2] [:b 1] [:b 3]]))

(comment
  (time (dotimes [_ 10000] (distinctv [:a :a :b :c :d :d :e :a :b :c :d])))
  (time (dotimes [_ 10000] (doall (distinct [:a :a :b :c :d :d :e :a :b :c :d]))))
  (time (dotimes [_ 10000] (set [:a :a :b :c :d :d :e :a :b :c :d]))))

(defn distinct-by "Like `sort-by` for distinct. Based on clojure.core/distinct."
  [keyfn coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[v :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [v* (keyfn v)]
                       (if (contains? seen v*)
                         (recur (rest s) seen)
                         (cons v (step (rest s) (conj seen v*)))))))
                 xs seen)))]
    (step coll #{})))

(defn rcompare "Reverse comparator." [x y] (compare y x))

(defn merge-deep-with ; From clojure.contrib.map-utils
  "Like `merge-with` but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

  (merge-deep-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                    {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  => {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(def merge-deep (partial merge-deep-with (fn [x y] y)))

(comment (merge-deep {:a {:b {:c {:d :D :e :E}}}}
                     {:a {:b {:g :G :c {:c {:f :F}}}}}))

(defn greatest "Returns the 'greatest' element in coll in O(n) time."
  [coll & [?comparator]]
  (let [comparator (or ?comparator rcompare)]
    (reduce #(if (pos? (comparator %1 %2)) %2 %1) coll)))

(defn least "Returns the 'least' element in coll in O(n) time."
  [coll & [?comparator]]
  (let [comparator (or ?comparator rcompare)]
    (reduce #(if (neg? (comparator %1 %2)) %2 %1) coll)))

(comment (greatest ["a" "e" "c" "b" "d"]))

(defn repeatedly-into
  "Like `repeatedly` but faster and `conj`s items into given collection."
  [coll n f]
  (if (instance? clojure.lang.IEditableCollection coll)
    (loop [v (transient coll) idx 0]
      (if (>= idx n) (persistent! v)
        (recur (conj! v (f))
               (inc idx))))
    (loop [v coll idx 0]
      (if (>= idx n) v
        (recur (conj v (f))
               (inc idx))))))

(comment (repeatedly-into [] 10 rand))

(defmacro repeatedly-into*
  [coll n & body]
  `(let [coll# ~coll
         n#    ~n]
     (if (instance? clojure.lang.IEditableCollection coll#)
       (loop [v# (transient coll#) idx# 0]
         (if (>= idx# n#)
           (persistent! v#)
           (recur (conj! v# ~@body)
                  (inc idx#))))
       (loop [v#   coll#
              idx# 0]
         (if (>= idx# n#) v#
           (recur (conj v# ~@body)
                  (inc idx#)))))))

(defmacro repeatedly* "Like `repeatedly` but faster and returns a vector."
  [n & body] `(repeatedly-into* [] ~n ~@body))

;;;; Strings

(defn substr
  "Gives a consistent, flexible, cross-platform substring API with support for:
    * Clamping of indexes beyond string limits.
    * Negative indexes: [   0   |   1   |  ...  |  n-1  |   n   ) or
                        [  -n   | -n+1  |  ...  |  -1   |   0   ).
                        (start index inclusive, end index exclusive).

  Note that `max-len` was chosen over `end-idx` since it's less ambiguous and
  easier to reason about - esp. when accepting negative indexes."
  [s start-idx & [max-len]]
  {:pre [(or (nil? max-len) (nneg-int? max-len))]}
  (let [;; s       (str   s)
        slen       (count s)
        start-idx* (if (>= start-idx 0)
                     (min start-idx slen)
                     (max 0 (dec (+ slen start-idx))))
        end-idx*   (if-not max-len slen
                     (min (+ start-idx* max-len) slen))]
    ;; (println [start-idx* end-idx*])
    #+clj  (.substring ^String s start-idx* end-idx*)
    ;; Could also use .substr:
    #+cljs (.substring         s start-idx* end-idx*)))

(comment
  (substr "Hello"  0 5) ; "Hello"
  (substr "Hello"  0 9) ; "Hello"
  (substr "Hello" -4 5) ; "Hello"
  (substr "Hello"  2 2) ; "ll"
  (substr "Hello" -2 2) ; "ll"
  (substr "Hello" -2)   ; "llo"
  (substr "Hello"  2)   ; "llo"
  (substr "Hello"  9 9) ; ""
  (substr "Hello"  0 0) ; ""
  )

(defn str-contains? [s substr]
  #+clj  (.contains ^String s ^String substr)
  #+cljs (not= -1 (.indexOf s substr)))

(defn str-starts-with? [s substr]
  #+clj  (.startsWith ^String s ^String substr)
  #+cljs (zero? (.indexOf s substr)))

(defn str-ends-with? [s substr]
  #+clj  (.endsWith ^String s ^String substr)
  #+cljs (let [s-len      (alength s) ; not .length!
               substr-len (alength substr)]
           (when (>= s-len substr-len)
             (not= -1 (.indexOf s substr (- s-len substr-len))))))

(defn join-once
  "Like `clojure.string/join` but ensures no double separators."
  [separator & coll]
  (reduce
   (fn [s1 s2]
     (let [s1 (str s1) s2 (str s2)]
       (if (str-ends-with? s1 separator)
         (if (str-starts-with? s2 separator)
           (str s1 (.substring s2 1))
           (str s1 s2))
         (if (str-starts-with? s2 separator)
           (str s1 s2)
           (if (or (= s1 "") (= s2 ""))
             (str s1 s2)
             (str s1 separator s2))))))
   nil
   coll))

(defn path
  "Joins string paths (URLs, file paths, etc.) ensuring correct \"/\"
  interposition."
  [& parts] (apply join-once "/" parts))

(comment (path "foo/"  "/bar" "baz/" "/qux/")
         (path "foo" nil "" "bar"))

;; (defn base64-enc "Encodes string as URL-safe Base64 string."
;;   [s] {:pre [(string? s)]}
;;   #+clj  (Base64/encodeBase64URLSafeString (.getBytes ^String s "UTF-8"))
;;   #+cljs (base64/encodeString s (boolean :web-safe)))

;; (defn base64-dec "Decodes Base64 string to string."
;;   [s]
;;   #+clj  (String. (Base64/decodeBase64 ^String s) "UTF-8")
;;   #+cljs (base64/decodeString s (boolean :web-safe)))

;; (comment (-> "Hello this is a test" base64-enc base64-dec))

(defn norm-word-breaks
  "Converts all word breaks of any form and length (including line breaks of any
  form, tabs, spaces, etc.) to a single regular space."
  [s] (str/replace (str s) #"\s+" \space))

(defn count-words [s] (if (str/blank? s) 0 (count (str/split s #"\s+"))))
(count-words "Hello this is a    test")

;;;; IO

#+clj
(do
  (defn- file-resource-last-modified
    "Returns last-modified time for file backing given named resource, or nil if
    file doesn't exist."
    [resource-name]
    (when-let [file (try (->> resource-name io/resource io/file)
                         (catch Exception _))]
      (.lastModified ^java.io.File file)))

  (def file-resources-modified?
    "Returns true iff any files backing the given group of named resources
    have changed since this function was last called."
    (let [;; {#{file1A file1B ...#} (time1A time1A ...),
          ;;  #{file2A file2B ...#} (time2A time2B ...), ...}
          group-times (atom {})]
      (fn [resource-names]
        (let [file-group (into (sorted-set) resource-names)
              file-times (map file-resource-last-modified file-group)
              last-file-times (get @group-times file-group)]
          (when-not (= file-times last-file-times)
            (swap! group-times assoc file-group file-times)
            (boolean last-file-times)))))))

;;;; Memoization

;;; TODO
;; * Consider implementing a self-gc'ing hashmap for use here & elsewhere?
;; * Invalidating memoize* cache doesn't scale horizontally; could easily build
;;   a Redis-backed distributed version with pttl, though it'd be slower.

(def ^:private ^:const gc-rate (/ 1.0 16000))
(defn swap-val! ; Public since it can be useful for custom memoization utils
  "Swaps associative value at key and returns the new value.
  Specialized, fast `swap-in!` for use mostly by memoization utils."
  [atom_ k f]
  (loop []
    (let [old-m @atom_
          new-v (f (get old-m k))
          new-m (assoc old-m k new-v)]
      (if (compare-and-set! atom_ old-m new-m) new-v
        (recur)))))

(defn memoized
  "Like `(memoize* f)` but takes an explicit cache atom (possibly nil)
  and immediately applies memoized f to given arguments."
  [cache f & args]
  (if-not cache ; {<args> <delay-val>}
    (apply f args)
    @(swap-val! cache args #(if % % (delay (apply f args))))))

(defn memoize*
  "Like `clojure.core/memoize` but:
    * Uses delays to prevent race conditions on writes.
    * Supports auto invalidation & gc with `ttl-ms` option.
    * Supports manual invalidation by prepending args with `:mem/del` or `:mem/fresh`.
    * Supports cache size limit & gc with `cache-size` option."
  ([f] ; De-raced, commands
    (let [cache (atom {})] ; {<args> <delay-val>}
      (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
        (if (identical? arg1 :mem/del)
          (do (if (identical? (first argn) :mem/all)
                (reset! cache {})
                (swap!  cache dissoc argn))
              nil)
          (let [fresh? (identical? arg1 :mem/fresh)
                args   (if fresh? argn args)]
            @(swap-val! cache args
               (fn [?dv] (if (and ?dv (not fresh?)) ?dv
                           (delay (apply f args))))))))))

  ([ttl-ms f] ; De-raced, commands, ttl, gc
    (let [cache (atom {})] ; {<args> <[delay-val udt :as cache-val]>}
      (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
        (if (identical? arg1 :mem/del)
          (do (if (identical? (first argn) :mem/all)
                (reset! cache {})
                (swap!  cache dissoc argn))
              nil)

          (do
            (when (<= (rand) gc-rate) ; GC
              (let [instant (now-udt)]
                (swap! cache
                  (fn [m] (reduce-kv (fn [m* k [dv udt :as cv]]
                                      (if (> (- instant udt) ttl-ms) m*
                                          (assoc m* k cv))) {} m)))))

            (let [fresh?  (identical? arg1 :mem/fresh)
                  args    (if fresh? argn args)
                  instant (now-udt)]
              @(first-nth
                (swap-val! cache args
                  (fn [?cv]
                    (if (and ?cv (not fresh?)
                             (let [[_dv udt] ?cv]
                               (< (- instant udt) ttl-ms))) ?cv
                      [(delay (apply f args)) instant]))))))))))

  ([cache-size ttl-ms f] ; De-raced, commands, ttl, gc, max-size
    (let [state (atom {:tick 0})] ; {:tick _
                                  ;  <args> <[dval ?udt tick-lru tick-lfu :as cval]>}
      (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
        (if (identical? arg1 :mem/del)
          (do (if (identical? (first argn) :mem/all)
                (reset! state {:tick 0})
                (swap!  state dissoc argn))
              nil)

          (do
            (when (<= (rand) gc-rate) ; GC
              (let [instant (now-udt)]
                (swap! state
                  (fn [m]
                    (let [m* (dissoc m :tick)
                          ;; First prune expired stuff:
                          m* (if-not ttl-ms m*
                               (reduce-kv (fn [m* k [dv udt _ _ :as cv]]
                                            (if (> (- instant udt) ttl-ms) m*
                                                (assoc m* k cv))) {} m*))
                          n-to-prune (- (count m*) cache-size)
                          ;; Then prune by descending tick-sum:
                          m* (if-not (pos? n-to-prune) m*
                               (->>
                                (keys m*)
                                (mapv (fn [k] (let [[_ _ tick-lru tick-lfu] (m* k)]
                                                [(+ tick-lru tick-lfu) k])))
                                (sort-by #(nth % 0))
                                ;; (#(do (println %) %)) ; Debug
                                (take    n-to-prune)
                                (mapv    #(nth % 1))
                                (apply dissoc m*)))]
                      (assoc m* :tick (:tick m)))))))

            (let [fresh?   (identical? arg1 :mem/fresh)
                  args     (if fresh? argn args)
                  ?instant (when ttl-ms (now-udt))
                  tick'    (:tick @state) ; Accuracy/sync irrelevant
                  dv
                  (first-nth
                   (swap-val! state args
                     (fn [?cv]
                       (if (and ?cv (not fresh?)
                                (or (nil? ?instant)
                                    (let [[_dv udt] ?cv]
                                      (< (- ?instant udt) ttl-ms)))) ?cv
                         [(delay (apply f args)) ?instant tick' 1]))))]

              ;; We always adjust counters, even on reads:
              (swap! state
                (fn [m]
                  (when-let [[dv ?udt tick-lru tick-lfu :as cv] (get m args)]
                    (assoc m :tick (inc tick')
                              args [dv ?udt tick' (inc tick-lfu)]))))

              @dv)))))))

(comment
  (def f0 (memoize         (fn [& xs] (Thread/sleep 600) (rand))))
  (def f1 (memoize*        (fn [& xs] (Thread/sleep 600) (rand))))
  (def f2 (memoize* 5000   (fn [& xs] (Thread/sleep 600) (rand))))
  (def f3 (memoize* 2 nil  (fn [& xs] (Thread/sleep 600) (rand))))
  (def f4 (memoize* 2 5000 (fn [& xs] (Thread/sleep 600) (rand))))

  (time (dotimes [_ 10000] (f0))) ;  ~3ms
  (time (dotimes [_ 10000] (f1))) ;  ~4ms
  (time (dotimes [_ 10000] (f2))) ;  ~9ms
  (time (dotimes [_ 10000] (f3))) ;  ~9ms
  (time (dotimes [_ 10000] (f4))) ; ~11ms

  (f1)
  (f1 :mem/del)
  (f1 :mem/fresh)

  ;; For testing, these need GC set to -always- run
  (f3 "a")
  (f3 "b")
  (f3 "c")
  (f3 "d")

  (println "--")
  (let [f0 (memoize  (fn [] (Thread/sleep 5) (println "compute0")))]
    (dotimes [_ 500] (future (f0)))) ; Prints many
  (let [f1 (memoize* (fn [] (Thread/sleep 5) (println "compute1")))]
    (dotimes [_ 500] (future (f1)))) ; NEVER prints >1
  (let [f4 (memoize* 2 5000 (fn [] (Thread/sleep 5) (println "compute1")))]
    (dotimes [_ 10] (future (f4)))))

(defn rate-limiter
  "Returns a `(fn [& [id]])` that returns either `nil` (limit okay) or number of
  msecs until next rate limit window (rate limited)."
  [ncalls-limit window-ms]
  (let [state (atom [nil {}])] ; [<pull> {<id> {[udt-window-start ncalls]}}]
    (fn [& [id]]

      (when (<= (rand) gc-rate) ; GC
        (let [instant (now-udt)]
          (swap! state
            (fn [[_ m]]
              [nil (reduce-kv
                    (fn [m* id [udt-window-start ncalls]]
                      (if (> (- instant udt-window-start) window-ms) m*
                          (assoc m* id [udt-window-start ncalls]))) {} m)]))))

      (->
       (let [instant (now-udt)]
         (swap! state
           (fn [[_ m]]
             (if-let [[udt-window-start ncalls] (m id)]
               (if (> (- instant udt-window-start) window-ms)
                 [nil (assoc m id [instant 1])]
                 (if (< ncalls ncalls-limit)
                   [nil (assoc m id [udt-window-start (inc ncalls)])]
                   [(- (+ udt-window-start window-ms) instant) m]))
               [nil (assoc m id [instant 1])]))))
       (nth 0)))))

(comment
  (def rl (rate-limiter 10 10000))
  (repeatedly 10 #(rl (rand-nth [:a :b :c])))
  (rl :a)
  (rl :b)
  (rl :c))

(defn rate-limited "Wraps fn so that it returns {:result _ :backoff-ms _}."
  [ncalls-limit window-ms f]
  (let [rl (rate-limiter ncalls-limit window-ms)]
    (fn [& args] (if-let [backoff-ms (rl)]
                  {:backoff-ms backoff-ms}
                  {:result     (f)}))))

(comment (def compute (rate-limited 3 5000 (fn [] "Compute!")))
         (compute))

;;;; Benchmarking

(defmacro time-ms "Returns number of milliseconds it takes to execute body."
  [& body] `(let [t0# (now-udt)] ~@body (- (now-udt) t0#)))

(defmacro time-ns "Returns number of nanoseconds it takes to execute body."
  [& body] `(let [t0# (System/nanoTime)] ~@body (- (System/nanoTime) t0#)))

#+clj
(defn bench*
  "Repeatedly executes fn and returns time taken to complete execution."
  [nlaps {:keys [nlaps-warmup nthreads as-ns?]
          :or   {nlaps-warmup 0
                 nthreads     1}} f]
  (try (dotimes [_ nlaps-warmup] (f))
    (let [nanosecs
          (if (= nthreads 1)
            (time-ns (dotimes [_ nlaps] (f)))
            (let [nlaps-per-thread (int (/ nlaps nthreads))]
              (time-ns
               (->> (fn [] (future (dotimes [_ nlaps-per-thread] (f))))
                    (repeatedly nthreads)
                    (doall)
                    (map deref)
                    (dorun)))))]
      (if as-ns? nanosecs (Math/round (/ nanosecs 1000000.0))))
    (catch Exception e (format "DNF: %s" (.getMessage e)))))

(defmacro bench [nlaps bench*-opts & body]
  `(bench* ~nlaps ~bench*-opts (fn [] ~@body)))

;;;; Client misc

#+cljs
(do ; Logging stuff

  (defn log [x]
    (if (js* "typeof console != 'undefined'")
      (.log js/console x)
      (js/print x))
    nil)

  (defn sayp [& xs]     (js/alert (str/join " " xs)))
  (defn sayf [fmt & xs] (js/alert (apply format fmt xs)))
  (defn logp [& xs]     (log (str/join " " xs)))
  (defn logf [fmt & xs] (log (apply format fmt xs)))

  (def debugf (comp #(str ""        %) logf))
  (def infof  (comp #(str ""        %) logf))
  (def warnf  (comp #(str "WARN: "  %) logf))
  (def errorf (comp #(str "ERROR: " %) logf)))

#+cljs
(defn get-window-location
  "Returns browser window's current location. Forgeable."
  []
  (let [loc* (.-location js/window)
        loc
        {;; Ref. http://bl.ocks.org/abernier/3070589
         :href     (.-href     loc*) ; "http://www.example.org:80/foo/bar?q=baz#bang"
         :protocol (.-protocol loc*) ; "http"
         :hostname (.-hostname loc*) ; "example.org"
         :host     (.-host     loc*) ; "example.org:80"
         :pathname (.-pathname loc*) ; "/foo/bar"
         :search   (.-search   loc*) ; "?q=baz"
         :hash     (.-hash     loc*) ; "#bang"
         }]
    loc))

#+cljs
(defn set-exp-backoff-timeout! [nullary-f & [nattempt]]
  (.setTimeout js/window nullary-f (exp-backoff (or nattempt 0))))

;;;; Ajax

#+cljs (def ^:private xhr-pool_ (delay (goog.net.XhrIoPool.)))
#+cljs
(defn- get-pooled-xhr!
  "Returns an immediately available XhrIo instance, or nil. The instance must be
  released back to pool manually. Use core.async to wait for an available
  instance, etc."
  []
  (let [result (.getObject @xhr-pool_)]
    (when-not (undefined? result) result)))

#+cljs
(defn- coerce-xhr-params "[uri method get-or-post-params] -> [uri post-content]"
  [uri method params] {:pre [(or (nil? params) (map? params))]}
  (let [?pstr ; URL-encoded string, or nil
        (when-not (empty? params)
          (let [s (-> params clj->js gstructs/Map. gquery-data/createFromMap
                      .toString)]
            (when-not (str/blank? s) s)))]
    (case method
      :get  [(if ?pstr (str uri "?" ?pstr) uri) nil]
      :post [uri ?pstr])))

#+cljs
(defn ajax-lite
  "Alpha - subject to change.
  Simple+lightweight Ajax via Google Closure.
  Ref. https://developers.google.com/closure/library/docs/xhrio"
  [uri {:keys [method params headers timeout resp-type]
        :or   {method :get timeout 10000 resp-type :auto}}
   callback]
  {:pre [(or (nil? timeout) (nneg-int? timeout))]}
  (if-let [xhr (get-pooled-xhr!)]
    (try
      (let [method* (case method :get "GET" :post "POST")
            params  (map-keys name params)
            headers (merge {"X-Requested-With" "XMLHTTPRequest"}
                           (map-keys name headers))
            ;;
            [uri* post-content*] (coerce-xhr-params uri method params)
            headers*
            (clj->js
             (if-not post-content* headers
               (assoc headers "Content-Type"
                 "application/x-www-form-urlencoded; charset=UTF-8")))]

        (doto xhr
          (gevents/listenOnce goog.net.EventType/READY
            (fn [_] (.releaseObject @xhr-pool_ xhr)))

          (gevents/listenOnce goog.net.EventType/COMPLETE
            (fn wrapped-callback [resp]
              (let [status       (.getStatus xhr) ; -1 or http-status
                    got-resp?    (not= status -1)
                    content-type (when got-resp?
                                   (.getResponseHeader xhr "Content-Type"))
                    cb-arg
                    {;;; Raw stuff
                     :raw-resp resp
                     :xhr      xhr ; = (.-target resp)
                     ;;;
                     :content-type (when got-resp? content-type)
                     :content
                     (when got-resp?
                       (let [resp-type
                             (if-not (= resp-type :auto) resp-type
                               (condp #(str-contains? %2 %1)
                                   (str content-type) ; Prevent nil
                                 "/edn"  :edn
                                 "/json" :json
                                 "/xml"  :xml
                                 "/html" :text ; :xml only for text/xml!
                                 :text))]
                         (case resp-type
                           :text (.getResponseText xhr)
                           :json (.getResponseJson xhr)
                           :xml  (.getResponseXml  xhr)
                           :edn  (edn/read-string (.getResponseText xhr)))))

                     :status (when got-resp? status) ; nil or http-status
                     :error ; nil, error status, or keyword
                     (if got-resp?
                       (when-not (<= 200 status 299) status) ; Non 2xx resp
                       (get {;; goog.net.ErrorCode/NO_ERROR nil
                             goog.net.ErrorCode/EXCEPTION  :exception
                             goog.net.ErrorCode/HTTP_ERROR :http-error
                             goog.net.ErrorCode/ABORT      :abort
                             goog.net.ErrorCode/TIMEOUT    :timeout}
                            (.getLastErrorCode xhr) :unknown))}]
                (callback cb-arg))))

          (.setTimeoutInterval (or timeout 0)) ; nil = 0 = no timeout
          (.send uri* method* post-content* headers*)))

      (catch js/Error e
        (logf "Ajax error: %s" e)
        (.releaseObject @xhr-pool_ xhr)
        nil))

    ;; Pool failed to return an available xhr instance:
    (callback {:error :xhr-pool-depleted})))

;;;; Ring

#+clj
(defn session-swap
  "Small util to help correctly manage (modify) funtional sessions. Please use
  this when writing Ring middleware!"
  [req resp f & args]
  (when resp
    (if (contains? resp :session) ; Use response session (may be nil)
      (assoc resp :session (apply f (:session resp) args))
      (assoc resp :session (apply f (:session req)  args)))))

(comment
  (session-swap {:session {:req? true}} {:session nil}           assoc :new-k :new-v)
  (session-swap {:session {:req? true}} {:session {:resp? true}} assoc :new-k :new-v)
  (session-swap {:session {:old? true}} {}                       assoc :new-k :new-v))

#+clj
(defn normalize-headers [req-or-resp]
  (when req-or-resp
    (assoc req-or-resp :headers (map-keys str/lower-case (:headers req-or-resp)))))

(comment (normalize-headers {:headers {"Foo1" "bar1" "FOO2" "bar2" "foo3" "bar3"}}))

#+clj
(do
  (defn- ->body-in-map [x] (when x (if-not (map? x) {:body x} x)))
  (defn set-body      [resp body]    (assoc     (->body-in-map resp) :body   body))
  (defn set-status    [resp code]    (assoc     (->body-in-map resp) :status code))
  (defn merge-headers [resp headers] (update-in (->body-in-map resp) [:headers]
                                                merge headers)))

(comment (merge-headers {:body "foo"} {"BAR" "baz"})
         (merge-headers "foo"         {"bar" "baz"}))

#+clj
(defn redirect-resp
  ([url] (redirect-resp :temp url nil))
  ([type url & [flash]]
     {:status  (case type (301 :permanent :perm)     301
                          (302 :temporary :temp nil) 302)
      :headers {"location" url}
      :body    nil
      :flash   flash}))

(comment (redirect-resp :temp "/foo" "boo!"))
