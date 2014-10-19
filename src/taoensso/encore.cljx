(ns taoensso.encore
  "The utils you want, in the package you deserve™.
  Subset of the commonest Ensso utils w/o external dependencies."
  {:author "Peter Taoussanis"}
  #+clj  (:refer-clojure :exclude [format])
  #+clj  (:require [clojure.string      :as str]
                   [clojure.set         :as set]
                   [clojure.java.io     :as io]
                   ;; [clojure.core.async  :as async]
                   [clojure.tools.reader.edn :as edn])
  ;; #+clj  (:import [org.apache.commons.codec.binary Base64])
  #+clj  (:import  [java.util Date Locale TimeZone]
                   [java.text SimpleDateFormat])
  ;;;
  #+cljs (:require [clojure.string    :as str]
                   [clojure.set         :as set]
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
  #+cljs (:require-macros [taoensso.encore :as encore-macros
                           :refer [have? have have-in]]))

;;;; Core

(defmacro ^:also-cljs if-cljs
  "Executes `then` clause iff generating ClojureScript code.
  Useful for writing macros that can produce different Clj/Cljs code (this isn't
  something Cljx currently provides support for). Stolen from Prismatic code,
  Ref. http://goo.gl/DhhhSN,
       https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ."
  [then else]
  (if (:ns &env) ; nil in Clojure, nnil in ClojureScript
    then else))

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

(defmacro ^:also-cljs defonce*
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

(defmacro ^:also-cljs cond-throw
  "Like `cond` but throws on no-match like `case`, `condp`."
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

(defmacro ^:also-cljs case-eval
  "Like `case` but evaluates test constants for their compile-time value."
  [e & clauses]
  (let [;; Don't evaluate default expression!
        default (when (odd? (count clauses)) (last clauses))
        clauses (if default (butlast clauses) clauses)]
    `(case ~e
       ~@(map-indexed (fn [i# form#] (if (even? i#) (eval form#) form#))
                      clauses)
       ~(when default default))))

(defmacro ^:also-cljs if-lets
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

(defmacro ^:also-cljs when-lets
  "Like `when-let` but binds multiple values iff all tests are true."
  [bindings & body]
  (let [[b1 b2 & bnext] bindings]
    (if bnext
      `(when-let [~b1 ~b2] (when-lets ~(vec bnext) ~@body))
      `(when-let [~b1 ~b2] ~@body))))

(comment (when-lets [a :a b nil] "foo"))

;;;; Types

;; ClojureScript keywords aren't `identical?` and Clojure doesn't have
;; `keyword-identical?`. This util helps alleviate the pain of writing
;;  cross-platform code. Ref. http://goo.gl/be8CGP.
#+clj  (def kw-identical? identical?)
#+cljs (def kw-identical? keyword-identical?)

(defn atom? [x]
  #+clj  (instance? clojure.lang.Atom x)
  #+cljs (instance? Atom              x))

;; (defn- chan? [x]
;;   #+clj  (instance? clojure.core.async.impl.channels.ManyToManyChannel x)
;;   #+cljs (instance?    cljs.core.async.impl.channels.ManyToManyChannel x))

#+clj (defn throwable? [x] (instance? Throwable x))
#+clj (defn exception? [x] (instance? Exception x))
      (defn error?     [x] #+clj  (throwable? x)
                           #+cljs (instance? js/Error x))
(defn error-data [x]
  "Returns data map iff `x` is an error of any type on platform."
  (when-let [data-map (or (ex-data x) ; ExceptionInfo
                          #+clj  (when (instance? Throwable x) {})
                          #+cljs (when (instance? js/Error  x) {}))]
    (merge data-map
      #+clj  (let [^Throwable t x] ; (catch Throwable t <...>)
               {:type*    (type        t)
                :message* (.getMessage t)
                :cause*   (.getCause   t)})
      #+cljs (let [err x] ; (catch :default t <...)
               {:type*    (type        err)
                :message* (.-message   err)
                :cause*   (.-cause     err)}))))

(comment (error-data (Throwable. "foo"))
         (error-data (Exception. "foo"))
         (error-data (ex-info    "foo" {:bar :baz})))

(defmacro ^:also-cljs catch-errors
  "Experimental. Returns [<?result> <?error>]."
  [& body]
  (if-cljs
    `(try [(do ~@body)] (catch :default  e# [nil e#]))
    `(try [(do ~@body)] (catch Throwable t# [nil t#]))))

(comment (catch-errors (zero? "a")))

(defmacro caught-error-data "Handy for error-throwing unit tests."
  [& body]
  `(let [[result# err#] (catch-errors ~@body)]
     (when err# (error-data err#))))

(comment (caught-error-data (/ 5 0)))

;;; Or compose with `every-pred`, `some-fn` (`have` also now traps errors)
(defn       nnil? [x] (not (nil? x))) ; some?
(defn     nblank? [x] (not (str/blank? x)))
(defn       nneg? [x] (not (neg? x)))
(defn    pos-int? [x] (and (integer? x) (pos? x)))
(defn   nneg-int? [x] (and (integer? x) (not (neg? x))))
(defn   zero-num? [x] (= 0 x)) ; Unlike `zero?`, works on non-nums
(defn    pos-num? [x] (and (number? x) (pos? x)))
(defn   nneg-num? [x] (and (number? x) (not (neg? x))))
(defn nblank-str? [x] (and (string? x) (not (str/blank? x))))

(defn nnil=
  ([x y]        (and (nnil? x) (= x y)))
  ([x y & more] (and (nnil? x) (apply = x y more))))

(comment (nnil= nil nil)
         (nnil= :foo :foo :foo))

(defn nvec? "Is `x` a vector of size `n`?" [n x] (and (vector? x) (= (count x) n)))

(comment (nvec? 2 [:a :b]))

;;;; Validation ; Experimental!!
;; * `have?`   - pred form assertion/s; on success returns true.
;; * `have`    - pred form assertion/s; on success returns input/s.
;; * `have-in` - pred coll assertion/s; on success returns input/s.

(declare format)
(defn assertion-error [msg] #+clj (AssertionError. msg) #+cljs (js/Error. msg))
(defn hthrow "Implementation detail." [ns-str line form val]
  ;; http://dev.clojure.org/jira/browse/CLJ-865 would be handy for line numbers:
  (let [pattern "Assert failed in `%s:%s` [pred-form,val]: [%s,%s]"]
    (throw (assertion-error (format pattern ns-str line
                              (pr-str form) (pr-str val))))))

(declare set*)
(defn hpred "Implementation detail." [pred-form]
  (if-not (vector? pred-form) pred-form
    (let [[type p1 p2 & more] pred-form]
      (case type
        :in  (fn [x] (contains? (set* p1) x))
        ;; (apply some-fn preds):
        :or  (fn [x] (or (when p1 (p1 x)) (when p2 (p2 x)) (some #(% x) more)))
        ;; (apply every-pred preds):
        :and (fn [x] (and (if-not p1 true (p1 x)) (if-not p2 true (p2 x))
                      (every? #(% x) more)))))))

(comment (hpred string?) (hpred [:or nil? string?]))

(defmacro ^:also-cljs asserted* "Implementation detail."
  ([line truthy? x] `(asserted* ~line ~truthy? nnil? ~x))
  ([line truthy? pred x]
     (if-not *assert* (or truthy? x)
       `(let [[[x# pass?#] err#]
              (catch-errors
                (let [pred# ~pred
                      x#    ~x]
                  [x# ((hpred pred#) x#)]))]
          (if pass?# (or ~truthy? x#)
            (hthrow (str *ns*) ~line (list '~pred '~x) (or x# err#))))))
  ([line truthy? pred x & more]
     (let [xs (into [x] more)]
       (if-not *assert* (or truthy? xs)
         ;; Truthy when nothing throws + allows [] destructuring:
         (mapv (fn [x] `(asserted* ~line ~truthy? ~pred ~x)) xs)))))

(defmacro ^:also-cljs have?
  "Experimental. Like `assert` but:
    * Takes a pred and x/s.
    * Returns true on success for convenient use in pre/post conds.
    * Traps errors.
    * Provides better messages on failure!"
  [& args] `(asserted* ~(:line (meta &form)) (boolean :truthy) ~@args))

(defmacro ^:also-cljs have
  "Experimental. Like `have?` but returns input/s on success for use in bindings."
  [& args] `(asserted* ~(:line (meta &form)) (not :truthy) ~@args))

(defmacro ^:also-cljs have-in
  "Experimental. Like `have` but takes an evaluated, single-form collection arg."
  ([xs] `(have-in nnil? ~xs))
  ([pred xs]
     (if-not *assert* xs
       (let [g (gensym "have-in__")] ; Will (necessarily) lose exact form
         `(mapv (fn [~g] (have ~pred ~g)) ~xs)))))

(comment
  (let [x 5]      (have integer? x))
  (let [x 5]      (have string?  x))
  (let [x 5 y  6] (have odd?     x x x y x))
  (let [x 0 y :a] (have zero?    x x x y x))
  (have string? (do (println "eval1") "foo")
                (do (println "eval2") "bar"))
  (have number? (do (println "eval1") "foo")
                (do (println "eval2") "bar"))
  (have-in string? ["a" "b"])
  (have-in string? (if true ["a" "b"] [1 2]))
  (have-in string? (mapv str (range 10)))
  (have-in string? [1 2])
  ((fn foo [x] {:pre [(have integer? x)]} (* x x)) "foo")
  (macroexpand '(have? a))
  (have? [:or nil? string?] "hello")
  (qb 10000 (have? "a") (have string? "a" "b" "c") (have? [:or nil? string?] "a" "b" "c")))

(defmacro ^:also-cljs check-some
  "Experimental. Returns first logical false/throwing expression (id/form), or nil."
  ([test & more] `(or ~@(map (fn [test] `(check-some ~test)) (cons test more))))
  ([test]
     (let [[error-id test] (if (vector? test) test [nil test])]
       `(let [[test# err#] (catch-errors ~test)]
          (when-not test# (or ~error-id '~test :check/falsey))))))

(defmacro ^:also-cljs check-all
  "Experimental. Returns all logical false/throwing expressions (ids/forms), or nil."
  ([test] `(check-some ~test))
  ([test & more]
     `(let [errors# (filter identity
                      (list ~@(map (fn [test] `(check-some ~test))
                                (cons test more))))]
        (when-not (empty? errors#) errors#))))

(comment (check-some false [:bad-type (string? 0)] nil [:blank (str/blank? 0)])
         (check-all  false [:bad-type (string? 0)] nil [:blank (str/blank? 0)]))

(defn vec* [x] (if (vector? x) x (vec x)))
(defn set* [x] (if (set?    x) x (set x)))

;;; Useful for map assertions, etc. (do *not* check that input is a map)
(defn ks=      [ks m] (=             (set (keys m)) (set* ks)))
(defn ks<=     [ks m] (set/subset?   (set (keys m)) (set* ks)))
(defn ks>=     [ks m] (set/superset? (set (keys m)) (set* ks)))
(defn ks-nnil? [ks m] (every? #(nnil? (get m %)) ks))

(comment
  (ks=      {:a :A :b :B  :c :C}  #{:a :b})
  (ks<=     {:a :A :b :B  :c :C}  #{:a :b})
  (ks>=     {:a :A :b :B  :c :C}  #{:a :b})
  (ks-nnil? {:a :A :b :B  :c nil} #{:a :b})
  (ks-nnil? {:a :A :b nil :c nil} #{:a :b}))

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
          (string? x) (let [x (js/parseInt x 10)]
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

;;;; Math

(defn pow [n exp] (Math/pow n exp))
(defn abs [n]     (if (neg? n) (- n) n)) ; #+clj (Math/abs n) reflects

(defn round
  [n & [type nplaces]]
  (let [modifier (when nplaces (Math/pow 10.0 nplaces))
        n* (if-not modifier n (* n modifier))
        rounded
        (case (or type :round)
          ;;; Note same API for both #+clj, #+cljs:
          :round (Math/round (double n*))        ; Round to nearest int or nplaces
          :floor (long (Math/floor (double n*))) ; Round down to -inf
          :ceil  (long (Math/ceil  (double n*))) ; Round up to +inf
          :trunc (long n*)                       ; Round up/down toward zero
          (throw (ex-info "Unknown round type" {:type type})))]
    (if-not modifier rounded
      (/ rounded modifier))))

(def round* round) ; Alias for ns refers
(defn round2 "Optimized common case." [n] (/ (Math/round (* n 100.0)) 100.0))

(comment
  (round -1.5 :floor)
  (round -1.5 :trunc)
  (round 1.1234567 :floor 5)
  (round 1.1234567 :round 5))

(defn exp-backoff "Returns binary exponential backoff value."
  [nattempt & [{:keys [factor] min' :min max' :max :or {factor 1000}}]]
  (let [binary-exp (Math/pow 2 (dec nattempt))
        time       (* (+ binary-exp (rand binary-exp)) 0.5 factor)]
    (long (let [time (if min' (max min' time) time)
                time (if max' (min max' time) time)]
            time))))

;;;; Date & time

(defn  now-dt [] #+clj (java.util.Date.) #+cljs (js/Date.))
(defn now-udt []
  #+clj  (System/currentTimeMillis)
  #+cljs (.getTime (js/Date.)))

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
  {:pre [(have-in #{:years :months :weeks :days :hours :mins :secs :msecs :ms}
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

(comment (qb 10000 (.format (simple-date-format "yyyy-MMM-dd") (Date.))))

;;;; Collections

(defrecord Swapped [new-val return-val])
(defn      swapped [new-val return-val] (Swapped. new-val return-val))
(defn-  as-swapped [x] (if (instance? Swapped x) [(:new-val x) (:return-val x)]
                           [x x]))

(comment ; TODO Debug, Ref. http://dev.clojure.org/jira/browse/CLJ-979
  (defrecord Foo1 [x])
  (instance? Foo1 (Foo1.  "bar"))
  (instance? Foo1 (->Foo1 "bar"))
  (compile 'taoensso.encore))

(declare dissoc-in)

;; Recall: no `korks` support since it makes `nil` ambiguous (`[]` vs `[nil]`).
;; This ambiguity extends to (assoc-in {} [] :a), which (along with perf)
;; is why we special case empty/nil ks.
(defn- replace-in*
  "Reduces input with
  [<type> <ks> <reset-val-or-swap-fn>] or
         [<ks> <reset-val-or-swap-fn>] ops."
  [?vf-type m ops]
  (reduce
    (fn [accum ?op]
      (if-not ?op ; Allow conditional ops: (when <pred> <op>), etc.
        accum
        (let [[vf-type ks valf] (if-not ?vf-type ?op (cons ?vf-type ?op))]
          (case vf-type
            :reset (if (empty? ks) valf (assoc-in accum ks valf))
            :swap  (if (empty? ks)
                     (valf accum)
                     (if (= valf :swap/dissoc)
                       (dissoc-in accum (butlast ks) (last ks))
                       (assoc-in  accum ks (valf (get-in accum ks)))))))))
    m ops))

(defn replace-in "Experimental. For use with `swap!`, etc."
  [m & ops] (replace-in* nil m ops))

(comment
  (replace-in {}
    [:reset [:a] {:b :B :c 100}]
    (when false [:reset [:a :b] :B2]) ; conditionals okay
    (do (assert true)
        [:reset [:a :b] :B3]) ; side-effects okay
    (let [my-swap-fn inc] ; `let`s okay
      [:swap [:a :c] my-swap-fn]))

  (let [a_ (atom {})]
    (swap! a_ replace-in
      [:reset [:a]    {:b :b1 :c :c1 :d 100}]
      [:swap  [:a :d] inc]
      [:swap  [:a :b] :swap/dissoc])))

(defn swap-in!
  "More powerful version of `swap!`:
    * Supports optional `update-in` semantics.
    * Swap fn can return `(swapped <new-val> <return-val>)` rather than just
      <new-val>. This is useful when writing atomic pull fns, etc."
  ([atom_ ks f]
     (if (empty? ks)
       (loop []
         (let [old-val @atom_
               [new-val return-val] (as-swapped (f old-val))]
           (if-not (compare-and-set! atom_ old-val new-val)
             (recur) ; Ref. http://goo.gl/rFG8mW
             return-val)))

       (loop []
         (let [old-val @atom_
               old-val-in (get-in old-val ks)
               [new-val-in return-val] (as-swapped
                                         (if (= f :swap/dissoc) :swap/dissoc
                                           (f old-val-in)))
               new-val (if (= new-val-in :swap/dissoc)
                         (dissoc-in old-val (butlast ks) (last ks))
                         (assoc-in  old-val ks new-val-in))]
           (if-not (compare-and-set! atom_ old-val new-val)
             (recur)
             return-val)))))

  ;; Experimental:
  ([atom_ ks f & more] {:pre [(have? even? (count more))]}
     (let [pairs (into [[ks f]] (partition 2 more))]
       (swap! atom_ (fn [old-val] (replace-in* :swap old-val pairs))))))

(defn reset-in! "Is to `reset!` as `swap-in!` is to `swap!`."
  ([atom_ ks new-val]
     (if (empty? ks)
       (reset! atom_ new-val)
       ;; Actually need swap! (CAS) to preserve other keys:
       (swap!  atom_ (fn [old-val] (assoc-in old-val ks new-val)))))

  ;; Experimental:
  ([atom_ ks new-val & more] {:pre [(have? even? (count more))]}
     (let [pairs (into [[ks new-val]] (partition 2 more))]
       (swap! atom_ (fn [old-val] (replace-in* :reset old-val pairs))))))

(comment
  ;;; update-in, `swapped`
  (let [a_ (atom {:a :A :b :B})] ; Returns new-val (default)
    [(swap-in! a_ [] (fn [m] (assoc m :c :C))) @a_])
  (let [a_ (atom {:a :A :b :B})] ; Returns old-val
    [(swap-in! a_ [] (fn [m] (swapped (assoc m :c :C) m))) @a_])
  (let [a_ (atom {:a {:b :B}})] ; Returns new-val-in (default)
    [(swap-in! a_ [:a] (fn [m] (assoc m :c :C))) @a_])
  (let [a_ (atom {:a {:b :B}})] ; Returns old-val-in
    [(swap-in! a_ [:a] (fn [m] (swapped (assoc m :c :C) m))) @a_])
  (let [a_ (atom {:a {:b 100}})] (swap-in! a_ [:a :b] inc)) ; => 101

  (let [a_ (atom {:a {:b :b1 :c :c1} :d :d1})]
    (swap-in! a_ [:a :c] :swap/dissoc) @a_)

  ;;; Bulk atomic updates
  (let [a_ (atom {})]
    (swap-in! a_
      []      (constantly {:a {:b :b1 :c :c1 :d 100}})
      [:a :b] (constantly :b2)
      [:a]    #(dissoc % :c)
      [:a :d] inc))

  (let [a_ (atom {})]
    (reset-in! a_
      []      {:a {:b :b1 :c :c1 :d 100}}
      [:a :b] :b2
      [:a :d] inc)))

(defn dissoc-in [m ks & dissoc-ks]
  (if (empty? ks)
    (apply dissoc m dissoc-ks)
    (apply update-in m ks dissoc dissoc-ks)))
(defn contains-in? [coll ks] (contains? (get-in coll (butlast ks)) (last ks)))

(comment (dissoc-in    {:a {:b {:c :C :d :D :e :E}}} [:a :b] :c :e)
         (contains-in? {:a {:b {:c :C :d :D :e :E}}} [:a :b :c])
         (contains-in? {:a {:b {:c :C :d :D :e :E}}} [:a]))

(defn assoc-some "Assocs each kv iff its value is not nil."
  [m & kvs] {:pre [(have? even? (count kvs))]}
  (into (or m {}) (for [[k v] (partition 2 kvs) :when (not (nil? v))] [k v])))

(defn assoc-when "Assocs each kv iff its val is truthy."
  [m & kvs] {:pre [(have? even? (count kvs))]}
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
  (apply f (concat (butlast args) (seq-kvs (last args)))))

(defn- clj1098
  "Workaround for Clojure versions [1.4, 1.5) that blow up on `reduce-kv`s
  against a nil coll, Ref. http://dev.clojure.org/jira/browse/CLJ-1098."
  [x] (or x {}))

(defn map-kvs [kf vf m]
  (if-not m {} ; Note also clj1098-safe
    (let [kf (if-not (kw-identical? kf :keywordize) kf (fn [k _] (keyword k)))
          vf (if-not (kw-identical? vf :keywordize) vf (fn [_ v] (keyword v)))]
      (persistent! (reduce-kv (fn [m k v] (assoc! m (if kf (kf k v) k)
                                                   (if vf (vf v v) v)))
                              (transient {}) m)))))

(defn map-keys [f m] (map-kvs     (fn [k _] (f k)) nil m))
(defn map-vals [f m] (map-kvs nil (fn [_ v] (f v)) m))

(defn filter-kvs [predk predv m]
  (if-not m {} ; Note also clj1098-safe
    (reduce-kv (fn [m k v] (if (and (predk k) (predv v)) m (dissoc m k))) m m)))

(defn filter-keys [pred m] (filter-kvs pred (constantly true) m))
(defn filter-vals [pred m] (filter-kvs (constantly true) pred m))

(comment (filter-vals (complement nil?) {:a :A :b :B :c false :d nil}))

(defn remove-vals
  "Smaller, common-case version of `filter-vals`. Esp useful with `nil?`/`blank?`
  pred when constructing maps: {:foo (when _ <...>) :bar (when _ <...>)} in a
  way that preservers :or semantics."
  [pred m]
  (if-not m {} ; Note also clj1098-safe
    (reduce-kv (fn [m k v] (if (pred v) (dissoc m k) m)) m m)))

(comment (remove-vals nil? {:a :A :b false :c nil :d :D}))

;; (def keywordize-map #(map-kvs :keywordize nil %))
(defn keywordize-map [m]
  (if-not m {} ; Note also clj1098-safe
    (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} m)))

(comment (keywordize-map nil)
         (keywordize-map {"akey" "aval" "bkey" "bval"}))

(defn as-map "Cross between `hash-map` & `map-kvs`."
  [coll & [kf vf]]
  {:pre  [(have? [:or nil? coll?] coll)
          (have? [:or nil? ifn?]  kf vf)]
   :post [(have? [:or nil? map?]  %)]}
  (when coll
    (if (empty? coll) {}
      (let [kf (if-not (kw-identical? kf :keywordize) kf
                 (fn [k _] (keyword k)))]
        (loop [m (transient {}) [k v :as s] coll]
          (let [k (if-not kf k (kf k v))
                v (if-not vf v (vf k v))
                new-m (assoc! m k v)]
            (if-let [n (nnext s)]
              (recur new-m n)
              (persistent! new-m))))))))

(comment
  (as-map nil)
  (as-map [])
  (as-map ["a" "A" "b" "B" "c" "C"] :keywordize
    (fn [k v] (case k (:a :b) (str "boo-" v) v))))

(defn into-all "Like `into` but supports multiple \"from\"s."
  ([to from] (into to from))
  ([to from & more] (reduce into (into to from) more)))

(defn interleave-all
  "Greedy version of `interleave`.
  Ref. https://groups.google.com/d/msg/clojure/o4Hg0s_1Avs/rPn3P4Ig6MsJ"
  ([]   '())
  ([c1] (lazy-seq c1))
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

(comment
  (distinctv        [[:a 1] [:a 1] [:a 2] [:b 1] [:b 3]])
  (distinctv second [[:a 1] [:a 1] [:a 2] [:b 1] [:b 3]])
  (qb 10000
    (distinctv       [:a :a :b :c :d :d :e :a :b :c :d])
    (doall (distinct [:a :a :b :c :d :d :e :a :b :c :d]))
    (set             [:a :a :b :c :d :d :e :a :b :c :d])))

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

(defmacro ^:also-cljs repeatedly-into*
  [coll n & body]
  `(let [coll# ~coll
         n#    ~n]
     (if #+clj  (instance?   clojure.lang.IEditableCollection coll#)
         #+cljs (implements? IEditableCollection              coll#)
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

;;;; Strings

#+clj (def format clojure.core/format) ; For easier encore/format portability
#+cljs
(do
  (defn- undefined->nil [x] (if (undefined? x) nil x))
  (defn format "Removed from cljs.core 0.0-1885, Ref. http://goo.gl/su7Xkj"
    [fmt & args] (apply gstr/format fmt (map undefined->nil args))))

(defn substr
  "Gives a consistent, flexible, cross-platform substring API with support for:
    * Clamping of indexes beyond string limits.
    * Negative indexes: [   0   |   1   |  ...  |  n-1  |   n   ) or
                        [  -n   | -n+1  |  ...  |  -1   |   0   ).
                        (start index inclusive, end index exclusive).

  Note that `max-len` was chosen over `end-idx` since it's less ambiguous and
  easier to reason about - esp. when accepting negative indexes."
  [s start-idx & [max-len]]
  {:pre [(have? [:or nil? nneg-int?] max-len)]}
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
;;   [s] {:pre [(have? string? s)]}
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

(defn uuid-str
  "Returns a UUIDv4 string of form \"xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx\",
  Ref. http://www.ietf.org/rfc/rfc4122.txt,
       https://gist.github.com/franks42/4159427"
  ([]
     #+clj (str (java.util.UUID/randomUUID))
     #+cljs
     (let [fs (fn [n] (apply str (repeatedly n (fn [] (.toString (rand-int 16) 16)))))
           g  (fn [] (.toString (bit-or 0x8 (bit-and 0x3 (rand-int 15))) 16))
           sb (.append (goog.string.StringBuffer.)
                (fs 8) "-" (fs 4) "-4" (fs 3) "-" (g) (fs 3) "-" (fs 12))]
       ;;(UUID. sb) ; Equality fails on roundtrips
       (.toString sb)))
  ([max-length] (substr (uuid-str) 0 max-length)))

(comment (uuid-str 5))

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
        (if (kw-identical? arg1 :mem/del)
          (do (if (kw-identical? (first argn) :mem/all)
                (reset! cache {})
                (swap!  cache dissoc argn))
              nil)
          (let [fresh? (kw-identical? arg1 :mem/fresh)
                args   (if fresh? argn args)]
            @(swap-val! cache args
               (fn [?dv] (if (and ?dv (not fresh?)) ?dv
                           (delay (apply f args))))))))))

  ([ttl-ms f] ; De-raced, commands, ttl, gc
     (have? [:or nil? pos-int?] ttl-ms)
     (let [cache (atom {})] ; {<args> <[delay-val udt :as cache-val]>}
      (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
        (if (kw-identical? arg1 :mem/del)
          (do (if (kw-identical? (first argn) :mem/all)
                (reset! cache {})
                (swap!  cache dissoc argn))
              nil)

          (do
            (when (<= (rand) gc-rate) ; GC
              (let [instant (now-udt)]
                (swap! cache
                  (fn [m] (reduce-kv (fn [m* k [dv udt :as cv]]
                                      (if (> (- instant udt) ttl-ms) m*
                                          (assoc m* k cv))) {} (clj1098 m))))))

            (let [fresh?  (kw-identical? arg1 :mem/fresh)
                  args    (if fresh? argn args)
                  instant (now-udt)
                  [dv]    (swap-val! cache args
                            (fn [?cv]
                              (if (and ?cv (not fresh?)
                                    (let [[_dv udt] ?cv]
                                      (< (- instant udt) ttl-ms))) ?cv
                                      [(delay (apply f args)) instant])))]
              @dv))))))

  ([cache-size ttl-ms f] ; De-raced, commands, ttl, gc, max-size
    (have? [:or nil? pos-int?] ttl-ms)
    (have? pos-int? cache-size)
    (let [state (atom {:tick 0})] ; {:tick _
                                  ;  <args> <[dval ?udt tick-lru tick-lfu :as cval]>}
      (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
        (if (kw-identical? arg1 :mem/del)
          (do (if (kw-identical? (first argn) :mem/all)
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
                                                (assoc m* k cv))) {} (clj1098 m*)))
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

            (let [fresh?   (kw-identical? arg1 :mem/fresh)
                  args     (if fresh? argn args)
                  ?instant (when ttl-ms (now-udt))
                  tick'    (:tick @state) ; Accuracy/sync irrelevant
                  [dv]     (swap-val! state args
                             (fn [?cv]
                               (if (and ?cv (not fresh?)
                                     (or (nil? ?instant)
                                       (let [[_dv udt] ?cv]
                                         (< (- ?instant udt) ttl-ms)))) ?cv
                                         [(delay (apply f args)) ?instant tick' 1])))]

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

  (qb 10000 (f0) (f1) (f2) (f3) (f4)) ; [1.094 2.535 5.397 6.254 7.678]

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
                          (assoc m* id [udt-window-start ncalls]))) {}
                          (clj1098 m))]))))

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

(def nano-time
  #+clj (fn [] (System/nanoTime)) ; Since Unix Epoch
  #+cljs ; Since window context, etc., Ref. http://goo.gl/mWZWnR
  (if-let [perf (aget js/window "performance")]
    ;; Ref. http://goo.gl/fn84us
    (if-let [f (or (aget perf "now")  (aget perf "mozNow") (aget perf "msNow")
                   (aget perf "oNow") (aget perf "webkitNow"))]
      (fn [] (long (* 1000 (.call f perf))))
      (fn [] (* 1000 (now-udt))))
    (fn [] (* 1000 (now-udt)))))

(defmacro ^:also-cljs time-ms
  "Returns number of milliseconds it takes to execute body."
  [& body] `(let [t0# (now-udt)] ~@body (- (now-udt) t0#)))

(defmacro ^:also-cljs time-ns "Returns number of nanoseconds it takes to execute body."
  [& body] `(let [t0# (nano-time)] ~@body (- (nano-time) t0#)))

(defmacro ^:also-cljs qbench
  "Quick bench. Returns fastest of 3 sets of lap times for each form, in msecs."
  ([nlaps form]
     `(let [times# (for [_# [1 2 3]] (time-ns (dotimes [_# (have integer? ~nlaps)]
                                                (do ~form))))]
        (round2 (/ (apply min times#) 1000000.0))))
  ([nlaps form & more]
     (mapv (fn [form] `(qbench ~nlaps ~form)) (cons form more))))

(defmacro qb [& args] `(qbench ~@args)) ; Alias

(comment (qb 2     (Thread/sleep 100))
         (qb 10000 (first [1 2 3 4 5]) (nth [1 2 3 4 5] 0)))

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
    (catch Throwable t (format "DNF: %s" (.getMessage t)))))

(defmacro bench [nlaps bench*-opts & body]
  `(bench* ~nlaps ~bench*-opts (fn [] ~@body)))

;;;; Client misc

#+cljs
(do ; Logging stuff

  (defn log [x]
    ;; (undefined? (aget "console" js/window))
    (if (js* "typeof console != 'undefined'")
      (.log js/console x)
      (js/print x))
    nil)

  (defn sayp [& xs]     (js/alert (str/join " " xs)))
  (defn sayf [fmt & xs] (js/alert (apply format fmt xs)))
  (defn logp [& xs]     (log (str/join " " xs)))
  (defn logf [fmt & xs] (log (apply format fmt xs)))

  ;;; Simplified logging stuff borrowed from Timbre
  (def logging-level "Log only >= <this-level> calls" (atom :debug))
  (def logging-level-sufficient?
    (let [ordered-levels [#_nil :trace :debug :info :warn :error :fatal :report]
          scored-levels  (zipmap ordered-levels (next (range)))
          valid-level?   (set ordered-levels)]
      (fn [level]
        (let [current-level @logging-level]
          (>= (scored-levels (encore-macros/have valid-level? level))
              (scored-levels (encore-macros/have valid-level? current-level)))))))

  (comment (logging-level-sufficient? :debug)
           (logging-level-sufficient? :invalid))

  (def ^:private lls? logging-level-sufficient?)
  ;;; Note current lack of macros, [?throwable fmt & xs] support:
  (defn tracef  [fmt & xs] (when (lls? :trace)  (apply logf fmt xs)))
  (defn debugf  [fmt & xs] (when (lls? :debug)  (apply logf fmt xs)))
  (defn infof   [fmt & xs] (when (lls? :info)   (apply logf fmt xs)))
  (defn warnf   [fmt & xs] (when (lls? :warn)   (str "WARN: "  (apply logf fmt xs))))
  (defn errorf  [fmt & xs] (when (lls? :error)  (str "ERROR: " (apply logf fmt xs))))
  (defn fatalf  [fmt & xs] (when (lls? :fatal)  (str "FATAL: " (apply logf fmt xs))))
  (defn reportf [fmt & xs] (when (lls? :report) (apply logf fmt xs))))

#+cljs
(defn get-window-location
  "Returns browser window's current location. Forgeable."
  []
  (let [loc* (.-location js/window)
        loc
        {;; Ref. http://bl.ocks.org/abernier/3070589
         :href     (.-href     loc*) ; "http://www.example.org:80/foo/bar?q=baz#bang"
         :protocol (.-protocol loc*) ; "http:" ; Note the :
         :hostname (.-hostname loc*) ; "example.org"
         :host     (.-host     loc*) ; "example.org:80"
         :pathname (.-pathname loc*) ; "/foo/bar"
         :search   (.-search   loc*) ; "?q=baz"
         :hash     (.-hash     loc*) ; "#bang"
         }]
    loc))

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
  [uri method params] {:pre [(have? [:or nil? map?] params)]}
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
  Simple+lightweight Ajax via Google Closure. Returns nil, or the xhr instance.
  Ref. https://developers.google.com/closure/library/docs/xhrio.

  (ajax-lite \"/my-post-route\"
    {:method     :post
     :params     {:username \"Rich Hickey\"
                  :type     \"Awesome\"}
     :headers    {\"Foo\" \"Bar\"}
     :resp-type  :text
     :timeout-ms 7000}
    (fn async-callback [resp-map]
      (let [{:keys [?status ?error ?content ?content-type]} resp-map]
        ;; ?status - 200, 404, ..., or nil on no response
        ;; ?error  - e/o #{:xhr-pool-depleted :exception :http-error :abort
        ;;                 :timeout <http-error-status> nil}
        (js/alert (str \"Ajax response: \" resp-map)))))"
  [uri {:keys [method params headers timeout-ms resp-type] :as opts
        :or   {method :get timeout-ms 10000 resp-type :auto}}
   callback]
  {:pre [(have? [:or nil? nneg-int?] timeout-ms)]}
  (if-let [xhr (get-pooled-xhr!)]
    (try
      (let [timeout-ms (or (:timeout opts) timeout-ms) ; Deprecated opt
            method*    (case method :get "GET" :post "POST")
            params     (map-keys name params)
            headers    (merge {"X-Requested-With" "XMLHTTPRequest"}
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
              (let [status        (.getStatus xhr) ; -1 when no resp
                    ?http-status  (when (not= status -1) status)
                    ?content-type (when ?http-status
                                    (.getResponseHeader xhr "Content-Type"))
                    ?content
                    (when ?http-status
                      (let [resp-type
                            (if-not (= resp-type :auto) resp-type
                              (condp #(str-contains? %2 %1)
                                  (str ?content-type) ; Prevent nil
                                "/edn"  :edn
                                "/json" :json
                                "/xml"  :xml
                                "/html" :text ; :xml only for text/xml!
                                :text))]
                        (try
                          (case resp-type
                            :text (.getResponseText xhr)
                            :json (.getResponseJson xhr)
                            :xml  (.getResponseXml  xhr)
                            :edn  (edn/read-string (.getResponseText xhr)))
                          (catch :default e
                            ;; Undocumented, subject to change:
                            {:ajax/bad-response-type resp-type
                             :ajax/resp-as-text (.getResponseText xhr)}))))

                    cb-arg
                    {;;; Raw stuff
                     :raw-resp resp
                     :xhr      xhr ; = (.-target resp)
                     ;;;
                     :?content-type (when ?http-status ?content-type)
                     :?content ?content
                     :?status  ?http-status
                     :?error
                     (or
                       (if ?http-status
                         (when-not (<= 200 ?http-status 299) ?http-status)
                         (get { ;; goog.net.ErrorCode/NO_ERROR nil
                               goog.net.ErrorCode/EXCEPTION  :exception
                               goog.net.ErrorCode/HTTP_ERROR :http-error
                               goog.net.ErrorCode/ABORT      :abort
                               goog.net.ErrorCode/TIMEOUT    :timeout}
                           (.getLastErrorCode xhr) :unknown))
                       (when (nil? ?content) :no-content))}]
                (callback cb-arg))))

          (.setTimeoutInterval (or timeout-ms 0)) ; nil = 0 = no timeout
          (.send uri* method* post-content* headers*))

        ;; Allow aborts, etc.:
        xhr)

      (catch js/Error e
        (errorf "`ajax-lite` error: %s" e)
        (.releaseObject @xhr-pool_ xhr)
        nil))

    (do ; Pool failed to return an available xhr instance
      (callback {:?error :xhr-pool-depleted})
      nil)))

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

;;;; DEPRECATED

;; Used by Carmine <= v2.7.0
(defmacro ^:also-cljs repeatedly* [n & body] `(repeatedly-into* [] ~n ~@body))

;; Used by Sente <= v1.1.0
#+cljs (defn set-exp-backoff-timeout! [nullary-f & [nattempt]]
         (.setTimeout js/window nullary-f (exp-backoff (or nattempt 0))))

;; Arg order changed for easier partials
(defn keys=      [m ks] (ks=      ks m))
(defn keys<=     [m ks] (ks<=     ks m))
(defn keys>=     [m ks] (ks>=     ks m))
(defn keys=nnil? [m ks] (ks-nnil? ks m))
