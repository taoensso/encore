(ns taoensso.encore
  "Core utils library for Clojure/Script"
  {:author "Peter Taoussanis (@ptaoussanis)"}
  #+clj  (:refer-clojure :exclude (format))
  #+clj  (:require [clojure.string  :as str]
                   [clojure.set     :as set]
                   [clojure.java.io :as io]
                   ;; [clojure.core.async    :as async]
                   [clojure.tools.reader.edn :as edn]
                   [taoensso.truss :as truss])
  #+clj  (:import  [java.util Date Locale TimeZone]
                   [java.text SimpleDateFormat]
                   ;; [org.apache.commons.codec.binary Base64]
                   )
  #+cljs (:require [clojure.string      :as str]
                   [clojure.set         :as set]
                   ;; [cljs.core.async  :as async]
                   [cljs.reader]
                   [cljs.tools.reader.edn :as edn]
                   ;;[goog.crypt.base64 :as base64]
                   [goog.object         :as gobj]
                   [goog.string         :as gstr]
                   [goog.string.format]
                   [goog.string.StringBuffer]
                   [goog.events         :as gevents]
                   [goog.net.XhrIo      :as gxhr]
                   [goog.net.XhrIoPool  :as gxhr-pool]
                   [goog.Uri.QueryData  :as gquery-data]
                   [goog.structs        :as gstructs]
                   [goog.net.EventType]
                   [goog.net.ErrorCode]
                   [taoensso.truss :as truss])
  #+cljs
  (:require-macros
   [taoensso.encore :as enc-macros :refer
    (compile-if catch-errors* catch-errors have have! have?
     name-with-attrs -vol! -vol-reset! -vol-swap!)]))

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *unchecked-math* false))

;;;; Truss aliases (for back compatibility, convenience)

(defmacro have   [& sigs] `(taoensso.truss/have   ~@sigs))
(defmacro have!  [& sigs] `(taoensso.truss/have!  ~@sigs))
(defmacro have?  [& sigs] `(taoensso.truss/have?  ~@sigs))
(defmacro have!? [& sigs] `(taoensso.truss/have!? ~@sigs))
(defn      get-dynamic-assertion-data [] (truss/get-dynamic-assertion-data))
(defmacro with-dynamic-assertion-data [& sigs]
  `(taoensso.truss/with-dynamic-assertion-data ~@sigs))

;;;; Version check (helps with dependency conflicts, etc.)

(declare as-?int)
(def             encore-version [2 48 1])
(defn assert-min-encore-version [min-version]
  (let [[xc yc zc] encore-version
        [xm ym zm] (if (vector? min-version) min-version (re-seq #"\d+" (str min-version)))
        [xm ym zm] (mapv #(or (as-?int %) 0) [xm ym zm])]

    (when-not (or (> xc xm) (and (= xc xm) (or (> yc ym) (and (= yc ym) (>= zc zm)))))
      (throw
        (ex-info (str "Insufficient `com.taoensso/encore` version. You may have a Leiningen dependency conflict (see http://goo.gl/qBbLvC for solution).")
          {:min-version  (str/join "." [xm ym zm])
           :your-version (str/join "." [xc yc zc])})))))

(comment (assert-min-encore-version 3.10))

;;;; Core macros

(defmacro compile-if
  "Evaluates `test`. If it doesn't error and returns logical true, expands to
  `then`, otherwise expands to `else`. Stolen from `clojure.core.reducers`.
  (compile-if (Class/forName \"java.util.concurrent.ForkJoinTask\")
    (do-cool-stuff-with-fork-join)
    (fall-back-to-executor-services))"
  [test then & [else]]
  (if (try (eval test) (catch Throwable _ false))
    `(do ~then)
    `(do ~else)))

(defmacro if-cljs
  "Executes `then` clause iff generating ClojureScript code.
  Useful for writing macros that can produce different Clj/Cljs code (this isn't
  something Cljx currently provides support for). Stolen from Prismatic code,
  Ref. http://goo.gl/DhhhSN, http://goo.gl/Bhdyna."
  [then else]
  (if (:ns &env) ; nil when compiling for Clojure, nnil for ClojureScript
    then else))

(defn name-with-attrs
  "Given a name symbol and sigs, returns [<name-with-attrs-meta> <args>]"
  ([sym             sigs] (name-with-attrs sym nil sigs))
  ([sym attrs-merge sigs]
   (let [[?docstring sigs] (if (string? (first sigs)) [(first sigs) (next sigs)] [nil sigs])
         [attrs      sigs] (if (map?    (first sigs)) [(first sigs) (next sigs)] [{}  sigs])
         attrs (if ?docstring (assoc attrs :doc ?docstring) attrs)
         attrs (if (meta sym) (conj (meta sym) attrs) attrs)
         attrs (conj attrs attrs-merge)]
     [(with-meta sym attrs) sigs])))

(defmacro defonce*
  "Like `defonce` but uses `name-with-attrs` to support optional docstring
  and attrs map for name symbol"
  {:arglists '([sym expr])}
  [sym & sigs]
  (let [[sym body] (name-with-attrs sym sigs)]
    `(defonce ~sym ~@body)))

(defmacro declare-remote
  "Declares the given ns-qualified name symbols, preserving symbol metadata.
  Useful for circular dependencies."
  [& syms]
  (let [original-ns (str *ns*)]
    `(do ~@(map (fn [s]
                  (let [ns (namespace s)
                        v  (name      s)
                        m  (meta      s)]
                    `(do (in-ns  '~(symbol ns))
                         (declare ~(with-meta (symbol v) m))))) syms)
         (in-ns '~(symbol original-ns)))))

(defmacro defalias
  "Defines an alias for a var, preserving metadata. Adapted from
  `clojure.contrib/def.clj`, Ref. http://goo.gl/xpjeH"
  ;; TODO Would be nice to have a ClojureScript impln. (impossible?)
  ([    target          ] `(defalias ~(symbol (name target)) ~target nil))
  ([sym target          ] `(defalias ~sym                    ~target nil))
  ([sym target docstring]
   `(let [^clojure.lang.Var v# (var ~target)]
      (alter-meta! (def ~sym (.getRawRoot v#))
        #(merge % (dissoc (meta v#) :column :line :file :test :name)
           (when-let [doc# ~docstring] {:doc doc#})))
      (var ~sym))))

(defmacro cond! "Like `cond` but throws on no-match like `case` and `condp`"
  [& clauses] `(cond ~@clauses :else (throw (ex-info "No matching clause" {}))))

(comment [(cond false "false") (cond! false "false")])

(defmacro doto-cond "Diabolical cross between `doto`, `cond->` and `as->`"
  [[sym x] & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test-expr step]] `(when-let [~sym ~test-expr] (-> ~g ~step)))]
    `(let [~g ~x]
       ~@(map pstep (partition 2 clauses))
       ~g)))

(defmacro case-eval
  "Like `case` but evals test constants for their compile-time value"
  [expr & clauses]
  (let [;; Don't evaluate default expression!
        default (when (odd? (count clauses)) (last clauses))
        clauses (if default (butlast clauses) clauses)]
    `(case ~expr
       ~@(map-indexed (fn [i# form#] (if (even? i#) (eval form#) form#)) clauses)
       ~(when default default))))

(defmacro if-lets
  "Like `if-let` but binds multiple values iff all tests are true"
  ([bindings then] `(if-lets ~bindings ~then nil))
  ([bindings then else]
   (let [[b1 b2 & bnext] bindings]
     (if bnext
       `(if-let [~b1 ~b2] (if-lets ~(vec bnext) ~then ~else) ~else)
       `(if-let [~b1 ~b2] ~then ~else)))))

(defmacro when-lets
  "Like `when-let` but binds multiple values iff all tests are true"
  [bindings & body]
  (let [[b1 b2 & bnext] bindings]
    (if bnext
      `(when-let [~b1 ~b2] (when-lets ~(vec bnext) ~@body))
      `(when-let [~b1 ~b2] ~@body))))

(comment
  (if-lets   [a :a b (= a :a)] [a b] "else")
  (if-lets   [a :a b (= a :b)] [a b] "else")
  (when-lets [a :a b nil] "true"))

(defmacro do-nil   [& body] `(do ~@body nil))
(defmacro do-false [& body] `(do ~@body false))
(defmacro do-true  [& body] `(do ~@body true))

;;;; Edn

(declare map-keys kw-identical?)

(defn read-edn
  "Experimental. Attempts to pave over differences in:
    `clojure.edn/read-string`, `clojure.tools.edn/read-string`,
    `cljs.reader/read-string`, `cljs.tools.reader/read-string`.
   `cljs.reader` in particular can be a bit of a pain."

  ([     s] (read-edn nil s))
  ([opts s]
   ;; First normalize behaviour for unexpected inputs:
   (if (or (nil? s) (identical? s ""))
     nil
     (if-not (string? s)
       (throw (ex-info "`read-edn` attempt against non-nil, non-string arg"
                {:arg s :type (type s)}))

       (let [readers (get opts :readers ::dynamic)
             default (get opts :default ::dynamic)

             ;; Nb we ignore as implementation[1] detail:
             ;;  *.tools.reader/*data-readers*,
             ;;  *.tools.reader/default-data-reader-fn*
             ;;
             ;; [1] Lib consumer doesn't care that we've standardized to
             ;;     using tools.reader under the covers

             readers
             (if-not (kw-identical? readers ::dynamic)
               readers
               #+clj  clojure.core/*data-readers*
               ;; Unfortunate (slow), but faster than gc'd memoization in most cases:
               #+cljs (map-keys symbol @cljs.reader/*tag-table*))

             default
             (if-not (kw-identical? default ::dynamic)
               default
               #+clj  clojure.core/*default-data-reader-fn*
               #+cljs @cljs.reader/*default-data-reader-fn*)

             opts (merge opts {:readers readers
                               :default default})]

         #+clj  (clojure.tools.reader.edn/read-string opts s)
         #+cljs (cljs.tools.reader.edn/read-string    opts s))))))

(defn pr-edn
  ([      x] (pr-edn nil x))
  ([_opts x] ; Opts currently unused
   #+cljs (binding [*print-level* nil, *print-length* nil] (pr-str x))
   #+clj
   (let [sw (java.io.StringWriter.)]
     (binding [*print-level* nil, *print-length* nil, *out* sw]
       (pr x) ; Avoid `pr-str`'s `apply` call
       (str sw)))))

;;;; Type preds, etc.

(compile-if (do (require 'clojure.core.async) true)
  (defn chan? [x]
    #+clj  (instance? clojure.core.async.impl.channels.ManyToManyChannel x)
    #+cljs (instance?    cljs.core.async.impl.channels.ManyToManyChannel x))

  ;; nil to help distinguish from negative `instance?` test:
  (defn chan? [x] nil))

(defn named? [x]
  #+cljs (implements? INamed             x)
  #+clj  (instance?   clojure.lang.Named x))

(defn editable? [coll]
  #+cljs (implements? IEditableCollection              coll)
  #+clj  (instance?   clojure.lang.IEditableCollection coll))

(defn derefable? [x]
  #+cljs (satisfies? IDeref              x)
  #+clj  (instance?  clojure.lang.IDeref x))

#+clj (defn throwable? [x] (instance? Throwable x))
#+clj (defn exception? [x] (instance? Exception x))

(defn    stringy? [x] (or (keyword? x) (string? x)))
(defn      error? [x] (instance? #+cljs js/Error  #+clj Throwable          x))
(defn       atom? [x] (instance? #+cljs Atom      #+clj clojure.lang.Atom  x))
(defn   lazy-seq? [x] (instance? #+cljs LazySeq   #+clj clojure.lang.LazySeq    x))
(defn re-pattern? [x] (instance? #+cljs js/RegExp #+clj java.util.regex.Pattern x))
(defn       nnil? [x] (not (nil? x))) ; Same as `some?` in Clojure v1.6+
(defn     nblank? [x] (not (str/blank? x)))
(defn       nneg? [x] (not (neg? x)))
(defn    pos-int? [x] (and (integer? x) (pos? x)))
(defn       pint? [x] (and (integer? x) (pos? x)))
(defn   nneg-int? [x] (and (integer? x) (not (neg? x))))
(defn       uint? [x] (and (integer? x) (not (neg? x)))) ; Alias
(defn        udt? [x] (and (integer? x) (not (neg? x)))) ; Alias
(defn       vec2? [x] (and (vector?  x) (= (count x) 2)))
(defn       vec3? [x] (and (vector?  x) (= (count x) 3)))
(defn nblank-str? [x] (and (string?  x) (not (str/blank? x))))
(defn   nneg-num? [x] (and (number?  x) (not (neg? x))))
(defn    pos-num? [x] (and (number?  x) (pos? x)))
(defn   zero-num? [x] (= 0 x)) ; Unlike `zero?`, works on non-nums
(defn       pval? [x] (and (number? x) (let [n (double x)] (and (>= n 0.0) (<= n 1.0)))))

(do
  (def sentinel #+clj (Object.) #+cljs (js-obj))
  #+cljs (defn sentinel? [x] (identical? x sentinel))
  #+clj  (defn sentinel?
           {:inline (fn [x] `(. clojure.lang.Util identical ~x sentinel))
            :inline-arities #{1}}
           [x] (identical? x sentinel))
  (defn nil->sentinel [x] (if (nil? x) sentinel x))
  (defn sentinel->nil [x] (if (sentinel? x) nil x)))

(defn force-ref "Like `force` for refs" [x] (if (derefable? x) (deref x) x))

#+cljs
(def js-?win "May not be available with Node.js, etc."
  (when (exists? js/window) js/window))

;;;; Errors

(defn error-data
  "Returns data map iff `x` is an error of any type on platform"
  ;; Note that Clojure 1.7+ now has `Throwable->map`
  [x]
  (when-let [data-map
             (or (ex-data x) ; ExceptionInfo
               #+clj  (when (instance? Throwable x) {})
               #+cljs (when (instance? js/Error  x) {}))]
    (merge
      #+clj  (let [^Throwable t x] ; (catch Throwable t <...>)
               {:err-type   (type                 t)
                :err-msg    (.getLocalizedMessage t)
                :err-cause  (.getCause            t)})
      #+cljs (let [err x] ; (catch :default t <...)
               {:err-type  (type      err)
                :err-msg   (.-message err)
                :err-cause (.-cause   err)})
      data-map)))

(comment
  (error-data (Throwable. "foo"))
  (error-data (Exception. "foo"))
  (error-data (ex-info    "foo" {:bar :baz})))

(defmacro catch-errors*
  ;; Badly need something like http://dev.clojure.org/jira/browse/CLJ-1293
  ;; TODO js/Error instead of :default as temp workaround for http://goo.gl/UW7773
  ([try-form] `(catch-errors* ~try-form ~'_ nil))
  ([try-form error-sym error-form]
   `(if-cljs
      (try ~try-form (catch js/Error  ~error-sym ~error-form))
      (try ~try-form (catch Throwable ~error-sym ~error-form))))
  ([try-form error-sym error-form finally-form]
   `(if-cljs
      (try ~try-form (catch js/Error  ~error-sym ~error-form) (finally ~finally-form))
      (try ~try-form (catch Throwable ~error-sym ~error-form) (finally ~finally-form)))))

(defmacro catch-errors "Returns [<?result> <?error>]"
  [& body] `(catch-errors* [(do ~@body) nil] e# [nil e#]))

(comment
  (catch-errors (zero? "9"))
  (macroexpand '(catch-errors* (do "foo") e e (println "finally")))
  (catch-errors* (do "foo") e e)
  (catch-errors* (/ 5 0)    e e))

(defmacro caught-error-data "Handy for error-throwing unit tests"
  [& body] `(catch-errors* (do ~@body nil) e# (error-data e#)))

(comment (caught-error-data (/ 5 0)))

;;;; Type coercions, etc.

;;; Clojure 1.7-alpha5+ now has similar behaviour in `vec`, `set`
(defn vec* [x] (if (vector? x) x (vec x)))
(defn set* [x] (if (set?    x) x (set x)))

;; ClojureScript keywords aren't `identical?` and Clojure doesn't have
;; `keyword-identical?`. This util helps alleviate the pain of writing
;; cross-platform code, Ref. http://goo.gl/be8CGP
#+cljs (def  kw-identical? keyword-identical?)
#+clj  (defn kw-identical?
         {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))
          :inline-arities #{2}}
         ([x y] (clojure.lang.Util/identical x y)))

(defn without-meta [x] (if (meta x) (with-meta x nil) x))
(defn merge-meta   [x m] (with-meta x (merge (meta x) m)))
(defn distinct-elements? [x] (or (set? x) (= (count x) (count (set* x)))))
(defn nnil=
  ([x y]        (and (nnil? x) (= x y)))
  ([x y & more] (and (nnil? x) (apply = x y more))))

(comment (nnil= :foo :foo nil))

(defn as-?nzero  [x] (when (number?  x) (if (zero? x)      nil x)))
(defn as-?nblank [x] (when (string?  x) (if (str/blank? x) nil x)))
(defn as-?kw     [x] (cond (keyword? x)       x  (string? x) (keyword x)))
(defn as-?name   [x] (cond (named?   x) (name x) (string? x)          x))
(defn as-?qname  [x]
  (cond
    (named?  x) (let [n (name x)] (if-let [ns (namespace x)] (str ns "/" n) n))
    (string? x) x))

(defn as-?int #_as-?long [x]
  (cond (number? x) (long x)
        (string? x)
        #+cljs (let [x (js/parseInt x 10)] (when-not (js/isNaN x) x))
        #+clj  (try (Long/parseLong x)
                    (catch NumberFormatException _
                      (try (long (Float/parseFloat x))
                           (catch NumberFormatException _ nil))))))

(defn as-?float #_as-?double [x]
  (cond (number? x) (double x)
        (string? x)
        #+cljs (let [x (js/parseFloat x)] (when-not (js/isNaN x) x))
        #+clj  (try (Double/parseDouble x)
                    (catch NumberFormatException _ nil))))

(defn as-?uint   [x] (when-let [n (as-?int   x)] (when-not (neg? ^long   n) n)))
(defn as-?udt    [x] (when-let [n (as-?int   x)] (when-not (neg? ^long   n) n))) ; Alias
(defn as-?pint   [x] (when-let [n (as-?int   x)] (when     (pos? ^long   n) n)))
(defn as-?ufloat [x] (when-let [n (as-?float x)] (when-not (neg? ^double n) n)))
(defn as-?pfloat [x] (when-let [n (as-?float x)] (when     (pos? ^double n) n)))
(defn as-?pval   [x] (when-let [^double f (as-?float x)]
                       (if (> f 1.0) 1.0 (if (< f 0.0) 0.0 f))))
(defn as-?bool   [x]
  (cond (nil?  x) nil
    (or (true? x) (false? x)) x
    (or (= x 0) (= x "false") (= x "FALSE") (= x "0")) false
    (or (= x 1) (= x "true")  (= x "TRUE")  (= x "1")) true))

;; Uses simple regex to test for basic "x@y.z" form:
(defn as-?email  [?s] (when ?s (re-find #"^[^\s@]+@[^\s@]+\.\S*[^\.]$" (str/trim ?s))))
(defn as-?nemail [?s] (when-let [email (as-?email ?s)] (str/lower-case email)))
(comment (mapv as-?nemail ["foo" "foo@" "foo@bar" "Foo@BAR.com"
                           "foo@@bar.com" "foo@bar.com." "foo.baz@bar.com"]))

(defn- try-pred [pred x] (catch-errors* (pred x) _ false))
(defn when?     [pred x] (when (try-pred pred x) x))
(defn is! "Cheaper `have!` that provides less diagnostic info"
  ([     x           ] (is! identity x nil)) ; Nb different to single-arg `have`
  ([pred x           ] (is! identity x nil))
  ([pred x fail-?data]
   (if (try-pred pred x)
     x
     (throw (ex-info (str "`is!` " (str pred) " failure against arg: "
                       (pr-str x))
              {:arg-val x :arg-type (type x) :fail-?data fail-?data})))))

(comment [(is! false) (when-let [n (when? nneg? (as-?int 37))] n)])

(defn -as-throw [as-name x]
  (throw (ex-info (str "`as-" (name as-name) "` failed against: `" (pr-str x) "`")
           {:arg x :type (type x)})))

(defn as-nzero          [x] (or (as-?nzero  x) (-as-throw :nzero  x)))
(defn as-nblank         [x] (or (as-?nblank x) (-as-throw :nblank x)))
(defn as-kw             [x] (or (as-?kw     x) (-as-throw :kw     x)))
(defn as-name           [x] (or (as-?name   x) (-as-throw :name   x)))
(defn as-qname          [x] (or (as-?qname  x) (-as-throw :qname  x)))
(defn as-bool           [x] (let [?b (as-?bool x)] (if-not (nil? ?b) ?b (-as-throw :bool x))))
(defn as-email          [x] (or (as-?email  x) (-as-throw :email  x)))
(defn as-nemail         [x] (or (as-?nemail x) (-as-throw :nemail x)))
(defn as-int      ^long [x] (or (as-?int    x) (-as-throw :int    x)))
(defn as-uint     ^long [x] (or (as-?uint   x) (-as-throw :uint   x)))
(defn as-udt      ^long [x] (or (as-?uint   x) (-as-throw :udt    x)))
(defn as-pint     ^long [x] (or (as-?pint   x) (-as-throw :pint   x)))
(defn as-float  ^double [x] (or (as-?float  x) (-as-throw :float  x)))
(defn as-ufloat ^double [x] (or (as-?ufloat x) (-as-throw :ufloat x)))
(defn as-pfloat ^double [x] (or (as-?pfloat x) (-as-throw :pfloat x)))
(defn as-pval   ^double [x] (or (as-?pval   x) (-as-throw :pval   x)))

;;;; Validation

(defmacro check-some
  "Returns first logical false/throwing expression (id/form), or nil"
  ([test & more] `(or ~@(map (fn [test] `(check-some ~test)) (cons test more))))
  ([test]
   (let [[error-id test] (if (vector? test) test [nil test])]
     `(let [[test# err#] (catch-errors ~test)]
        (when-not test# (or ~error-id '~test :check/falsey))))))

(defmacro check-all
  "Returns all logical false/throwing expressions (ids/forms), or nil"
  ([test] `(check-some ~test))
  ([test & more]
   `(let [errors# (filterv identity
                    [~@(map (fn [test] `(check-some ~test)) (cons test more))])]
      (not-empty errors#))))

(comment
  (check-some false [:bad-type (string? 0)] nil [:blank (str/blank? 0)])
  (check-all  false [:bad-type (string? 0)] nil [:blank (str/blank? 0)]))

;;;; Keywords

(defn explode-keyword [k] (str/split (as-qname k) #"[\./]"))
(comment (explode-keyword :foo.bar/baz))

(defn merge-keywords [ks & [no-slash?]]
  (let [parts (reduce (fn [acc in] (if in (into acc (explode-keyword in)) acc))
                [] ks)]
    (when-not (empty? parts)
      (if no-slash?
        (keyword (str/join "." parts))
        (let [ppop (pop parts)]
          (keyword (when-not (empty? ppop) (str/join "." ppop))
            (peek parts)))))))

(comment (merge-keywords [:foo.bar nil "d.e/k" :baz.qux/end nil] :no))

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

  (defn ba-split [^bytes ba ^long idx]
    (if (zero? idx)
      [nil ba]
      (let [s (alength ba)]
        (when (> s idx)
          [(java.util.Arrays/copyOf      ba idx)
           (java.util.Arrays/copyOfRange ba idx s)]))))

  (comment
    (String. (ba-concat (.getBytes "foo") (.getBytes "bar")))
    (let [[x y] (ba-split (.getBytes "foobar") 5)] [(String. x) (String. y)])))

;;;; Math

(defn approx=
  ([x y      ] (approx= x y 0.001))
  ([x y signf] (< (Math/abs (- (double x) (double y))) (double signf))))

(comment (qb 10000 (approx= 3.141592 (/ 22 7) 0.01)))

;; This must reflect to output correct long/double types:
(defn clamp [nmin nmax n] (if (< n nmin) nmin (if (> n nmax) nmax n)))

;;; These will pass primitives through w/o reflection
(defmacro <=*    [x y z]       `(let [y# ~y] (and (<= ~x y#) (<= y# ~z))))
(defmacro >=*    [x y z]       `(let [y# ~y] (and (>= ~x y#) (>= y# ~z))))
(defmacro <*     [x y z]       `(let [y# ~y] (and (<  ~x y#) (<  y# ~z))))
(defmacro >*     [x y z]       `(let [y# ~y] (and (>  ~x y#) (>  y# ~z))))
(defmacro min*   [n1 n2]       `(let [n1# ~n1 n2# ~n2] (if (> n1# n2#) n2# n1#)))
(defmacro max*   [n1 n2]       `(let [n1# ~n1 n2# ~n2] (if (< n1# n2#) n2# n1#)))
(defmacro clamp* [nmin nmax n] `(let [nmin# ~nmin nmax# ~nmax n# ~n]
                                  (if (< n# nmin#) nmin# (if (> n# nmax#) nmax# n#))))

(defn pow [n exp] (Math/pow n exp))
(defn abs [n]     (if (neg? n) (- n) n)) ; #+clj (Math/abs n) reflects
(defn round*
  ([             n] (round* :round nil n))
  ([type         n] (round* type   nil n))
  ([type nplaces n]
   (let [n        (double n)
         modifier (when nplaces (Math/pow 10.0 nplaces))
         n*       (if-not modifier n (* n ^double modifier))
         rounded
         (case type
           ;;; Note same API for both #+clj, #+cljs:
           :round (Math/round n*) ; Round to nearest int or nplaces
           :floor (Math/floor n*) ; Round down to -inf
           :ceil  (Math/ceil  n*) ; Round up to +inf
           :trunc (long n*)       ; Round up/down toward zero
           (throw (ex-info "Unknown round type" {:type type})))]
     (if-not modifier
       (long rounded)                        ; Returns long
       (/ (double rounded) ^double modifier) ; Returns double
       ))))

(comment
  (round* :floor -1.5)
  (round* :trunc -1.5)
  (round* :floor 5 1.1234567)
  (round* :round 5 1.1234567 ))

;;; Optimized common cases
(defn round0   ^long [n]            (Math/round    (double n)))
(defn round1 ^double [n] (/ (double (Math/round (* (double n)  10.0)))  10.0))
(defn round2 ^double [n] (/ (double (Math/round (* (double n) 100.0))) 100.0))

(defn exp-backoff "Returns binary exponential backoff value."
  [nattempt & [{:keys [factor] min' :min max' :max :or {factor 1000}}]]
  (let [binary-exp (double (Math/pow 2 (dec ^long nattempt)))
        time       (* (+ binary-exp ^double (rand binary-exp)) 0.5 (double factor))]
    (long (let [time (if min' (max (long min') (long time)) time)
                time (if max' (min (long max') (long time)) time)]
            time))))

(comment (exp-backoff 4))

;;;; Date & time

(defn   now-dt       [] #+clj (java.util.Date.) #+cljs (js/Date.))
(defn  now-udt ^long [] #+clj (System/currentTimeMillis) #+cljs (.getTime (js/Date.)))
(defn secs->ms ^long [secs] (* (long secs)  1000))
(defn ms->secs ^long [ms]   (quot (long ms) 1000))
(defn ms "Returns ~number of milliseconds in period defined by given args"
  [& {:as opts :keys [years months weeks days hours mins secs msecs ms]}]
  {:pre [(have #{:years :months :weeks :days :hours :mins :secs :msecs :ms}
           :in (keys opts))]}
  (round*
    (+
      (if years  (* (double years)  #=(* 1000 60 60 24 365))    0.0)
      (if months (* (double months) #=(* 1000 60 60 24 29.53))  0.0)
      (if weeks  (* (double weeks)  #=(* 1000 60 60 24 7))      0.0)
      (if days   (* (double days)   #=(* 1000 60 60 24))        0.0)
      (if hours  (* (double hours)  #=(* 1000 60 60))           0.0)
      (if mins   (* (double mins)   #=(* 1000 60))              0.0)
      (if secs   (* (double secs)   1000)                       0.0)
      (if msecs     (double msecs)                              0.0)
      (if ms        (double ms)                                 0.0))))

(def secs (comp ms->secs ms))
(comment #=(ms   :years 88 :months 3 :days 33)
         #=(secs :years 88 :months 3 :days 33))

(defmacro thread-local-proxy "Ref. http://goo.gl/CEBJnQ (instant.clj)"
  [& body] `(proxy [ThreadLocal] [] (initialValue [] (do ~@body))))

#+clj
(def ^:private -simple-date-format
  "Returns a SimpleDateFormat ThreadLocal proxy"
  (memoize
    (fn [^String pattern & [{:keys [locale timezone]}]]
      (let [^Locale locale
            (if (kw-identical? locale :jvm-default)
              (Locale/getDefault)
              locale)

            ^TimeZone timezone
            (case timezone
              :jvm-default (TimeZone/getDefault)
              :utc         (TimeZone/getTimeZone "UTC")
              timezone)]

        (thread-local-proxy
          (let [^SimpleDateFormat sdformat
                (if locale
                  (SimpleDateFormat. pattern locale)
                  (SimpleDateFormat. pattern))]
            (when timezone (.setTimeZone sdformat timezone))
            sdformat))))))

#+clj
(defn simple-date-format
  "Returns a thread-local `java.text.SimpleDateFormat`, Ref. http://goo.gl/Vh392A
  ~Prefer java.time (Java 8) > Joda-Time > Tower/DateFormat > SimpleDateFormat"
  ^java.text.SimpleDateFormat [pattern & [{:keys [locale timezone] :as opts}]]
  (let [pattern
        (case pattern
          :iso8601 "yyyy-MM-dd HH:mm:ss.SSSZ"
          :rss2 "EEE, dd MMM yyyy HH:mm:ss z"
          pattern)]
    (.get ^ThreadLocal (-simple-date-format pattern opts))))

(comment (qb 10000 (.format (simple-date-format "yyyy-MMM-dd") (Date.))))

;;;; Collections

#+cljs
(defn oget
  "Like `aget` for JS objects, Ref. https://goo.gl/eze8hY.
  Unlike `aget`, returns nil for missing keys instead of throwing."
  ([o k]          (when      o                 (gobj/get o k  nil)))
  ([o k1 k2]      (when-let [o (oget o k1)]    (gobj/get o k2 nil)))
  ([o k1 k2 & ks] (when-let [o (oget o k1 k2)] (apply oget o ks))))

(defn   singleton? [coll] (if (counted? coll) (= (count coll) 1) (not (next coll))))
(defn ->?singleton [coll] (when (singleton? coll) (let [[c1] coll] c1)))
(defn ->vec [x] (cond (vector? x) x (sequential? x) (vec x) :else [x]))

(defn vnext [v] (when (> (count v) 1) (subvec v 1)))
(defn vsplit-last  [v]
  (let [c (count v)] (when (> c 0) [(when (> c 1) (pop v)) (peek v)])))
(defn vsplit-first [v]
  (let [c (count v)] (when (> c 0) (let [[v1] v] [v1 (when (> c 1) (subvec v 1))]))))

(comment
  (vsplit-first [:a :b :c])
  (vsplit-last  [:a :b :c]))

(defn nnil-set [x] (disj (set* x) nil))
(defn conj-some
  ([             ] [])
  ([coll         ] coll)
  ([coll ?x      ] (if (nnil? ?x) (conj coll ?x) coll))
  ([coll ?x & ?xs] (reduce conj-some (conj-some coll ?x) ?xs)))

(comment [(nnil-set [:a :b nil]) (conj-some [] :a :b nil :c :d nil :e)])

(defn preserve-reduced "As `clojure.core/preserving-reduced`"
  [rf]
  (fn [acc in]
    (let [result (rf acc in)]
      (if (reduced? result)
        (reduced result)
        result))))

(defn run-kv! [proc    m] (reduce-kv #(proc %2 %3) nil    m) nil)
(defn run!*   [proc coll] (reduce    #(proc %2)    nil coll) nil)

(defn rsome "Faster `some` based on `reduce`"
  [pred coll] (reduce (fn [acc in] (when-let [p (pred in)] (reduced p))) nil coll))

(defn revery? "Faster `every?` based on `reduce`"
  [pred coll] (reduce (fn [acc in] (if (pred in) true (reduced nil))) true coll))

(defn every [pred coll]
  ;; Note that `(every? even? nil)` ≠ `(every even? nil)`
  (reduce (fn [acc in] (if (pred in) coll (reduced nil))) coll coll))

(comment [(every? even? nil) (every even? nil)])

(compile-if (completing (fn [])) ; Transducers
  (defn reduce-kvs
    "Like `reduce-kv` but takes a flat sequence of kv pairs"
    [rf init kvs]
    (transduce (partition-all 2)
      (completing (fn [acc [k v]] (rf acc k v))) init kvs))

  (defn reduce-kvs [rf init kvs]
    (reduce (fn [acc [k v]] (rf acc k v)) init (partition-all 2 kvs))))

(compile-if clojure.lang.LongRange ; Clojure 1.7+ (no Cljs support yet)
  (defn reduce-n [rf init ^long n] (reduce rf init (range n)))
  (defn reduce-n [rf init ^long n]
    (loop [acc init idx 0]
      (if (== idx n)
        acc
        (let [acc (rf acc idx)]
          (if (reduced? acc)
            @acc
            (recur acc (unchecked-inc idx))))))))

(comment (reduce-n conj [] 100))

(compile-if (completing (fn [])) ; Transducers
  (defn into!
       ([to       from] (reduce          conj! to from))
       ([to xform from] (transduce xform conj! to from)))
  (defn into! [to from] (reduce          conj! to from)))

(compile-if (completing (fn [])) ; Transducers
  (defn xdistinct
    ([] (distinct)) ; clojure.core now has a distinct transducer
    ([keyfn]
     (fn [rf]
       (let [seen_ (volatile! #{})]
         (fn
           ([]    (rf))
           ([acc] (rf acc))
           ([acc input]
            (let [k (keyfn input)]
              (if (contains? @seen_ k)
                acc
                (do (vswap! seen_ conj k)
                    (rf acc input)))))))))))

(comment (into [] (xdistinct) [1 2 3 1 4 5 2 6 7 1]))

(declare subvec*)
(compile-if (completing (fn [])) ; Transducers
  (defn takev [n coll] (if (vector? coll) (subvec* coll 0 n) (into [] (take n) coll)))
  (defn takev [n coll] (if (vector? coll) (subvec* coll 0 n) (vec (take n coll)))))

(defn into-all "Like `into` but supports multiple \"from\"s"
  ([to from       ]              (into to from))
  ([to from & more] (reduce into (into to from) more)))

(defn repeatedly-into
  "Like `repeatedly` but faster and `conj`s items into given collection"
  [coll ^long n f]
  (if (and (> n 10) (editable? coll))
    (persistent! (reduce-n (fn [acc _] (conj! acc (f))) (transient coll) n))
    (do          (reduce-n (fn [acc _] (conj  acc (f)))            coll  n))))

(comment (repeatedly-into [] 100 (partial rand-nth [1 2 3 4 5 6])))

;;; Note `mapv`-like nil->{} semantics
(defn map-vals       [f m] (if (nil? m) {} (reduce-kv (fn [m k v] (assoc m k (f v))) m m)))
(defn map-keys       [f m] (if (nil? m) {} (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m)))
(defn filter-kvs  [pred m] (if (nil? m) {} (reduce-kv (fn [m k v] (if (pred k v) m (dissoc m k))) m m)))
(defn filter-keys [pred m] (if (nil? m) {} (reduce-kv (fn [m k v] (if (pred k)   m (dissoc m k))) m m)))
(defn filter-vals [pred m] (if (nil? m) {} (reduce-kv (fn [m k v] (if (pred v)   m (dissoc m k))) m m)))
(defn remove-vals [pred m] (filter-vals (complement pred) m))
(defn remove-keys [pred m] (filter-keys (complement pred) m))
(defn remove-kvs  [pred m] (filter-kvs  (complement pred) m))

(defn ks=      [ks m] (=             (set (keys m)) (set* ks)))
(defn ks<=     [ks m] (set/subset?   (set (keys m)) (set* ks)))
(defn ks>=     [ks m] (set/superset? (set (keys m)) (set* ks)))
(defn ks-nnil? [ks m] (revery?     #(nnil? (get m %))     ks))

(comment
  (ks=      {:a :A :b :B  :c :C}  #{:a :b})
  (ks<=     {:a :A :b :B  :c :C}  #{:a :b})
  (ks>=     {:a :A :b :B  :c :C}  #{:a :b})
  (ks-nnil? {:a :A :b :B  :c nil} #{:a :b})
  (ks-nnil? {:a :A :b nil :c nil} #{:a :b}))

;; Recall: no `korks` support due to inherent ambiguous nil ([] vs [nil])
(defn update-in*
  "Like `update-in` but faster, more flexible, and simpler (less ambiguous)"
  [m ks f]
  (if (empty? ks)
    (f m) ; Resolve [] = [nil] ambiguity in `update-in`, `assoc-in`, etc.
    (let [[k & ks] ks]
      (if ks ; Avoid apply calls:
        (assoc m k (update-in* (get m k) ks f))
        (assoc m k (f          (get m k)))))))

(defn- translate-signed-idx ^long [^long signed-idx ^long max-idx]
  (if (>= signed-idx 0)
    (min      signed-idx max-idx)
    (max 0 (+ signed-idx max-idx))))

(comment (translate-signed-idx -3 5))

(defn sub-indexes
  "Returns [<inclusive-start-idx*> <exclusive-end-idx*>] for counted 0-indexed
  input (str, vec, etc.) with support for:
    * Clamping of indexes beyond limits.
    * Max-length -> end-index.
    * -ive indexes (as +ive indexes but work from back of input):
      (+0) (+1) (+2) (+3) (+4)  ; inclusive +ive indexes
        h    e    l    l    o   ; 5 count
      (-5) (-4) (-3) (-2) (-1)  ; inclusive -ive indexes"
  [x start-idx & {:keys [^long max-len ^long end-idx]}]
  {:pre  [(have? [:or nil? nneg-int?] max-len)]
   ;; Common out-of-bounds conds:
   ;; :post [(have? (fn [[s e]] (>= e s)) %)
   ;;        (have? (fn [[s e]] (>= s 0)) %)
   ;;        (have? (fn [[s e]] (<= e (count x))) %)]
   }
  (let [start-idx  ^long start-idx
        xlen       (count x) ; also = max-exclusive-end-idx
        start-idx* (translate-signed-idx start-idx xlen)
        end-idx*   (long
                     (cond
                       max-len (#+clj min* #+cljs enc-macros/min*
                                 (+ start-idx* max-len) xlen)
                       end-idx (inc ; Want exclusive
                                 (translate-signed-idx end-idx xlen))
                       :else   xlen))]
    (if (> start-idx* end-idx*)
      ;; [end-idx* start-idx*] ; Allow wrapping
      [0 0] ; Disallow wrapping
      [start-idx* end-idx*])))

(comment
  (sub-indexes "hello"  0 :max-len 5)  ; hello
  (sub-indexes "hello"  0 :max-len 9)  ; hello
  (sub-indexes "hello" -4 :max-len 5)  ; _ello
  (sub-indexes "hello"  2 :max-len 2)  ; __ll_
  (sub-indexes "hello" -2 :max-len 2)  ; ___lo
  (sub-indexes "hello" -2)             ; ___lo
  (sub-indexes "hello"  2)             ; __llo
  (sub-indexes "hello"  8)             ; _____
  (sub-indexes "hello"  9 :max-len 9)  ; _____
  (sub-indexes "hello"  0 :max-len 0)  ; _____
  (sub-indexes "hello"  3 :end-idx  1) ; _____
  (sub-indexes "hello"  3 :end-idx -1) ; ___lo
  )

(defn subvec* "Like `subvec` but uses `sub-indexes`"
  [v start-idx & [?max-len]] {:pre [(have? vector? v)]}
  (let [[start-idx* end-idx*] (sub-indexes v start-idx :max-len ?max-len)]
    (subvec v start-idx* end-idx*)))

(comment (subvec* [:a :b :c :d :e] -1))

(defn top
  "Returns a sorted vector of the top n items from coll of N items in O(N.logn)
  time. (take n (sort-by ...)) is O(N.logN) time, so much slower when n << N.
  Ref. http://stevehanov.ca/blog/index.php?id=122"
  ([n           coll] (top n identity compare coll))
  ([n keyfn     coll] (top n keyfn    compare coll))
  ([n keyfn cmp coll]
   ;; TODO Real cljs impl:
   #+cljs (into [] (take n) (sort-by keyfn cmp coll)) ; Requires transducers
   #+clj
   (let [coll-size (count coll)
         n         (long (min coll-size (long n)))]
     (if-not (pos? n)
       []
       (let [^java.util.PriorityQueue pq
             (java.util.PriorityQueue. coll-size
               (if (= keyfn identity)
                 (fn [x y] (cmp (sentinel->nil x) (sentinel->nil y)))
                 (fn [x y] (cmp
                            (keyfn (sentinel->nil x))
                            (keyfn (sentinel->nil y))))))]

         (run!* #(.add pq (nil->sentinel %)) coll)
         (repeatedly-into [] n #(sentinel->nil (.poll pq))))))))

(comment
  (top 0 [])
  (top 3 [])
  (top 20 [2 3 5 3 88 nil])
  (sort   [2 3 5 3 88 nil])
  (top 20 - [2 3 5 3 88])
  (let [c (repeatedly 5000 rand)]
    (qb 100 (into [] (take 2) (sort-by - c)) (top 2 - c))))

(defn contains-in? [coll ks] (contains? (get-in coll (butlast ks)) (last ks)))
(defn dissoc-in
  ([m ks dissoc-k]        (update-in* m ks (fn [m]       (dissoc m dissoc-k))))
  ([m ks dissoc-k & more] (update-in* m ks (fn [m] (apply dissoc m dissoc-k more)))))

(comment
  (dissoc-in    {:a {:b {:c :C :d :D :e :E}}} [:a :b] :c :e)
  (contains-in? {:a {:b {:c :C :d :D :e :E}}} [:a :b :c])
  (contains-in? {:a {:b {:c :C :d :D :e :E}}} [:a]))

(defn assoc-some "Assocs each kv iff its value is not nil"
  ([m k v      ] (if (nil? v) (if (nil? m) {} m) (assoc m k v)))
  ([m k v & kvs] (reduce-kvs (fn [acc k v] (assoc-some acc k v))
                   (assoc-some m k v) kvs)))

(defn assoc-when "Assocs each kv iff its val is truthy"
  ([m k v      ] (if-not v (if (nil? m) {} m) (assoc m k v)))
  ([m k v & kvs] (reduce-kvs (fn [acc k v] (assoc-when acc k v))
                   (assoc-when m k v) kvs)))

(comment (assoc-some {:a :A} :b nil :c :C :d nil :e :E))

(defn queue? [x]
  #+clj  (instance? clojure.lang.PersistentQueue x)
  #+cljs (instance? cljs.core.PersistentQueue    x))

(defn queue "Returns a PersistentQueue"
  ([coll] (into (queue) coll))
  ([] #+clj  clojure.lang.PersistentQueue/EMPTY
      #+cljs cljs.core.PersistentQueue.EMPTY))

(defn queue* [& items] (queue items))

(def seq-kvs "(seq-kvs {:a :A}) => (:a :A)" (partial reduce concat))
(defn mapply "Like `apply` but calls `seq-kvs` on final arg"
  [f & args] (apply f (concat (butlast args) (seq-kvs (last args)))))

(comment [(seq-kvs {:a :A :b :B}) (mapply str 1 2 3 {:a :A})])

(defn fzipmap "Faster `zipmap` using transients" [ks vs]
  (loop [m  (transient {})
         ks (seq ks)
         vs (seq vs)]
    (if-not (and ks vs)
      (persistent! m)
      (recur (assoc! m (first ks) (first vs))
        (next ks)
        (next vs)))))

(comment (let [kvs (range 100)] (qb 100 (zipmap kvs kvs) (fzipmap kvs kvs))))

(defn interleave-all
  "Greedy version of `interleave`, Ref. http://goo.gl/KvzqWb"
  ([] '())
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

#+cljs (defn rcompare "Reverse comparator" [x y] (compare y x))
#+clj  (defn rcompare "Reverse comparator"
         {:inline (fn [x y] `(. clojure.lang.Util compare ~y ~x))}
         [x y] (compare y x))

(defn nested-merge-with [f & maps]
  (when (some identity maps)
    (let [merge-entry
          (fn [m e]
            (let [k (key e) rv (val e)]
              (if-not (contains? m k) ; No lv
                (assoc m k rv)
                (let [lv (get m k)]
                  (if (and (map? lv) (map? rv)
                        #_(or (map? rv) (nil? rv)))
                    ;; Stack limited (don't nest too deaply):
                    (assoc m k (nested-merge-with f lv rv))
                    (assoc m k (f lv rv)))))))
          merge2 (fn [m1 m2] (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(def nested-merge
  (partial nested-merge-with #_(fn [x y] y)
    ;; We'll mimic `merge` behaviour re: nils against maps:
    (fn [x y] (if (and (map? x) (nil? y)) x y))))

(comment
  (nested-merge
    {:a1 :A1 :b1 :B1  :c1 {:a2 :A2 :b2 {:a3 :A3 :b3 :B3}}}
    {        :b1 :B1* :c1 {        :b2 {        :b3 :B3*}}}
    {                 :c1 nil}
    {                 :c1 {}}
    nil
    {}) ; =>
  {:a1 :A1, :b1 :B1*, :c1 {:a2 :A2, :b2 {:a3 :A3, :b3 :B3*}}})

;;;; Swap stuff

(defn- -platform-cas! "Minor optimization for single-threaded Cljs"
  [atom_ old-val new-val]
  #+cljs (do (reset! atom_ new-val) true) ; No compare for our uses
  #+clj  (.compareAndSet ^clojure.lang.Atom atom_ old-val new-val))

;; Fastest possible simple swap with [old new] return
(defn dswap! "Returns [<old-val> <new-val>]" [atom_ f]
  #+cljs (let [ov @atom_ nv (f ov)] (reset! atom_ nv) [ov nv])
  #+clj
  (loop []
    (let [old-val @atom_
          new-val (f old-val)]
      (if (.compareAndSet ^clojure.lang.Atom atom_ old-val new-val)
        [old-val new-val]
        (recur)))))

;; Fastest possible simple swap with k, new-in return
(defn- -swap-cache! "Used by memoization utils" [atom_ k f]
  #+cljs (let [om @atom_ nv (f (get om k)) nm (assoc om k nv)] (reset! atom_ nm) nv)
  #+clj
  (loop []
    (let [old-m @atom_
          new-v (f (get old-m k))
          new-m (assoc  old-m k new-v)]
      (if (.compareAndSet ^clojure.lang.Atom atom_ old-m new-m)
        new-v
        (recur)))))

(defrecord Swapped [new-val return-val])
(defn  swapped? [x] (instance? Swapped x))
(defn  swapped  [new-val return-val] (Swapped. new-val return-val))
(defn -swapped "Returns [<new-val> <return-val>]"
  ([x] (if (swapped? x) [(:new-val x) (:return-val x)] [x x]))
  ([old-val    f] (-swapped (f old-val)))
  ([old-val ks f]
   (let [[k1 & kn] ks ; Singular k1 is common case
         m old-val]
     (if kn ; >1 ks
       (if (kw-identical? f :swap/dissoc)
         (-swapped (dissoc-in m (butlast ks) (last ks)))
         (let [old-val-in (get-in m ks)
               [new-val-in return-val] (-swapped (f old-val-in))
               new-val (if (kw-identical? new-val-in :swap/dissoc)
                         (dissoc-in m (butlast ks) (last ks))
                         (assoc-in  m ks new-val-in))]
           [new-val return-val]))

       ;; 0 or 1 ks
       (if (and (nil? k1) (empty? ks))
         (-swapped (f old-val))
         (if (kw-identical? f :swap/dissoc)
           (-swapped (dissoc m k1))
           (let [old-val-in (get m k1)
                 [new-val-in return-val] (-swapped (f old-val-in))
                 new-val (if (kw-identical? new-val-in :swap/dissoc)
                           (dissoc m k1)
                           (assoc  m k1 new-val-in))]
             [new-val return-val])))))))

(defn- -replace-in
  "Reduces input with
  [<type> <ks> <reset-val-or-swap-fn>] or
         [<ks> <reset-val-or-swap-fn>] ops"
  [?vf-type m ops]
  (reduce
    (fn [acc ?op]
      (if-not ?op ; Allow conditional ops: (when <pred> <op>), etc.
        acc
        (let [[vf-type ks valf] (if-not ?vf-type ?op (cons ?vf-type ?op))]
          (case vf-type
            :reset (if (empty? ks) valf (assoc-in acc ks valf))
            :swap  (if (nil? valf)
                     acc ; Noop, allows conditional ops
                     (if (empty? ks)
                       (valf acc)
                       ;; Currently ignore possible <return-val>:
                       (nth (-swapped acc ks valf) 0)))))))
    m ops))

(defn replace-in "For use with `swap!`, etc." [m & ops] (-replace-in nil m ops))

(comment
  (replace-in {}
    [:reset [:a] {:b :B :c 100}]
    (when false [:reset [:a :b] :B2]) ; Conditionals okay
    (do (assert true)
        [:reset [:a :b] :B3]) ; Side-effects okay
    (let [my-swap-fn inc] ; `let`s okay
      [:swap [:a :c] my-swap-fn]))

  (let [a_ (atom {})]
    (swap! a_ replace-in
      [:reset [:a]    {:b :b1 :c :c1 :d 100}]
      [:swap  [:a :d] inc]
      [:swap  [:a :b] :swap/dissoc]
      [:swap  [:a :c] (fn [_] :swap/dissoc)]))

  (let [a_ (atom [0 1 2])]
    (swap! a_ replace-in
      [:swap [0] inc]
      ;; [:swap [5] identity] ; Will throw
      [:swap [5]] nil ; Noop (no throw)
      )))

(compile-if (completing (fn [])) ; Transducers
  (defn- pairs-into [to from] (into to (partition-all 2) from))
  (defn- pairs-into [to from] (into to (partition     2  from))))

(defn swap-in!
  "More powerful version of `swap!`:
    * Supports optional `update-in` semantics.
    * Swap fn can return `(swapped <new-val> <return-val>)` rather than just
      <new-val>. This is useful when writing atomic pull fns, etc."

  ([atom_ f] ; Like `swap!` with `swapped` support
   (loop []
     (let [old-val @atom_
           [new-val return-val] (-swapped (f old-val))]
       (if (-platform-cas! atom_ old-val new-val)
         return-val
         (recur)))))

  ([atom_ ks f] ; Add `update-in` support
   (loop []
     (let [old-val @atom_
           [new-val return-val] (-swapped old-val ks f)]
       (if (-platform-cas! atom_ old-val new-val)
         return-val
         (recur)))))

  ;; Add `replace-in` support, note no way to support `swapped`
  ([atom_ ks f & more] {:pre [(have? even? (count more))]}
   (let [op-pairs (pairs-into [[ks f]] more)]
     (loop []
       (let [old-val @atom_
             new-val (-replace-in :swap old-val op-pairs)]
         (if (-platform-cas! atom_ old-val new-val)
           [old-val new-val]
           (recur)))))))

(defn reset-in! "Is to `reset!` as `swap-in!` is to `swap!`"
  ([atom_ ks new-val]
   (if (empty? ks)
     (reset! atom_ new-val)
     ;; Actually need swap! (CAS) to preserve other keys:
     (swap!  atom_ (fn [old-val] (assoc-in old-val ks new-val)))))

  ([atom_ ks new-val & more] {:pre [(have? even? (count more))]}
   (let [op-pairs (pairs-into [[ks new-val]] more)]
     (loop []
       (let [old-val @atom_
             new-val (-replace-in :reset old-val op-pairs)]
         (if (-platform-cas! atom_ old-val new-val)
           [old-val new-val]
           (recur)))))))

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
      ;; [:a] #(dissoc % :c)
      ;; [:a :c] :swap/dissoc
      [:a :c] (fn [_] :swap/dissoc)
      [:a :d] inc))

  (let [a_ (atom {})]
    (reset-in! a_
      []      {:a {:b :b1 :c :c1 :d 100}}
      [:a :b] :b2
      [:a :d] inc)))

;;;; Strings

(defn str-builder? [x]
  #+clj  (instance? StringBuilder x)
  #+cljs (instance? goog.string.StringBuffer x))

(def str-builder "For cross-platform string building"
  #+clj  (fn (^StringBuilder []       (StringBuilder.))
            (^StringBuilder [s-init] (StringBuilder. ^String s-init)))
  #+cljs (fn ([]       (goog.string.StringBuffer.))
            ([s-init] (goog.string.StringBuffer. s-init))))

(def sb-append "For cross-platform string building"
  #+clj  (fn ^StringBuilder [^StringBuilder str-builder ^String s] (.append str-builder s))
  #+cljs (fn                [               str-builder         s] (.append str-builder s)))

(comment (str (sb-append (str-builder "foo") "bar")))

(def str-rf "String builder reducing fn"
  (fn
    ([]       (str-builder))
    ([acc]               (if (str-builder? acc) acc (str-builder (str acc)))) ; cf
    ([acc in] (sb-append (if (str-builder? acc) acc (str-builder (str acc))) (str in)))))

(comment
  (qb 1000 ; [358.45 34.6]
         (reduce str    (range 512))
    (str (reduce str-rf (range 512)))))

#+cljs (defn undefined->nil [x] (if (undefined? x) nil x))
(defn nil->str [x]
  #+clj  (if (nil? x) "nil" x)
  #+cljs (if (or (undefined? x) (nil? x)) "nil" x))

(compile-if (completing (fn [])) ; Transducers
  (defn str-join
    "Faster, transducer-based generalization of `clojure.string/join` with `xform`
    support"
    ([                coll] (str-join nil       nil coll))
    ([separator       coll] (str-join separator nil coll))
    ([separator xform coll]
     (if (and separator (not= separator ""))
       (let [sep-xform (interpose separator)
             str-rf*   (completing str-rf str)]
         (if xform
           (transduce (comp xform sep-xform) str-rf* coll)
           (transduce             sep-xform  str-rf* coll)))
       (if xform
         (transduce xform (completing str-rf str) coll)
         (str (reduce str-rf coll)))))))

(comment
  (qb 100000
    (str/join "," ["a" "b" "c" "d"])
    (str-join "," ["a" "b" "c" "d"])
    (str-join ""  ["a" "b" "c" "d"])) ; [29.37 23.63 13.34]
  (str-join "," (comp (filter #{"a" "c"}) (map str/upper-case)) ["a" "b" "c"]))

(defn format*
  #+clj ^String [fmt args]
  #+cljs        [fmt args]
  (let [fmt  (or fmt "") ; Prevent NPE
        args (mapv nil->str args)]
    #+clj  (String/format fmt (to-array args))
    ;; Removed from cljs.core 0.0-1885, Ref. http://goo.gl/su7Xkj (pulls in a
    ;; lot of Google Closure that's not v. friendly to dead code elimination):
    #+cljs (apply gstr/format fmt args)))

(defn format
  "Like `clojure.core/format` but:
    * Returns \"\" when fmt is nil rather than throwing an NPE
    * Formats nil as \"nil\" rather than \"null\"
    * Provides ClojureScript support via goog.string.format (this has fewer
      formatting options than Clojure's `format`!)"
  [fmt & args] (format* fmt args))

(defn str-replace
  "Workaround for http://dev.clojure.org/jira/browse/CLJS-794,
                  http://dev.clojure.org/jira/browse/CLJS-911.

  Note that ClojureScript 1.7.145 technically introduced a breaking \"fix\" for
  CLJS-911 (Ref. https://goo.gl/bk5hcT) but it's a mess in an attempt to keep
  some compatibility with the previous broken behaviour. The merged CLJS-911 fix
  provides only inconsistent (single-match) and error-prone compatibility with
  Clojure's `str/replace`. CLJS-794 is also still unresolved.

  This util provides behaviour consistent between Clojure/Script."

  [s match replacement]
  #+clj (str/replace s match replacement)
  #+cljs
  (cond
    (string? match) ; string -> string replacement
    (.replace s (js/RegExp. (gstr/regExpEscape match) "g") replacement)
    ;; (.hasOwnProperty match "source") ; No! Ref. http://goo.gl/8hdqxb

    (instance? js/RegExp match) ; pattern -> string/fn replacement
    (let [flags (str "g" (when (.-ignoreCase match) "i")
                         (when (.-multiline  match) "m")) ; Fix CLJS-794
          replacement ; Fix CLJS-911
          (if (string? replacement)
            replacement
            ;; Note that the merged CLJS-911 fix actually tries to vary
            ;; behaviour here based on the number of matches(!)
            (fn [& args] (replacement (vec args))))]
      (.replace s (js/RegExp. (.-source match) flags) replacement))
    :else (throw (str "Invalid match arg: " match))))

(defn substr
  "Gives a consistent, flexible, cross-platform substring API built on
  `sub-indexes`"
  [s start-idx & [?max-len]] {:pre [(have? string? s)]}
  (let [[start-idx* end-idx*] (sub-indexes s start-idx :max-len ?max-len)]
    #+clj  (.substring ^String s start-idx* end-idx*)
    ;; Could also use .substr:
    #+cljs (.substring         s start-idx* end-idx*)))

(comment (substr "hello" -1 1))

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

(defn str-?index [s substr & [start-idx last?]]
  (let [start-idx (int (or start-idx 0))
        result    (int (if last?
                         #+clj  (.lastIndexOf ^String s ^String substr start-idx)
                         #+cljs (.lastIndexOf         s         substr start-idx)
                         #+clj  (.indexOf     ^String s ^String substr start-idx)
                         #+cljs (.indexOf             s         substr start-idx)))]
    (when (not= result -1) result)))

(comment (str-?index "hello there" "there"))

;; Back-compatible volatiles, private for now
;; Note: benching seems to consistently show that atoms are actually no
;; slower than volatiles when used in the same way (i.e. w/o contention
;; or watches)?
(compile-if (volatile! nil)
  (do
    (defmacro -vol!       [val]           `(volatile!     ~val))
    (defmacro -vol-reset! [vol_ val]      `(vreset! ~vol_ ~val))
    (defmacro -vol-swap!  [vol_ f & args] `(vswap!  ~vol_ ~f ~@args)))
  (do
    (defmacro -vol!       [val]           `(atom         ~val))
    (defmacro -vol-reset! [vol_ val]      `(reset! ~vol_ ~val))
    (defmacro -vol-swap!  [vol_ f & args] `(swap!  ~vol_ ~f ~@args))))

(defn str-join-once "Like `string/join` but skips duplicate separators"
  [separator coll]
  (let [sep separator]
    (if (str/blank? sep)
      (str (reduce str-rf "" coll))
      (let [acc-ends-with-sep?_ (-vol! false)
            acc-empty?_         (-vol! true)]
        (str
          (reduce
            (fn [acc in]
              (let [in (str in)
                    in-empty? (= in "")
                    in-starts-with-sep? (str-starts-with? in sep)
                    in-ends-with-sep?   (str-ends-with?   in sep)
                    acc-ends-with-sep?  @acc-ends-with-sep?_
                    acc-empty?          @acc-empty?_]

                (-vol-reset! acc-ends-with-sep?_ in-ends-with-sep?)
                (when acc-empty? (-vol-reset! acc-empty?_ in-empty?))

                (if acc-ends-with-sep?
                  (if in-starts-with-sep?
                    (sb-append acc (.substring in 1))
                    (sb-append acc in))

                  (if in-starts-with-sep?
                    (sb-append acc in)
                    (if (or acc-empty? in-empty?)
                      (sb-append acc in)
                      (do (sb-append acc sep)
                          (sb-append acc in)))))))
            (str-builder)
            coll))))))

(defn path [& parts] (str-join-once "/" parts))

(comment (path "foo/" nil "/bar" "baz/" "/qux/"))

(defn norm-word-breaks
  "Converts all word breaks of any form and length (including line breaks of any
  form, tabs, spaces, etc.) to a single regular space"
  [s] (str/replace (str s) #"\s+" \space))

(defn count-words [s] (if (str/blank? s) 0 (count (str/split s #"\s+"))))
(comment (count-words "Hello this is a    test"))

(defn uuid-str
  "Returns a UUIDv4 string of form \"xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx\",
  Ref. http://www.ietf.org/rfc/rfc4122.txt,
       https://gist.github.com/franks42/4159427"
  ([max-length] (substr (uuid-str) 0 max-length))
  ([]
   #+clj (str (java.util.UUID/randomUUID))
   #+cljs
   (let [hex  (fn [] (.toString (rand-int 16) 16))
         rhex (.toString (bit-or 0x8 (bit-and 0x3 (rand-int 16))) 16)]
     (str (hex) (hex) (hex) (hex)
          (hex) (hex) (hex) (hex) "-"
          (hex) (hex) (hex) (hex) "-"
          "4"   (hex) (hex) (hex) "-"
          rhex  (hex) (hex) (hex) "-"
          (hex) (hex) (hex) (hex)
          (hex) (hex) (hex) (hex)
          (hex) (hex) (hex) (hex)))))

(comment (uuid-str 5))

;;;; Env

(defmacro get-env []
  (let [ks (reduce
             (fn [acc in]
               (if (str-starts-with? (name in) "__") ; Hide privates
                 acc ; Strip primitive tags which can cause issues:
                 (conj acc (without-meta in))))
             [] (keys &env))]
    `(zipmap '~ks ~ks)))

(comment [(let [x :x] (get-env)) ((fn [^long x] (get-env)) 0)])

;;;; IO

#+clj
(defn slurp-resource
  "Returns slurped named resource on classpath, or nil when resource not found"
  [rname]
  (when-let [r (io/resource rname)]
    (try (slurp (io/reader r))
         (catch Exception e
           (throw (ex-info (format "Failed to slurp resource: %s" rname)
                    {:rname rname} e))))))

#+clj
(defn get-file-resource-?last-modified
  "Returns last-modified time for file backing given named resource, or nil if
  file doesn't exist"
  [rname]
  (when-let [file (try (->> rname io/resource io/file) (catch Exception _))]
    (.lastModified ^java.io.File file)))

#+clj
(def file-resources-modified?
  "Returns true iff any files backing the given named resources have changed
  since last call"
  (let [udts_ (atom {}) ; {<rname-or-rnames> <udt-or-udts>}
        swap! (fn [ks v] (swap-in! udts_ ks
                          (fn [?v] (swapped v (when (not= v ?v) v)))))]
    (fn [rnames & [?id]]
      (if-let [rn1 (->?singleton rnames)] ; Optimize single-file case:
        (swap! [?id rn1] (get-file-resource-?last-modified rn1))
        (let [rgroup (into (sorted-set) rnames)]
          (swap! [?id rgroup] (mapv get-file-resource-?last-modified rgroup)))))))

#+clj
(def slurp-file-resource
  "Like `slurp-resource` but caches slurps against file's last-modified udt"
  (let [;; {<rname> [<content_> <last-modified-udt>]}
        cache_ (atom {})]
    (fn [rname]
      (when-let [curr-udt (get-file-resource-?last-modified rname)]
        (force
          (swap-in! cache_ [rname]
            (fn [[?prev-content_ ?prev-udt :as ?v]]
              (if (= curr-udt ?prev-udt)
                (swapped ?v ?prev-content_)
                (let [content_ (delay (slurp-resource rname))]
                  (swapped [content_ curr-udt] content_))))))))))

(comment (slurp-file-resource "log4j.properties"))

;;;; Memoization

(def ^:private ^:const gc-rate (/ 1.0 16000))
(defn gc-now? [] (<= ^double (rand) gc-rate))

(defn memoize_
  "Like `clojure.core/memoize` but faster + uses delays to avoid write races"
  [f]
  #+cljs
  (let [a0-sentinel (js-obj) ; Faster as a local binding
        cache_ (atom {})]
    (fn
      ([ ] @(or (get @cache_ a0-sentinel)
                (-swap-cache! cache_ a0-sentinel (fn [?dv] (if ?dv ?dv (delay (f)))))))
      ([x] @(or (get @cache_ x)
                (-swap-cache! cache_ x           (fn [?dv] (if ?dv ?dv (delay (f x)))))))
      ([x & more]
       (let [xs (cons x more)]
         @(or (get @cache_ xs)
              (-swap-cache! cache_ xs (fn [?dv] (if ?dv ?dv (delay (apply f xs))))))))))

  #+clj
  (let [a0-sentinel     (Object.)
        nil-a1-sentinel (Object.)
        cache_ (java.util.concurrent.ConcurrentHashMap.)]
    (fn
      ([ ] @(or (.get cache_ a0-sentinel)
                (let [dv (delay (f))] (or (.putIfAbsent cache_ a0-sentinel dv) dv))))
      ([x] (let [xk (if (nil? x) nil-a1-sentinel x)]
             @(or (.get cache_ xk)
                  (let [dv (delay (f x))] (or (.putIfAbsent cache_ xk dv) dv)))))
      ([x & more]
       (let [xs (cons x more)]
         @(or (.get cache_ xs)
            (let [dv (delay (apply f xs))] (or (.putIfAbsent cache_ xs dv) dv))))))))

(comment
  (do
    (def f0  (memoize  (fn [])))
    (def f0_ (memoize_ (fn [])))
    (def f1  (memoize  (fn [x] x)))
    (def f1_ (memoize_ (fn [x] x))))
  (qb 100000 (f0   ) (f0_   )) ; [ 5.61 4.94]
  (qb 100000 (f1 :x) (f1_ :x)) ; [20.79 5.61]
  )

(defn memoize1
  "Great for Reactjs render op caching on mobile devices, etc."
  [f]
  (let [cache_ (atom {})]
    (fn [& args]
      @(or (get @cache_ args)
           (get (swap! cache_
                  (fn [cache]
                    (if (get cache args)
                      cache
                      {args (delay (apply f args))})))
             args)))))

(defn memoized
  "Like `(memoize* f)` but takes an explicit cache atom (possibly nil)
  and immediately applies memoized f to given arguments"
  [cache f & args]
  (if-not cache ; {<args> <delay-val>}
    (apply f args)
    @(-swap-cache! cache args (fn [?dv] (if ?dv ?dv (delay (apply f args)))))))

(defn memoize*
  "Like `clojure.core/memoize` but:
    * Can be significantly faster (depends on opts)
    * Uses delays to prevent race conditions on writes
    * Supports auto invalidation & gc with `ttl-ms` option
    * Supports cache size limit & gc with `cache-size` option
    * Supports manual invalidation by prepending args with `:mem/del` or `:mem/fresh`"

  ;; TODO Consider locking writes on GC?

  ;; De-raced, commands
  ([f]
   (let [cache_ (atom {})] ; {<args> <delay-val>}
     (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
       (cond
         (kw-identical? arg1 :mem/debug) cache_
         (kw-identical? arg1 :mem/del)
         (do (if (kw-identical? (first argn) :mem/all)
               (reset! cache_ {})
               (swap!  cache_ dissoc argn))
             nil)
         :else
         (let [fresh? (kw-identical? arg1 :mem/fresh)
               args   (if fresh? argn args)]
           @(or (get @cache_ args)
                (-swap-cache! cache_ args
                  (fn [?dv] (if (and ?dv (not fresh?))
                             ?dv
                             (delay (apply f args)))))))))))

  ;; De-raced, commands, ttl, gc
  ([ttl-ms f]
   (have? pos-int? ttl-ms)
   (let [cache_       (atom {}) ; {<args> <[delay-val udt :as cache-val]>}
         gc-running?_ (atom false)
         ttl-ms       (long ttl-ms)
         gc-fn
         (fn []
           (when (and (gc-now?)
                      (swap-in! gc-running?_ [] (fn [b] (swapped true (not b)))))
             (let [instant (now-udt)
                   snapshot @cache_ ; NON-atomic gc actually preferable
                   ks-to-gc
                   (persistent!
                     (reduce-kv
                       (fn [acc k [dv ^long udt :as cv]]
                         (if (> (- instant udt) ttl-ms)
                           (conj! acc k)
                           acc))
                       (transient [])
                       snapshot))]

               (swap! cache_
                 (fn [m]
                   (persistent!
                     (reduce (fn [acc in] (dissoc! acc in))
                       (transient m) ks-to-gc))))

               (reset! gc-running?_ false))))]

     (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
       (cond
         (kw-identical? arg1 :mem/debug) cache_
         (kw-identical? arg1 :mem/del)
         (do (if (kw-identical? (first argn) :mem/all)
               (reset! cache_ {})
               (swap!  cache_ dissoc argn))
             nil)
         :else
         (do
           (gc-fn)
           (let [fresh?  (kw-identical? arg1 :mem/fresh)
                 args    (if fresh? argn args)
                 instant (now-udt)
                 [dv]    (-swap-cache! cache_ args
                           (fn [?cv]
                             (if (and ?cv (not fresh?)
                                   (let [[_dv ^long udt] ?cv]
                                     (< (- instant udt) ttl-ms)))
                               ?cv
                               [(delay (apply f args)) instant])))]
             @dv))))))

  ;; De-raced, commands, ttl, gc, max-size
  ([cache-size ttl-ms f]
   (have? [:or nil? pos-int?] ttl-ms)
   (have? pos-int? cache-size)
   (let [;; {:tick _ <args> <[dval ?udt tick-lru tick-lfu :as cval]>}:
         state_       (atom {:tick 0})
         gc-running?_ (atom false)
         ttl-ms?      (not (nil? ttl-ms))
         ttl-ms       (long (or ttl-ms 0))
         cache-size   (long cache-size)
         gc-fn
         (fn []
           (when (and (gc-now?)
                      (swap-in! gc-running?_ [] (fn [b] (swapped true (not b)))))

             (let [instant (now-udt)]

               (when ttl-ms? ; First prune expired stuff
                 (let [snapshot (dissoc @state_ :tick)
                       ks-to-gc
                       (persistent!
                         (reduce-kv
                           (fn [acc k [dv ^long udt _ _ :as cv]]
                             (if (> (- instant udt) ttl-ms)
                               (conj! acc k)
                               acc))
                           (transient [])
                           snapshot))]

                   (swap! state_
                     (fn [m]
                       (persistent!
                         (reduce (fn [acc in] (dissoc! acc in))
                           (transient m) ks-to-gc))))))

               ;; Then prune by ascending (worst) tick-sum:
               (let [snapshot (dissoc @state_ :tick)
                     n-to-gc  (- (count snapshot) cache-size)]

                 (when (> n-to-gc 64) ; Tradeoff between sort cost + write contention
                   (let [ks-to-gc
                         (top n-to-gc
                           (fn [k]
                             (let [[_ _ ^long tick-lru ^long tick-lfu] (snapshot k)]
                               (+ tick-lru tick-lfu)))
                           (keys snapshot))]

                     ;; (println (str "ks-to-gc: " ks-to-gc)) ; Debug
                     (swap! state_
                       (fn [m]
                         (persistent!
                           (reduce (fn [acc in] (dissoc! acc in))
                             (transient m) ks-to-gc)))))))

               (reset! gc-running?_ false))))

         cv-fn
         (if-not ttl-ms?
           (fn [args fresh? tick]
             (-swap-cache! state_ args
               (fn [?cv]
                 (if (and ?cv (not fresh?))
                   ?cv
                   [(delay (apply f args)) nil tick 1]))))

           (fn [args fresh? tick]
             (let [instant (now-udt)]
               (-swap-cache! state_ args
                 (fn [?cv]
                   (if (and ?cv (not fresh?)
                         (let [[_dv ^long udt] ?cv]
                           (< (- instant udt) ttl-ms)))
                     ?cv
                     [(delay (apply f args)) instant tick 1]))))))]

     (fn ^{:arglists '([command & args] [& args])} [& [arg1 & argn :as args]]
       (cond
         (kw-identical? arg1 :mem/debug) state_
         (kw-identical? arg1 :mem/del)
         (do (if (kw-identical? (first argn) :mem/all)
               (reset! state_ {:tick 0})
               (swap!  state_ dissoc argn))
             nil)
         :else
         (do
           (gc-fn)
           (let [fresh?     (kw-identical? arg1 :mem/fresh)
                 args       (if fresh? argn args)
                 ^long tick (:tick @state_) ; Atomic sync unimportant
                 [dv]       (cv-fn args fresh? tick)]

             ;; We always adjust counters, even on reads:
             (swap! state_
               (fn [m]
                 (if-let [[dv ?udt tick-lru ^long tick-lfu :as cv] (get m args)]
                   (assoc m :tick (inc tick) args [dv ?udt tick (inc tick-lfu)])
                   (assoc m :tick (inc tick)))))
             @dv)))))))

(comment
  (do
    (def f0 (memoize         (fn [& [x]] (if x x (Thread/sleep 600)))))
    (def f1 (memoize*        (fn [& [x]] (if x x (Thread/sleep 600)))))
    (def f2 (memoize* 5000   (fn [& [x]] (if x x (Thread/sleep 600)))))
    (def f3 (memoize* 2 nil  (fn [& [x]] (if x x (Thread/sleep 600)))))
    (def f4 (memoize* 2 5000 (fn [& [x]] (if x x (Thread/sleep 600))))))

  (qb 10000 (f0 :x) (f1 :x) (f2 :x) (f3 :x) (f4 :x))
  ;; [2.06 3.99 7.23 14.66 15.24]

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

;;;;

(defn rate-limiter* ; `rate-limiter` name taken by deprecated API
  "Takes one or more rate specs of form [ncalls-limit window-ms ?spec-id] and
  returns a (fn [& [req-id])) that returns `nil` (=> all rate limits passed), or
  [<ms-wait> <worst-offending-spec-id>] / <ms-wait>."
  [specs]
  (if (empty? specs)
    (constantly nil)
    (let [vspecs      (vec specs)
          vstates_    (atom {}) ; {<req-id> [[ncalls udt-window-start] <...>]}
          max-win-ms  (long (reduce max 0 (mapv (fn [[_ win-ms _ :as spec]] win-ms)
                                            vspecs)))
          nspecs      (count vspecs)
          nid-specs   (count (filterv (fn [[_ _ id]] id) vspecs))
          _           (assert (or (zero? nid-specs) (= nid-specs nspecs)))
          return-ids? (not (zero? nid-specs))]

      (fn check-rate-limits [& [?a1 ?a2]]
        (cond
          (kw-identical? ?a1 :rl/debug) vstates_
          (kw-identical? ?a1 :rl/reset)
          (do
            (if (kw-identical? ?a2 :rl/all)
              (reset! vstates_ {})
              (swap!  vstates_ dissoc ?a2))
            nil)

          :else
          (let [peek?   (kw-identical? ?a1 :rl/peek)
                req-id  (if peek? ?a2 ?a1)
                instant (now-udt)]

            (when (and req-id (gc-now?))
              (swap-in! vstates_ []
                (fn gc [m]
                  (reduce-kv
                    (fn [m* req-id vstate]
                      (let [^long max-udt-win-start
                            (reduce (fn [^long acc [_ ^long udt _]]
                                      (max acc udt))
                              0 vstate)
                            min-win-ms-elapsed (- instant max-udt-win-start)]
                        (if (> min-win-ms-elapsed max-win-ms)
                          (dissoc m* req-id)
                          m*)))
                    m m))))

            (swap-in! vstates_ [req-id]
              (fn [?vstate]
                (if-not ?vstate
                  (if peek?
                    (swapped ?vstate nil)
                    (swapped (vec (repeat nspecs [1 instant])) nil))

                  ;; Need to atomically check if all limits pass before committing
                  ;; to any ncall increments:
                  (let [[vstate-with-resets ?worst-limit-offence]
                        (loop [in-vspecs  vspecs
                               in-vstate  ?vstate
                               out-vstate []
                               ?worst-limit-offence nil]
                          (let [[[^long ncalls-limit ^long win-ms ?spec-id]
                                 & next-in-vspecs] in-vspecs
                                [[^long ncalls ^long udt-win-start]
                                 & next-in-vstate] in-vstate

                                win-ms-elapsed (- instant udt-win-start)
                                reset-due?     (>= win-ms-elapsed win-ms)
                                rate-limited?  (and (not reset-due?)
                                                    (>= ncalls ncalls-limit))
                                new-out-vstate ; No ncall increments yet:
                                (conj out-vstate
                                  (if reset-due? [0 instant] [ncalls udt-win-start]))

                                new-?worst-limit-offence
                                (if-not rate-limited?
                                  ?worst-limit-offence
                                  (let [ms-wait (- win-ms win-ms-elapsed)]
                                    (if (or (nil? ?worst-limit-offence)
                                            (let [[^long max-ms-wait _] ?worst-limit-offence]
                                              (> ms-wait max-ms-wait)))
                                      [ms-wait ?spec-id]
                                      ?worst-limit-offence)))]

                            (if-not next-in-vspecs
                              [new-out-vstate new-?worst-limit-offence]
                              (recur next-in-vspecs next-in-vstate new-out-vstate
                                     new-?worst-limit-offence))))

                        all-limits-pass? (nil? ?worst-limit-offence)
                        new-vstate
                        (cond
                          peek? ?vstate
                          (not all-limits-pass?) vstate-with-resets
                          :else
                          (mapv (fn [[^long ncalls udt-win-start]]
                                  [(inc ncalls) udt-win-start])
                            vstate-with-resets))

                        result
                        (when-let [wlo ?worst-limit-offence]
                          (if return-ids?
                            wlo
                            (let [[ms-wait _] wlo] ms-wait)))]

                    (swapped new-vstate result)))))))))))

(comment
  (def rl (rate-limiter* [[5 2000 :5s] [10 20000 :20s]]))
  (def rl (rate-limiter* [[5 2000    ] [10 10000     ]]))
  (def rl (rate-limiter* [[5 2000 :5s]]))
  (repeatedly 5 (fn [] (rl :rid1)))
  (rl :rid1)
  (rl :rl/peek :rid1)
  (rl :rid2)
  (qb 10000 (rl)))

(defn rate-limit [specs f]
  (let [rl (rate-limiter* specs)]
    (fn [& args]
      (if-let [backoff (rl)]
        [nil backoff]
        [(f) nil]))))

(comment
  (def compute (rate-limit [[3 5000 :5s]] (fn [] "Compute!")))
  (compute))

;;;; Async

#+clj
(defn future-pool
  "Experimental. Returns a fixed-size future pool:
    (fn
      [f] - Blocks to acquire a future, then executes (f) on that future.
      [ ] - Blocks to acquire all futures, then immediately releases them.
            Useful for blocking till all outstanding work completes.
  Timeout variants are also provided."
  [^long n]
  (let [s    (java.util.concurrent.Semaphore. n)
        msecs java.util.concurrent.TimeUnit/MILLISECONDS
        fp-call
        (fn [f]
          (when-not (fn? f) (.release s) (throw (ex-info "Not a fn" {:arg f})))
          (future
            (try
              (f)
              (catch Throwable _ nil)
              (finally (.release s))))
          true)]

    (fn fp
      ([ ] (.acquire s n) (.release s n) true)
      ([f] (.acquire s) (fp-call f))

      ([^long timeout-ms timeout-val]
       (if (.tryAcquire s n timeout-ms msecs)
         (do (.release s n) true)
         timeout-val))

      ([^long timeout-ms timeout-val f]
       (if (.tryAcquire s timeout-ms msecs)
         (fp-call f)
         timeout-val)))))

(comment
  (time
    (let [fp (future-pool 2)]
      [(fp (fn [] (Thread/sleep 2000) (println "2000")))
       (fp (fn [] (Thread/sleep 500)  (println "500")))
       (fp 200 "timeout" (fn [] (Thread/sleep 900) (println "900")))
       (fp (fn [] (Thread/sleep 3000) (println "3000")))
       (fp)])))

;;;; Benchmarking

#+clj (defn nano-time ^long [] (System/nanoTime))
#+cljs
(def nano-time
  ;; 1ms = 10^6ns
  ;; Uses **window context** as epoch!, Ref. http://goo.gl/mWZWnR
  (if-let [perf (and (oget js-?win "performance"))]
    ;; Ref. http://goo.gl/fn84us
    (if-let [f (or (oget perf "now")  (oget perf "mozNow") (oget perf "msNow")
                   (oget perf "oNow") (oget perf "webkitNow"))]
      ;; JS call returns millisecs double, accurate to 1/1000th of a ms:
      (fn [] (long (* 1e6 (.call f perf))))
      (fn [] (* 1e6 (now-udt))))
    (fn [] (* 1e6 (now-udt)))))

(defmacro time-ms "Returns number of milliseconds it takes to execute body"
  [& body] `(let [t0# (now-udt)] ~@body (- (now-udt) t0#)))

(defmacro time-ns "Returns number of nanoseconds it takes to execute body"
  [& body] `(let [t0# (nano-time)] ~@body (- (nano-time) t0#)))

(defmacro quick-bench "Returns fastest of 3 sets of times for each form, in msecs"
  ([nlaps form & more] (mapv (fn [form] `(quick-bench ~nlaps ~form)) (cons form more)))
  ([nlaps form]
   `(let [nlaps# ~nlaps
          ;; 3 warmup sets, 3 working sets:
          [nlaps# nsets#] (if (vector? nlaps#) nlaps# [nlaps# 6])
          [nlaps# nsets#] (have pos-num? nlaps# nsets#)]
      (round2
        ;; Note that we avoid (repeatedly _ _ (fn [] _)) here since the fn
        ;; wrapping seems to have subtle effects on JIT inlining (artificially
        ;; _improving_ performance in some circumstances). Still unclear what
        ;; the ideal strategy is; will require some research.
        (/ (long (reduce min
                   (for [_# (range (long nsets#))]
                     (time-ns (dotimes [_# (long nlaps#)] (do ~form))))))
          1e6)))))

(defmacro qb     [& args] `(quick-bench ~@args)) ; Alias
(defmacro qbench [& args] `(quick-bench ~@args)) ; Alias (deprecated)

(comment
  (qb 4      (Thread/sleep 100))
  (qb [4 10] (Thread/sleep 100))
  (qb 1e5 (first [1 2 3 4 5]) (nth [1 2 3 4 5] 0)))

#+clj
(defn bench* "Repeatedly executes fn and returns time taken to complete execution"
  [^long nlaps {:keys [^long nlaps-warmup ^long nthreads as-ns?]
                :or   {nlaps-warmup 0
                       nthreads     1}} f]
  (try (dotimes [_ nlaps-warmup] (f))
    (let [nanosecs
          (if (= nthreads 1)
            (time-ns (dotimes [_ nlaps] (f)))
            (let [nlaps-per-thread (/ nlaps nthreads)]
              (time-ns
                (let [futures (repeatedly-into [] nthreads
                                (fn [] (future (dotimes [_ nlaps-per-thread] (f)))))]
                  (mapv deref futures)))))]
      (if as-ns? nanosecs (round* (/ nanosecs 1e6))))
    (catch Throwable t
      (println (str "Bench failure: " (.getMessage t)))
      -1)))

(defmacro bench [nlaps bench*-opts & body]
  `(bench* ~nlaps ~bench*-opts (fn [] ~@body)))

(declare spaced-str-with-nils)

;;;; Browser stuff

#+cljs
(do ; Trivial client-side logging stuff
  (def ^:private console-log
    (if-let [f (and (exists? js/console) (.-log js/console))]
      (fn [xs] (.apply f js/console (into-array xs)))
      (fn [xs] nil)))

  (defn log  [& xs]     (console-log xs)) ; Raw args
  (defn logp [& xs]     (console-log [(spaced-str-with-nils xs)]))
  (defn logf [fmt & xs] (console-log [(format* fmt xs)]))
  (defn sayp [    & xs] (js/alert (spaced-str-with-nils  xs)))
  (defn sayf [fmt & xs] (js/alert (format* fmt xs))))

#+cljs
(defn get-win-loc
  "Returns `js/window`'s current location as a map"
  []
  (when-let [js-win js-?win]
    (when-let [loc (.-location js-win)]
      {;; Ref. http://bl.ocks.org/abernier/3070589
       :href     (.-href     loc) ; "http://www.example.org:80/foo/bar?q=baz#bang"
       :protocol (.-protocol loc) ; "http:" ; Note the :
       :hostname (.-hostname loc) ; "example.org"
       :host     (.-host     loc) ; "example.org:80"
       :pathname (.-pathname loc) ; "/foo/bar"
       :search   (.-search   loc) ; "?q=baz"
       :hash     (.-hash     loc) ; "#bang"
       })))

#+cljs (def ^:private xhr-pool_ (delay (goog.net.XhrIoPool.)))
#+cljs
(defn- get-pooled-xhr!
  "Returns an immediately available XhrIo instance, or nil. The instance must be
  released back to pool manually."
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
      :post [uri ?pstr]
      :put  [uri ?pstr])))

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
     :timeout-ms 7000
     :with-credentials? false ; Enable if using CORS (requires xhr v2+)
    }
    (fn async-callback [resp-map]
      (let [{:keys [success? ?status ?error ?content ?content-type]} resp-map]
        ;; ?status  - 200, 404, ..., or nil on no response
        ;; ?error   - e/o #{:xhr-pool-depleted :exception :http-error :abort
        ;;                  :timeout :no-content <http-error-status> nil}
        (js/alert (str \"Ajax response: \" resp-map)))))"
  ;; TODO Ajax file params support
  [uri {:keys [method params headers timeout-ms resp-type with-credentials?
               progress-fn ; Undocumented, experimental
               errorf] :as opts
        :or   {method :get timeout-ms 10000 resp-type :auto
               errorf logf}}
   callback]
  {:pre [(have? [:or nil? nneg-int?] timeout-ms)]}
  (if-let [xhr (get-pooled-xhr!)]
    (try
      (let [timeout-ms (or (:timeout opts) timeout-ms) ; Deprecated opt
            method*    (case method :get "GET" :post "POST" :put "PUT")
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
                    ;; e/o #{200 201 202 204 206 304 1223},
                    ;; Ref. http://goo.gl/6qcVp0:
                    success?      (.isSuccess xhr)
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
                            :edn  (read-edn (.getResponseText xhr)))
                          ;; NB Temp workaround for http://goo.gl/UW7773:
                          (catch js/Error #_:default e
                            ;; Undocumented, subject to change:
                            {:ajax/bad-response-type resp-type
                             :ajax/resp-as-text (.getResponseText xhr)}))))

                    cb-arg
                    {;;; Raw stuff
                     :raw-resp resp
                     :xhr      xhr ; = (.-target resp)
                     ;;;
                     :success? success?
                     :?content-type (when ?http-status ?content-type)
                     :?content ?content
                     :?status  ?http-status
                     :?error
                     (or
                       (if ?http-status
                         ;; TODO `let` here is temporary workaround to suppress
                         ;; spurious Cljs warnings:
                         (let [^number n ?http-status]
                           (when-not success? ; (<= 200 n 299)
                             ?http-status))
                         (get { ;; goog.net.ErrorCode/NO_ERROR nil
                               goog.net.ErrorCode/EXCEPTION  :exception
                               goog.net.ErrorCode/HTTP_ERROR :http-error
                               goog.net.ErrorCode/ABORT      :abort
                               goog.net.ErrorCode/TIMEOUT    :timeout}
                           (.getLastErrorCode xhr) :unknown))
                       (when (and (nil? ?content)
                                  (not (#{204 1223} ?http-status)))
                         ;; Seems reasonable?:
                         :no-content))}]
                (callback cb-arg)))))

        ;; Experimental
        (when-let [pf progress-fn]
          (gevents/listen xhr goog.net.EventType/PROGRESS
            (fn [ev]
              (let [length-computable? (.-lengthComputable ev)
                    loaded (.-loaded ev)
                    total  (.-total  ev)
                    ?ratio (when (and length-computable?
                                      (not= total 0))
                             (/ loaded total))]
                (pf
                  {:?ratio ?ratio
                   :length-computable? length-computable?
                   :loaded loaded
                   :total  total
                   :ev     ev})))))

        (enc-macros/doto-cond [x xhr]
          :always (.setTimeoutInterval (or timeout-ms 0)) ; nil = 0 = no timeout
          with-credentials? (.setWithCredentials true) ; Requires xhr v2+
          :always (.send uri* method* post-content* headers*))

        ;; Allow aborts, etc.:
        xhr)

      (catch js/Error e
        ;; (logf "`ajax-lite` error: %s" e)
        (.releaseObject @xhr-pool_ xhr)
        nil))

    (do ; Pool failed to return an available xhr instance
      (callback {:?error :xhr-pool-depleted})
      nil)))

;;;; Ring

#+clj
(defn session-swap
  "Small util to help correctly manage (modify) funtional sessions. Please use
  this when writing Ring middleware! It's *so* easy to get this wrong and end up
  with subtle, tough-to-diagnose issues."
  [req resp f & args]
  (when resp
    (let [base (if (contains? resp :session) (:session resp) (:session req))
          new-session (if (empty? args) (f base) (apply f base args))]
      (assoc resp :session new-session))))

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

(defn url-encode "Stolen from http://goo.gl/99NSR1"
  #+clj  [s & [encoding]]
  #+cljs [s]
  (when s
    #+clj  (-> (str s)
               (java.net.URLEncoder/encode (or encoding "UTF-8"))
               (str/replace "*" "%2A")
               (str/replace "+" "%2B"))
    #+cljs (-> (str s)
               (js/encodeURIComponent s)
               (str/replace "*" "%2A")
               (str/replace "'" "%27"))))

(comment (mapv url-encode ["foo+bar" 47]))

(defn url-decode "Stolen from http://goo.gl/99NSR1"
  [s & [encoding]]
  (when s
    #+clj  (java.net.URLDecoder/decode s (or encoding "UTF-8"))
    #+cljs (js/decodeURIComponent s)))

(comment (url-decode (url-encode "Hello there~*+")))

(defn format-query-string [m]
  (let [param (fn [k v]  (str (url-encode (as-qname k)) "="
                             (url-encode (or (as-?qname v) (str v)))))
        join  (fn [strs] (str/join "&" strs))]
    (if (empty? m)
      ""
      (join (for [[k v] m :when (nnil? v)]
              (if (sequential? v)
                (join (mapv (partial param k) (or (seq v) [""])))
                (param k v)))))))

(comment
  (format-query-string {})
  (format-query-string {:k1 "v1" :k2 "v2" :k3 nil :k4 "" :k5 ["v4a" "v4b" 7] :k6 []})
  (format-query-string {:a/b :c/d})
  (format-query-string {:k nil}) ; Nb to allow removing pre-existing params, etc.
  )

(defn- assoc-conj [m k v]
  (assoc m k (if-let [cur (get m k)] (if (vector? cur) (conj cur v) [cur v]) v)))

(comment (assoc-conj {:a "a"} :a "b"))

(defn parse-query-params "Based on `ring-codec/form-decode`"
  [s & [keywordize? encoding]] {:post [(have? map? %)]}
  (if (str/blank? s) {}
    (let [;; For convenience (e.g. JavaScript win-loc :search)
          s (if (str-starts-with? s "?") (substr s 1) s)]
      (if-not (str-contains? s "=") {}
        (let [m (reduce
                  (fn [m param]
                    (if-let [[k v] (str/split param #"=" 2)]
                      (assoc-conj m (url-decode k encoding) (url-decode v encoding))
                      m))
                  {}
                  (str/split s #"&"))]
          (if-not keywordize? m
            (map-keys keyword m)))))))

(comment
  (parse-query-params nil)
  (parse-query-params "?foo=bar" :keywordize)
  (-> {:k1 "v1" :k2 "v2" :k3 nil :k4 "" :k5 ["v4a" "v4b"] :k6 [] :k7 47}
      (format-query-string)
      (parse-query-params)))

(defn merge-url-with-query-string [url m]
  (let [[url ?qstr] (str/split (str url) #"\?" 2)
        qmap  (merge
                (when ?qstr (map-keys keyword (parse-query-params ?qstr)))
                (map-keys keyword m))
        ?qstr (as-?nblank (format-query-string qmap))]
    (if-let [qstr ?qstr] (str url "?" qstr) url)))

(comment
  (merge-url-with-query-string "/" nil)
  (merge-url-with-query-string "/?foo=bar" nil)
  (merge-url-with-query-string "/?foo=bar" {"foo" "overwrite"})
  (merge-url-with-query-string "/?foo=bar" {:foo  "overwrite"})
  (merge-url-with-query-string "/?foo=bar" {:foo  nil})
  (merge-url-with-query-string "/?foo=bar" {:foo2 "bar2" :num 5 :foo nil}))

;;;; Stubfns

(defn -new-stub_ [sfn-name]
  (-vol!
    (fn [& args]
      (throw (ex-info (str "No stubfn implementation for: " sfn-name)
               {:sfn-name sfn-name :args args})))))

(defmacro defn-stub
  "Defines a stub function that can be implemented from anywhere (incl. other
  namespaces) using `implement-<stubfn-name>`. Handy for defining functions
  in shared namespaces but implementing them elsewhere (private namespaces,
  cyclic namespaces, etc.). `params*` are optional and used only for
  argslists and type hints."
  {:arglists '([name ?docstring ?attrs [params*] ...])}
  [sfn-name & sigs]
  (let [[sfn-name sigs] (name-with-attrs sfn-name sigs)
        stub_ (symbol (str "__"         (name sfn-name) "_stub"))
        impl  (symbol (str "implement-" (name sfn-name)))
        have-varargs? (some   #(some #{'&} %) sigs)
        fixed-params  (remove #(some #{'&} %) sigs)
        pmetas (reduce #(assoc %1 (count %2) (meta %2)) {} fixed-params)
        ps0    (with-meta '[              ] (pmetas 0))
        ps1    (with-meta '[x1            ] (pmetas 1))
        ps2    (with-meta '[x1 x2         ] (pmetas 2))
        ps3    (with-meta '[x1 x2 x3      ] (pmetas 3))
        ps4    (with-meta '[x1 x2 x3 x4   ] (pmetas 4))
        ps5    (with-meta '[x1 x2 x3 x4 x5] (pmetas 5))
        psm               '[x1 x2 x3 x4 x5 & more]]

    `(do
       (defonce ~stub_ (-new-stub_ ~(name sfn-name)))
       (defn ~impl [~'f] (-vol-reset! ~stub_ ~'f))
       (let [~'stub_ ~stub_]
         (defn ~sfn-name {:arglists '~sigs :stubfn? true}
           (~ps0       (@~'stub_      ))
           (~ps1       (@~'stub_ ~@ps1))
           (~ps2       (@~'stub_ ~@ps2))
           (~ps3       (@~'stub_ ~@ps3))
           (~ps4       (@~'stub_ ~@ps4))
           (~ps5       (@~'stub_ ~@ps5))
           (~psm (apply @~'stub_ ~@(remove #{'&} psm))))))))

(comment
  (defn -foo ^long [x] (* x x))
  (macroexpand '(defn-stub foo "docstring" [x] [x y & more]))
  (defn-stub foo "docstring" ^long [x])
  (implement-foo -foo)
  (qb 1000000 (-foo 5) (foo 5)) ; [69.87 70.46]
  )

;;;; Testing utils

;; TODO `deftest` with auto name prefixing
;; TODO `expect` macro, utils to pave over differences in clojure.test,
;; cljs.test (e.g. for `use-fixtures`)

;;;; DEPRECATED

#+cljs (def get-window-location get-win-loc)
(def backport-run!   run!*)
(def fq-name         as-qname)
(def qname           as-qname)
(def merge-deep-with nested-merge-with)
(def merge-deep      nested-merge)
(def parse-bool      as-?bool)
(def parse-int       as-?int)
(def parse-float     as-?float)
(def swapped*        -swapped)
(def swap-val!       -swap-cache!)
(def memoize-a0_     memoize_)
(def memoize-a1_     memoize_)
(def a0-memoize_     memoize_)
(def a1-memoize_     memoize_)
(def memoize-1       memoize1)

(defmacro cond-throw  [& args] `(cond! ~@args))
(defmacro have-in    [s1 & sn] `(have  ~s1 :in ~@sn))
(defmacro have-in!   [s1 & sn] `(have! ~s1 :in ~@sn))

;;; Prefer `str-join` when possible (needs Clojure 1.7+)
(defn spaced-str-with-nils [xs] (str/join " " (mapv nil->str xs)))
(defn spaced-str [xs] (str/join " " #+clj xs #+cljs (mapv undefined->nil xs)))

;; Arg order changed for easier partials, etc.:
(defn round [n & [type nplaces]] (round* (or type :round) nplaces n))

;; & coll changed to coll:
(defn join-once [sep & coll] (apply str-join-once coll))

;; Used by Carmine <= v2.7.0
(defmacro repeatedly* [n & body] `(repeatedly-into* [] ~n ~@body))
(defmacro repeatedly-into* "Deprecated" ; Used by Nippy < v2.10
  [coll n & body] `(repeatedly-into ~coll ~n (fn [] ~@body)))

;;; Arg order changed for easier partials
(defn keys=      [m ks] (ks=      ks m))
(defn keys<=     [m ks] (ks<=     ks m))
(defn keys>=     [m ks] (ks>=     ks m))
(defn keys=nnil? [m ks] (ks-nnil? ks m))

;; API changed for greater flexibility:
(defn rate-limiter [ncalls-limit window-ms] (rate-limiter* [[ncalls-limit window-ms]]))
(defn rate-limited [ncalls-limit window-ms f]
  (let [rl (rate-limiter* [[ncalls-limit window-ms]])]
    (fn [& args]
      (if-let [backoff-ms (rl)]
        {:backoff-ms backoff-ms}
        {:result     (f)}))))

;; Used by Sente <= v1.4.0-alpha2
(def logging-level (atom :debug)) ; Just ignoring this now

#+cljs ; Used by Sente <= v1.1.0
(defn set-exp-backoff-timeout! [nullary-f & [nattempt]]
  (when-let [js-win js-?win]
    (.setTimeout js-win nullary-f (exp-backoff (or nattempt 0)))))

#+cljs
(do ; Level-based Cljs logging (prefer Timbre v4+)
  (enc-macros/defonce* ^:dynamic *log-level* "DEPRECATED" :debug)
  (def ^:private log?
    (let [->n {:trace 1 :debug 2 :info 3 :warn 4 :error 5 :fatal 6 :report 7}]
      (fn [level] (>= (->n level) (->n *log-level*)))))

  (defn tracef  [fmt & xs] (when (log? :trace)  (apply logf fmt xs)))
  (defn debugf  [fmt & xs] (when (log? :debug)  (apply logf fmt xs)))
  (defn infof   [fmt & xs] (when (log? :info)   (apply logf fmt xs)))
  (defn warnf   [fmt & xs] (when (log? :warn)   (apply logf (str "WARN: "  fmt) xs)))
  (defn errorf  [fmt & xs] (when (log? :error)  (apply logf (str "ERROR: " fmt) xs)))
  (defn fatalf  [fmt & xs] (when (log? :fatal)  (apply logf (str "FATAL: " fmt) xs)))
  (defn reportf [fmt & xs] (when (log? :report) (apply logf fmt xs))))

(defn greatest [coll & [?comparator]]
  (let [comparator (or ?comparator rcompare)]
    (reduce #(if (pos? (comparator %1 %2)) %2 %1) coll)))

(defn least [coll & [?comparator]]
  (let [comparator (or ?comparator rcompare)]
    (reduce #(if (neg? (comparator %1 %2)) %2 %1) coll)))

(comment (greatest ["a" "e" "c" "b" "d"]))

(defn clj1098 "Ref. http://goo.gl/0GzRuz" [x] (or x {}))

(defn distinct-by "Deprecated, prefer `xdistinct`"
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

(defn distinctv "Deprecated, prefer `xdistinct`"
  ([      coll] (distinctv identity coll))
  ([keyfn coll]
   (let [tr (reduce (fn [[v seen] in]
                      (let [in* (keyfn in)]
                        (if-not (contains? seen in*)
                          [(conj! v in) (conj seen in*)]
                          [v seen])))
              [(transient []) #{}]
              coll)]
     (persistent! (nth tr 0)))))

(defn map-kvs "Deprecated, prefer `reduce-kv`" [kf vf m]
  (if-not m {}
    (let [vf (cond (nil? vf) (fn [_ v] v) :else vf)
          kf (cond (nil? kf) (fn [k _] k)
                   (kw-identical? kf :keywordize) (fn [k _] (keyword k))
                   :else kf)]
      (persistent!
        (reduce-kv (fn [m k v] (assoc! m (kf k v) (vf k v)))
          (transient {}) m)))))

(defn as-map "Deprecated, prefer `reduce-kvs`" [kvs & [kf vf]]
  (if (empty? kvs) {}
    (let [vf (cond (nil? vf) (fn [_ v] v) :else vf)
          kf (cond (nil? kf) (fn [k _] k)
                   (kw-identical? kf :keywordize) (fn [k _] (keyword k))
                   :else kf)]
      (persistent!
        (reduce-kvs
          (fn [m k v] (assoc! m (kf k v) (vf k v))) (transient {}) kvs)))))

(defn keywordize-map [m] (map-keys keyword m))
(defn removev [pred coll] (filterv (complement pred) coll))
(defn nvec? [n x] (and (vector? x) (= (count x) n)))
