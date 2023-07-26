(ns taoensso.encore
  "Extended core library for Clojure/Script that emphasizes:
    * Cross platform API compatibility
    * Flexibility
    * Performance
    * Backwards compatibility

  This lib's mostly for my own use and for advanced users that feel
  comfortable reading this source. Not providing much beginner-oriented
  documentation for this, sorry.

  Quick Taoensso naming conventions:
    **foo** - Dynamic var
    foo!    - Fn with side effects, or that should otherwise be used cautiously
    foo?    - Truthy val or fn that returns truthy val
    foo!?   - Fn that has side effects (or requires caution) and that return
              a truthy val. Note: !?, not ?!
    foo$    - Fn that's notably expensive to compute (e.g. hits db)
    foo_    - Derefable val (e.g. atom, volatile, delay, etc.)
    foo__   - Derefable in a derefable (e.g. delay in an atom), etc.
    _       - Unnamed val
    _foo    - Named but unused val
    ?foo    - Optional val (emphasize that val may be nil)
    foo*    - A variation of `foo` (e.g. `foo*` macro vs `foo` fn)
    foo'    - ''
    -foo    - Public implementation detail or intermediate (e.g. uncoerced) val
    >foo    - Val \"to   foo\" (e.g. >sender, >host), or fn to  put/coerce/transform
    <foo    - Val \"from foo\" (e.g. <sender, <host), or fn to take/coerce/transform
    ->foo   - Fn to put/coerce/transform

  Type affixes may be used for clarity:
    <prefix>-<name>  - m-users,   v-users,   n-users,    etc. (also nusers when unambiguous)
    <name>-<postfix> - users-map, users-vec, user-count, etc.

  Regarding name heirarchy:
    When there's a significant num of syms with a meaningful hierarchy,
    prefer names with descending hierarchy to emphasize structure and
    related groups/functionality, e.g.:
      `user-add`, `user-remove`, `user-mod` vs
      `add-user`, `remove-user`, `mod-user`, etc.

  Commit message tags (in priority order):
    ~      - Work-in-progress (still under development)
    [mod]  - Modify     behaviour (=>          breaking), [mod!], [mod!!], etc. for attention
    [fix]  - Fix broken behaviour (=> usu. non-breaking)
    [new]  - Add new    behaviour (=>      non-breaking)
    [nop]  - Unmodified behaviour (=>      non-breaking implementation or non-code changes, etc.)
    [x][y] - Single commit with multiple tags (in priority order), try avoid

  Example commit messages:
    v1.0.0 (2022-01-27) ; Tagged release
    [new] [#122] Add new feature x (@contributor)"

  {:author "Peter Taoussanis (@ptaoussanis)"}

  (:refer-clojure :exclude
   [if-let if-some if-not when when-not when-some when-let cond defonce
    run! some? ident? float? boolean? uri? indexed? bytes?
    int? pos-int? neg-int? nat-int?
    simple-ident?   qualified-ident?
    simple-symbol?  qualified-symbol?
    simple-keyword? qualified-keyword?
    format update-in merge merge-with
    memoize abs ex-message ex-cause])

  #?(:clj
     (:require
      [clojure.string  :as str]
      [clojure.set     :as set]
      [clojure.java.io :as io]
      [clojure.walk    :as walk :refer [macroexpand-all]]
      ;; [clojure.core.async    :as async]
      [clojure.tools.reader.edn :as edn]
      [taoensso.truss :as truss])

     :cljs
     (:require
      [clojure.string      :as str]
      [clojure.set         :as set]
      ;; [cljs.core.async  :as async]
      [cljs.reader]
      [cljs.tools.reader.edn :as edn]
      ;;[goog.crypt.base64 :as base64]
      [goog.object         :as gobj]
      [goog.array          :as garray]
      [goog.string         :as gstr]
      [goog.string.format]
      [goog.string.StringBuffer]
      [goog.events         :as gevents]
      [goog.net.XhrIo      :as gxhr]
      [goog.net.XhrIoPool  :as gxhr-pool]
      [goog.Uri.QueryData  :as gquery-data]
      [goog.net.EventType]
      [goog.net.ErrorCode]
      [taoensso.truss :as truss]))

  #?(:clj
     (:import
      [java.util Date Locale TimeZone]
      [java.text SimpleDateFormat]
      [java.util.concurrent CountDownLatch]
      ;; [org.apache.commons.codec.binary Base64]
      )

     :cljs
     (:require-macros
      [taoensso.encore :as enc-macros :refer
       [have have! have? compile-if
        if-let if-some if-not when when-not when-some when-let -cond cond defonce
        cond! catching -if-cas! now-dt* now-udt* now-nano* min* max*
        name-with-attrs deprecated new-object defalias throws throws?]])))

(def encore-version [3 62 1])

(comment "∴ ∵ ℕ ℤ ℝ ∞ ≠ ∈ ∉ ⇒⇔ → × ⊃⊂ ⊇⊆ ≡ ¬ ∀ ∃ ∝"
  (set! *unchecked-math* :warn-on-boxed)
  (set! *unchecked-math* false))

#?(:clj
   (do ; Bootstrap Truss aliases
     (defmacro ^:private -have  [& args] `(taoensso.truss/have  ~@args))
     (defmacro ^:private -have? [& args] `(taoensso.truss/have? ~@args))))

(comment
  (remove-ns 'taoensso.encore)
  (test/run-tests))

;;;; TODO v4
;; - Drop previously deprecated vars
;; - Transducers first
;; - Better match naming of late Clojure additions (e.g. nnil->some / sor)
;; - Consider docstrings catered for public consumption
;; - Imports from tl-core, tl-math, tl-nodes, baget, etc.
;; - Unit tests

;;;; Core macros

#?(:clj
   (defmacro compile-if
     "Evaluates `test`. If it returns logical true (and doesn't throw), expands
     to `then`, otherwise expands to `else`."
     {:style/indent 1}
     ([test then     ] `(compile-if ~test ~then nil)) ; Back compatibility
     ([test then else]
      (if (try (eval test) (catch Throwable _ false))
        `(do ~then)
        `(do ~else)))))

#?(:clj
   (defmacro try-eval
     "Evaluates `form`. If eval doesn't throw, expands to `form`, otherwise to nil."
     {:added "v3.50.0 (2023-03-07)"}
     [form] `(compile-if ~form ~form nil)))

(comment
  (macroexpand '(try-eval (java.util.concurrent.atomic.AtomicLong.)))
  (macroexpand '(try-eval (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)))
  (macroexpand '(try-eval (do (println "effect") :x))))

#?(:clj (defmacro compile-when {:style/indent 1} [test & body] `(compile-if ~test (do ~@body) nil)))
#?(:clj
   (compile-if
     ;; Avoiding for edge case where user cares about startup time and has
     ;; `core.async` as dependency but it never gets required anywhere else
     ;; (do (require 'clojure.core.async) true)
     (do (or
           (io/resource "clojure/core/async.clj")
           (io/resource "clojure/core/async.cljc")))
     (def have-core-async? true)
     (def have-core-async? false)))

(comment (require '[clojure.core.async] :verbose))

;;; (:ns &env) is nnil iff compiling for ClojureScript, giving us a way to
;;; write macros that produce different Clj/Cljs code (not something that
;;; .cljx or .cljc currently provide support for):
#?(:clj (defmacro if-clj  {:style/indent 1} [then & [else]] (if (:ns &env) else then)))
#?(:clj (defmacro if-cljs {:style/indent 1} [then & [else]] (if (:ns &env) then else)))

#?(:clj
   (defmacro if-let
     "Like `core/if-let` but can bind multiple values for `then` iff all tests
     are truthy, supports internal unconditional `:let`s."
     {:style/indent 1}
     ([bindings then     ] `(if-let ~bindings ~then nil))
     ([bindings then else]
      (let [s (seq bindings)]
        (if s ; (if-let [] true false) => true
          (let [[b1 b2 & bnext] s]
            (if (= b1 :let)
              `(let      ~b2  (if-let ~(vec bnext) ~then ~else))
              `(let [b2# ~b2]
                 (if b2#
                   (let [~b1 b2#]
                     (if-let ~(vec bnext) ~then ~else))
                   ~else))))
          then)))))

#?(:clj
   (defmacro if-some
     "Like `core/if-some` but can bind multiple values for `then` iff all tests
     are non-nil, supports internal unconditional `:let`s."
     {:style/indent 1}
     ([bindings then] `(if-some ~bindings ~then nil))
     ([bindings then else]
      (let [s (seq bindings)]
        (if s ; (if-some [] true false) => true
          (let [[b1 b2 & bnext] s]
            (if (= b1 :let)
              `(let      ~b2  (if-some ~(vec bnext) ~then ~else))
              `(let [b2# ~b2]
                 (if (nil? b2#)
                   ~else
                   (let [~b1 b2#]
                     (if-some ~(vec bnext) ~then ~else))))))
          then)))))

#?(:clj
   (defmacro if-not
     "Like `core/if-not` but acts like `if-let` when given a binding vector
     as test expr."
     ;; Also avoids unnecessary `(not test)`
     {:style/indent 1}
     ([test-or-bindings then]
      (if (vector? test-or-bindings)
        `(if-let ~test-or-bindings nil ~then)
        `(if     ~test-or-bindings nil ~then)))

     ([test-or-bindings then else]
      (if (vector? test-or-bindings)
        `(if-let ~test-or-bindings ~else ~then)
        `(if     ~test-or-bindings ~else ~then)))))

#?(:clj
   (defmacro when
     "Like `core/when` but acts like `when-let` when given a binding vector
     as test expr."
     {:style/indent 1}
     [test-or-bindings & body]
     (if (vector? test-or-bindings)
       `(if-let ~test-or-bindings (do ~@body) nil)
       `(if     ~test-or-bindings (do ~@body) nil))))

#?(:clj
   (defmacro when-not
     "Like `core/when-not` but acts like `when-let` when given a binding vector
     as test expr."
     {:style/indent 1}
     [test-or-bindings & body]
     (if (vector? test-or-bindings)
       `(if-let ~test-or-bindings nil (do ~@body))
       `(if     ~test-or-bindings nil (do ~@body)))))

#?(:clj
   (defmacro when-some
     {:style/indent 1}
     [test-or-bindings & body]
     (if (vector? test-or-bindings)
       `(if-some       ~test-or-bindings  (do ~@body) nil)
       `(if      (nil? ~test-or-bindings) nil (do ~@body)))))

#?(:clj
   (defmacro when-let
     "Like `core/when-let` but can bind multiple values for `body` iff all tests
     are truthy, supports internal unconditional `:let`s."
     {:style/indent 1}
     ;; Now a feature subset of all-case `when`
     [bindings & body] `(if-let ~bindings (do ~@body))))

(comment
  (if-let   [a :a b (= a :a)] [a b] "else")
  (if-let   [a :a b (= a :b)] [a b] "else")
  (if-some  [a :a b (= a :b)] [a b] "else")
  (when-let [a :a b nil] "true")
  (when-let [:let [a :a b :b] c (str a b)] c))

#?(:clj
   (defmacro -cond [throw? & clauses]
     (if-let [[test expr & more] (seq clauses)]
       (if-not (next clauses)
         test ; Implicit else
         (case test
           (true :else :default)       expr                             ; Faster than (if <truthy> ...)
           (false nil)                         `(-cond ~throw? ~@more)  ; Faster than (if <falsey> ...)
           :do          `(do          ~expr     (-cond ~throw? ~@more))
           :let         `(let         ~expr     (-cond ~throw? ~@more))
           :binding     `(binding     ~expr     (-cond ~throw? ~@more))

           :with-redefs `(with-redefs ~expr     (-cond ~throw? ~@more)) ; Undocumented
           :return-when `(if-let  [x# ~expr] x# (-cond ~throw? ~@more)) ; ''
           :return-some `(if-some [x# ~expr] x# (-cond ~throw? ~@more)) ; ''
           :when        `(when        ~expr     (-cond ~throw? ~@more)) ; ''
           :when-not    `(when-not    ~expr     (-cond ~throw? ~@more)) ; ''
           :when-some   `(when-some   ~expr     (-cond ~throw? ~@more)) ; ''

           ;;; 3-clause cases
           (:if-let :if-some :if-not)
           (if (empty? more) ; Missing 3rd clause
             (throw
               (ex-info (str "[encore/cond] Missing `then` clause for special test keyword: " test)
                 {:test-form test :expr-form expr}))

             (case test
               :if-let  `(if-let  ~expr ~(first more) (-cond ~throw? ~@(next more)))
               :if-some `(if-some ~expr ~(first more) (-cond ~throw? ~@(next more)))
               :if-not  `(if-not  ~expr ~(first more) (-cond ~throw? ~@(next more))) ; Undocumented
               ))

           (if (keyword? test)
             (throw ; Undocumented, but throws at compile-time so easy to catch
               (ex-info (str "[encore/cond] Unrecognized special test keyword: " test)
                 {:test-form test :expr-form expr}))

             (if (vector? test) ; Undocumented
               `(if-let ~test ~expr (-cond ~throw? ~@more))

               ;; Experimental, assumes `not` = `core/not`:
               (if (and (list? test) (= (first test) 'not))
                 `(if ~(second test) (-cond ~throw? ~@more) ~expr)
                 `(if ~test ~expr    (-cond ~throw? ~@more)))))))

       (when throw?
         `(throw
            (ex-info "[encore/cond!] No matching clause" {}))))))

#?(:clj
   (defmacro cond
     "Like `core/cond` but supports implicit final `else` clause, and special
     clause keywords for advanced behaviour:

     (cond
       :let     [x   \"x\"] ; Establish let     binding/s for remaining forms
       :binding [*x* \"x\"] ; Establish dynamic binding/s for remaining forms
       :do      (println (str \"x value: \" x)) ; Eval expr for side effects

       :if-let [y \"y\"
                z nil]
       \"y and z were both truthy\"

       :if-some [y \"y\"
                 z nil]
       \"y and z were both non-nil\")

     :let support inspired by https://github.com/Engelberg/better-cond.
     Simple, flexible way to eliminate deeply-nested control flow code."

     ;; Also avoids unnecessary `(if :else ...)`, etc.
     [& clauses] `(-cond false ~@clauses)))

#?(:clj
   (defmacro cond!
     "Like `cond` but throws on non-match like `case` and `condp`."
     [& clauses] `(-cond true ~@clauses)))

(comment
  [(macroexpand-all '(clojure.core/cond nil "a" nil "b" :else "c"))
   (macroexpand-all '(cond nil "a" nil "b" :else "c"))
   (macroexpand-all '(cond nil "a" nil "b" (println "bar")))
   (macroexpand-all '(cond :when true :let [x "x"] :else x))
   (macroexpand-all '(cond false 0 (not false) 1 2))
   (macroexpand-all '(cond [a :A] a))]

  (cond
    :if-not [n "Stu"] "Don't have a name"
    :else             (str n " Smith"))

  (cond  false "false")
  (cond! false "false")

  (cond  :if-let [])
  (cond  :if-let [a :a])
  (cond  :when   [a :a])
  (cond! :return-when (when true  "foo"))
  (cond! :return-when (when false "foo"))

  [(macroexpand-all '(cond  (= 1 0) "a"))
   (macroexpand-all '(cond! (= 1 0) "a"))

   (macroexpand-all '(cond  :let [a :a] (= 1 0) "a"))
   (macroexpand-all '(cond! :let [a :a] (= 1 0) "a"))

   (macroexpand-all '(cond  :if-let [a nil] a (= 1 0) "1=0" #_"default"))
   (macroexpand-all '(cond! :if-let [a nil] a (= 1 0) "1=0" #_"default"))

   (cond  :if-let [a nil] a (= 1 0) "1=0" #_"default")
   (cond! :if-let [a nil] a (= 1 0) "1=0" #_"default")])

(defn name-with-attrs
  "Given a symbol and args, returns [<name-with-attrs-meta> <args> <attrs>]
  with support for `defn` style `?docstring` and `?attrs-map`."
  ([sym args            ] (name-with-attrs sym args nil))
  ([sym args attrs-merge]
   (let [[?docstring args] (if (and (string? (first args)) (next args)) [(first args) (next args)] [nil args])
         [attrs      args] (if (and (map?    (first args)) (next args)) [(first args) (next args)] [{}  args])
         attrs (if ?docstring (assoc attrs :doc ?docstring) attrs)
         attrs (if-let [m (meta sym)] (conj m attrs) attrs)
         attrs (conj attrs attrs-merge)]

     [(with-meta sym attrs) args attrs])))

#?(:clj
   (defmacro defonce
     "Like `core/defonce` but supports optional docstring and attrs map."
     {:style/indent 1}
     [sym & args]
     (let [[sym body] (name-with-attrs sym args)]
       `(if-cljs
             (cljs.core/defonce ~sym ~@body)
          (clojure.core/defonce ~sym ~@body)))))

#?(:clj
   (defn compiling-cljs?
     "Return truthy iff currently generating Cljs code."
     []
     (when-let [n (find-ns 'cljs.analyzer)]
       (when-let [v (ns-resolve n '*cljs-file*)]
         @v))))

(comment (compiling-cljs?))

#?(:clj
   (defmacro keep-callsite
     "The long-standing CLJ-865 unfortunately means that it's currently
     not possible for an inner macro to access the &form metadata of an
     outer macro.

     This means that inner macros lose call site information like the
     line number of the outer macro.

     This util offers a workaround for macro authors:

       (defmacro inner  [] (meta &form))
       (defmacro outer1 []                `(inner))
       (defmacro outer2 [] (keep-callsite `(inner)))

       (inner)  => {:line _, :column _}
       (outer1) => nil
       (outer2) => {:line _, :column _}"

     {:added "v3.61.0 (2023-07-07)"}
     [& body] `(with-meta (do ~@body) (meta ~'&form))))

(comment :see-tests)

#?(:clj
   (defn get-source
     "Returns {:keys [ns line column file]} callsite and file info given a
     macro's compile-time `&form` and `&env` vals. See also `keep-callsite`."
     {:added "v3.61.0 (2023-07-07)"}
     [form env]
     (let [{:keys [line column file]} (meta form)
           file
           (if-not (:ns env)
             *file* ; Compiling clj
             (or    ; Compiling cljs
               (when-let [url (and file (try (io/resource file) (catch Throwable _ nil)))]
                 (try (.getPath (io/file url)) (catch Throwable _ nil))
                 (do            (str     url)))
               file))]

       {:ns     (str *ns*)
        :line   line
        :column column
        :file
        (when (string? file)
          (when-not (contains? #{"NO_SOURCE_PATH" "NO_SOURCE_FILE" ""} file)
            file))})))

(comment (io/resource "taoensso/encore.cljc"))
(comment :see-tests)

;;;; Core fns

(def ^:private core-merge     #?(:clj clojure.core/merge     :cljs cljs.core/merge))
(def ^:private core-update-in #?(:clj clojure.core/update-in :cljs cljs.core/update-in))
(declare merge update-in)

#?(:cljs (defn ^boolean some? [x] (if (nil? x) false true))
   :clj
   (defn some?
     {:inline (fn [x] `(if (identical? ~x nil) false true))}
     [x] (if (identical? x nil) false true)))

;;;; Secondary macros

#?(:clj
   (defmacro case-eval
     "Like `case` but evals test constants for their compile-time value."
     {:style/indent 1}
     [expr & clauses]
     (let [default (when (odd? (count clauses)) (last clauses))
           clauses (if default (butlast clauses) clauses)]
       `(case ~expr
          ~@(map-indexed (fn [i# form#] (if (even? i#) (eval form#) form#)) clauses)
          ~(when default default)))))

#?(:clj
   (do
     (defmacro do-nil   [& body] `(do ~@body nil))
     (defmacro do-false [& body] `(do ~@body false))
     (defmacro do-true  [& body] `(do ~@body true))))

#?(:clj
   (defmacro doto-cond
     "Cross between `doto`, `cond->` and `as->`."
     {:style/indent 1}
     [[sym x] & clauses]
     (assert (even? (count clauses)))
     (let [g (gensym)
           pstep (fn [[test-expr step]]
                   `(when-let [~sym ~test-expr] (-> ~g ~step)))]
       `(let [~g ~x]
          ~@(map pstep (partition 2 clauses))
          ~g))))

#?(:clj
   (defmacro declare-remote
     "Declares given ns-qualified symbols, preserving metadata. Useful for
     circular dependencies."
     [& syms]
     (let [original-ns (str *ns*)]
       `(do ~@(map (fn [s]
                     (let [ns (namespace s)
                           v  (name      s)
                           m  (meta      s)]
                       `(do (in-ns  '~(symbol ns))
                            (declare ~(with-meta (symbol v) m))))) syms)
            (in-ns '~(symbol original-ns))))))

(defn- quote-arglists [m]
  (if-let [x (get m :arglists)]
    (assoc m :arglists `'~x)
    (do    m)))

(comment (quote-arglists {:a :A :arglists '([x])}))

(def ^:private alias-src-attrs [:doc :arglists :private :macro :added :deprecated])
#?(:clj
   (defn -alias-link-var [dst-var src-var dst-attrs]
     (add-watch src-var dst-var
       (fn [_ _ _ new-val]
         (alter-var-root dst-var (fn [_] new-val))

         ;; Wait for src-var meta to change. This is hacky, but
         ;; generally only relevant for REPL dev so seems tolerable.
         (let [t (Thread/currentThread)]
           (future
             (.join t 100)
             (reset-meta! dst-var
               (core-merge (meta src-var) dst-attrs))))))))

#?(:clj
   (defmacro defalias
     "Defines a local alias for the var identified by the given qualified
     source symbol: (defalias my-map clojure.core/map), etc.

     For Clj:
       - Source var's metadata will be preserved (docstring, arglists, etc.).
       - Changes to source var's value will also be applied to alias."

     ([          src-sym            ] `(defalias nil        ~src-sym nil))
     ([alias-sym src-sym            ] `(defalias ~alias-sym ~src-sym nil))
     ([alias-sym src-sym alias-attrs]
      (-have? symbol? src-sym)
      (let [alias-sym (-have symbol? (or alias-sym (symbol (name src-sym))))
            compiling-cljs? (boolean (:ns &env))
            alias-attrs
            (if (string? alias-attrs) ; Back compatibility
              {:doc      alias-attrs}
              (do        alias-attrs))

            link?       (get    alias-attrs :link? true)
            alias-attrs (dissoc alias-attrs :link?)
            alias-attrs (core-merge (meta alias-sym) alias-attrs)]

        (if compiling-cljs?
          (let [alias-sym (with-meta alias-sym alias-attrs)]
            `(def ~alias-sym ~src-sym))

          (let [src-var (resolve src-sym)
                _
                (when-not src-var
                  (throw
                    (ex-info
                      (str "[encore/defalias] Failed to resolve source var for sym: " src-sym)
                      {:alias-sym       alias-sym
                       :src-sym         src-sym
                       :alias-attrs     alias-attrs
                       :compiling-cljs? compiling-cljs?
                       :reader-mode     #?(:clj :clj, :cljs :cljs, :default :other)})))

                src-attrs
                (let [src-meta (quote-arglists (meta src-var))]
                  (select-keys src-meta alias-src-attrs))

                final-attrs (core-merge src-attrs alias-attrs)
                alias-sym   (with-meta  alias-sym final-attrs)]

            `(do
               ;; Need `alter-meta!` to reliably retain ?macro status, Ref. Timbre #364
               (alter-meta!                  (def ~alias-sym @~src-var) conj ~final-attrs)
               (when ~link? (-alias-link-var (var ~alias-sym) ~src-var       ~alias-attrs))
               (do                           (var ~alias-sym)))))))))

(declare unexpected-arg!)

#?(:clj
   (defmacro defaliases
     "Bulk version of `defalias`.
     Takes source symbols or {:keys [alias src attrs]} maps:
       (defaliases
         {:alias my-map, :src map, :attrs {:doc \"My `map` alias\"}}
         {:alias my-vec, :src vec, :attrs {:doc \"My `vec` alias\"}})"

     {:added "v3.58.0 (2023-04-09)"}
     [& aliases]
     `(do
        ~@(map
            (fn [x]
              (cond
                (symbol? x) `(defalias ~x)
                (map?    x)
                (let [{:keys [alias src attrs]
                       :or   {attrs (dissoc x :alias :src)}} x]
                  `(defalias ~alias ~src ~attrs))

                :else
                (unexpected-arg! x
                  {:expected '#{symbol map}
                   :context  `defaliases})))

            aliases))))

(comment :see-tests)
(comment
  (defn src "src doc 1" [] "val1")
  (defalias ^{:doc "alias doc 1"} src* src {:doc "alias doc 2"})
  [(src*) (meta #'src*)]
  (macroexpand '(defaliases {:alias map2 :src map :doc "map2"}))
  (do           (defaliases {:alias map2 :src map :doc "map2"})))

#?(:clj
   (defmacro deftype-print-methods
     "Private, used by other Taoensso libs."
     {:added "v3.57.0 (2023-03-29)"}
     [& types]
     `(do ~@(map (fn [type]
                   `(defmethod print-method ~type [~'x ~(with-meta 'w {:tag 'java.io.Writer})]
                      (.write ~'w (str ~(str "#" *ns* ".") ~'x)))) types))))

(comment
  (do
    (deftype   Foo [] Object (toString [x] "Foo[]"))
    (defrecord Bar [] Object (toString [x] "Bar{}"))
    (deftype-print-methods Foo Bar)
    [(str (Foo.) " " (Bar.)) (Foo.) (Bar.)]))

;;;; Truss aliases (for back compatibility, convenience)

#?(:clj
   (do
     (defalias                 taoensso.truss/have)
     (defalias                 taoensso.truss/have!)
     (defalias                 taoensso.truss/have?)
     (defalias                 taoensso.truss/have!?)
     (defalias with-truss-data taoensso.truss/with-data)))

(defalias get-truss-data taoensso.truss/get-data)

;;;; Edn

(declare map-keys kw-identical?)

(defn read-edn
  "Attempts to pave over differences in:
    `clojure.edn/read-string`, `clojure.tools.edn/read-string`,
    `cljs.reader/read-string`, `cljs.tools.reader/read-string`.
   `cljs.reader` in particular can be a pain."

  ([     s] (read-edn nil s))
  ([opts s]
   ;; First normalize behaviour for unexpected inputs:
   (if (or (nil? s) (identical? s ""))
     nil
     (if-not (string? s)
       (throw
         (ex-info "[encore/read-edn] Unexpected arg type (expected string or nil)"
           {:arg {:value s :type (type s)}}))

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
               #?(:clj  clojure.core/*data-readers*

                  ;; Unfortunate (slow), but faster than gc'd memoization in most cases:
                  :cljs (map-keys symbol @cljs.reader/*tag-table*)))

             default
             (if-not (kw-identical? default ::dynamic)
               default
               #?(:clj  clojure.core/*default-data-reader-fn*
                  :cljs @cljs.reader/*default-data-reader-fn*))

             opts (assoc opts :readers readers :default default)]

         #?(:clj  (clojure.tools.reader.edn/read-string opts s)
            :cljs    (cljs.tools.reader.edn/read-string opts s)))))))

(defn pr-edn
  "Prints arg to an edn string readable with `read-edn`."
  ([      x] (pr-edn nil x))
  ([_opts x]
   #?(:cljs (binding [*print-level* nil, *print-length* nil] (pr-str x))
      :clj
      (let [sw (java.io.StringWriter.)]
        (binding [*print-level* nil, *print-length* nil,
                  ;; *out* sw, *print-dup* false
                  ]
          ;; (pr x)
          (print-method x sw) ; Bypass *out*, *print-dup*
          (.toString sw))))))

;;;; Errors

(defn ex-message
  "Copy of `core/ex-message` (added in Clojure v1.10)"
  {:added "v3.41.0 (2022-12-03)"}
  [ex]
  #?(:clj  (when (instance? Throwable ex) (.getMessage ^Throwable ex))
     :cljs (when (instance? js/Error  ex) (.-message              ex))))

(defn ex-cause
  "Copy of `core/ex-cause` (added in Clojure v1.10)"
  {:added "v3.41.0 (2022-12-03)"}
  [ex]
  #?(:clj  (when (instance? Throwable     ex) (.getCause ^Throwable ex))
     :cljs (when (instance? ExceptionInfo ex) (.-cause              ex))))

#?(:clj
   (defmacro catching
     "Cross-platform try/catch/finally."
     ;; Very unfortunate that CLJ-1293 has not yet been addressed
     ([try-expr                                             ] `(catching ~try-expr :all ~'__       nil         nil))
     ([try-expr            error-sym catch-expr             ] `(catching ~try-expr :all ~error-sym ~catch-expr nil))
     ([try-expr            error-sym catch-expr finally-expr] `(catching ~try-expr :all ~error-sym ~catch-expr ~finally-expr))
     ([try-expr error-type error-sym catch-expr finally-expr]
      (case error-type
        (:common :default) ; `:default` is a poor name, here only for back compatibility
        (if (nil? finally-expr)
          `(if-cljs
             (try ~try-expr (catch js/Error  ~error-sym ~catch-expr))
             (try ~try-expr (catch Exception ~error-sym ~catch-expr)))
          `(if-cljs
             (try ~try-expr (catch js/Error  ~error-sym ~catch-expr) (finally ~finally-expr))
             (try ~try-expr (catch Exception ~error-sym ~catch-expr) (finally ~finally-expr))))

        (:all :any)
        ;; Note unfortunate naming of `:default` in Cljs to refer to any error type
        (if (nil? finally-expr)
          `(if-cljs
             (try ~try-expr (catch :default  ~error-sym ~catch-expr))
             (try ~try-expr (catch Throwable ~error-sym ~catch-expr)))
          `(if-cljs
             (try ~try-expr (catch :default  ~error-sym ~catch-expr) (finally ~finally-expr))
             (try ~try-expr (catch Throwable ~error-sym ~catch-expr) (finally ~finally-expr))))

        ;; Specific error-type provided
        (if (nil? finally-expr)
          `(if-cljs
             (try ~try-expr (catch ~error-type ~error-sym ~catch-expr) (finally ~finally-expr))
             (try ~try-expr (catch ~error-type ~error-sym ~catch-expr) (finally ~finally-expr)))
          `(if-cljs
             (try ~try-expr (catch ~error-type ~error-sym ~catch-expr))
             (try ~try-expr (catch ~error-type ~error-sym ~catch-expr))))))))

(comment
  (macroexpand '(catching (do "foo") e e (println "finally")))
  (catching (zero? "9")))

(defn error-data
  "Returns data map iff `x` is an error of any type on platform."
  ;; Note Clojure >= 1.7 now has `Throwable->map` (clj only)
  [x]
  (when-let [data-map
             (and x
               (or
                 (ex-data x) ; ExceptionInfo
                 #?(:clj  (when (instance? Throwable x) {})
                    :cljs                               {})))]

    (let [base-map
          #?(:clj
             (let [^Throwable t x] ; (catch Throwable t <...>)
               {:err-type   (type                 t)
                :err-msg    (.getLocalizedMessage t)
                :err-cause  (.getCause            t)})

             :cljs
             (let [err x] ; (catch :default t <...)
               {:err-type  (type      err)
                :err-msg   (.-message err)
                :err-cause (.-cause   err)}))]

      #_(assoc base-map :err-data data-map)
      (conj    base-map           data-map))))

(comment
  (error-data (Throwable. "foo"))
  (error-data (Exception. "foo"))
  (error-data (ex-info    "foo" {:bar :baz})))

#?(:clj
   (defmacro caught-error-data
     "Handy for error-throwing unit tests."
     [& body] `(catching (do ~@body nil) e# (error-data e#))))

(comment (caught-error-data (/ 5 0)))

(defn- error-message
  ;; Note Clojure >= 1.10 now has `ex-message`
  [x]
  #?(:clj  (when (instance? Throwable x) (.getMessage ^Throwable x))
     :cljs (when (instance? js/Error  x) (.-message              x))))

(declare submap?)
(defn -matching-error
  ;; Ref. also CLJ-1293
  ([  err] err)
  ([c err]
   (when-let [match?
              (if (fn? c) ; Treat as pred
                (c err)
                #?(:clj
                   (case c
                     (:all        :any) (instance? Throwable                  err)
                     (:common :default) (instance? Exception                  err)
                     :ex-info           (instance? clojure.lang.ExceptionInfo err)
                     (do                (instance? c                          err)))

                   :cljs
                   (case c
                     (:all        :any) (some?                   err)
                     (:common :default) (instance? js/Error      err)
                     :ex-info           (instance? ExceptionInfo err)
                     (do                (instance? c             err)))))]
     err))

  ([c pattern err]
   (if-let [match?
            (and
              (-matching-error c err)
              (cond
                (nil? pattern) true
                (map? pattern)
                (if-let [data (ex-data err)]
                  (submap? data pattern)
                  false)

                :else
                (boolean
                  (re-find
                    (re-pattern pattern)
                    (error-message err)))))]
     err
     (if-let [cause (ex-cause err)]
       (-matching-error c pattern cause) ; Try match cause
       false))))

(comment
  (-matching-error                            (catching (/ 4 0) t t))
  (-matching-error :default #"Divide by zero" (catching (/ 4 0) t t))
  (-matching-error :default #"Nope"           (catching (/ 4 0) t t))
  (-matching-error :default #"Test"           (ex-info "Test" {:a :b}))
  (-matching-error :default {:a :b}           (ex-info "Test" {:a :b :c :d}))
  (-matching-error :ex-info {:a :b}           (ex-info "Dummy" {} (ex-info "Test" {:a :b}))))

#?(:clj
   (defmacro throws
     "Like `throws?`, but returns ?matching-error instead of true/false."
     {:added "v3.31.0 (2022-10-27)"}
     ([          form] `                             (catching (do ~form nil) ~'t ~'t))
     ([c         form] `(-matching-error ~c          (catching (do ~form nil) ~'t ~'t)))
     ([c pattern form] `(-matching-error ~c ~pattern (catching (do ~form nil) ~'t ~'t)))))

#?(:clj
   (defmacro throws?
     "Evals `form` and returns true iff it throws an error that matches given
     criteria:

       - `c` may be:
         - A predicate function, (fn match? [x]) -> bool
         - A class (e.g. ArithmeticException, AssertionError, etc.)
         - `:all`    => any    platform error (Throwable or js/Error, etc.)
         - `:common` => common platform error (Exception or js/Error)

       - `pattern` may be:
         - A string or Regex against which `ex-message` will be matched.
         - A map             against which `ex-data`    will be matched.

     When an error with (nested) causes doesn't match, a match will be attempted
     against its (nested) causes.

     Useful for unit tests, e.g.:
       (is (throws? {:a :b} (throw (ex-info \"Test\" {:a :b :c :d}))))

     See also `throws`."

     {:added "v3.31.0 (2022-10-27)"}
     ([          form] `(boolean (throws             ~form)))
     ([c         form] `(boolean (throws ~c          ~form)))
     ([c pattern form] `(boolean (throws ~c ~pattern ~form)))))

(comment :see-tests)

(let [get-default-error-fn
      (fn [base-data]
        (let [msg       (get    base-data :error/msg "Error thrown during reduction")
              base-data (dissoc base-data :error/msg)]

          (fn default-error-fn [data cause] ; == (partial ex-info <msg>)
            (throw (ex-info msg (conj base-data data) cause)))))]

  (defn catching-rf
    "Returns wrapper around given reducing function `rf` so that if `rf`
    throws, (error-fn <thrown-error> <contextual-data>) will be called.

    The default `error-fn` will rethrow the original error, wrapped in
    extra contextual information to aid debugging.

    See also `catching-xform`."
    {:added "v3.32.0 (2022-11-07)"}
    ([         rf] (catching-rf (get-default-error-fn {:rf rf}) rf))
    ([error-fn rf]
     (let [error-fn
           (if (map? error-fn) ; Undocumented convenience
             (get-default-error-fn error-fn)
             (do                   error-fn))]

       (fn catching-rf
         ([       ] (catching (rf)        t (error-fn {:rf rf :call '(rf)} t)))
         ([acc    ] (catching (rf acc)    t (error-fn {:rf rf :call '(rf acc)    :args {:acc {:value acc :type (type acc)}}} t)))
         ([acc in ] (catching (rf acc in) t (error-fn {:rf rf :call '(rf acc in) :args {:acc {:value acc :type (type acc)}
                                                                                        :in  {:value in  :type (type in)}}} t)))
         ([acc k v]
          (catching (rf acc k v) t
            (error-fn
              {:rf     rf
               :call '(rf acc k v)
               :args
               {:acc {:value acc :type (type acc)}
                :k   {:value k   :type (type k)}
                :v   {:value v   :type (type v)}}}
              t))))))))

(comment :see-tests)

(defn catching-xform
  "Like `catching-rf`, but applies to a transducer (`xform`).

  Makes debugging transductions much easier by greatly improving
  the information provided in any errors thrown by `xform` or the
  reducing fn:

    (transduce
      (catching-xform (comp (filter even?) (map inc))) ; Modified xform
      <reducing-fn>
      <...>)"

  {:added "v3.32.0 (2022-11-07)"}
  ([error-fn xform] (comp (fn [rf] (catching-rf error-fn rf)) xform))
  ([         xform] (comp           catching-rf               xform)))

(comment :see-tests)

;;;; Tests

(defn test-fixtures
  "Given a {:before ?(fn []) :after ?(fn [])} map, returns cross-platform
  test fixtures for use by both `clojure.test` and `cljs.test`:

    (let [f (test-fixtures {:before (fn [] (test-setup))})]
      (clojure.test/use-fixtures :once f)
         (cljs.test/use-fixtures :once f))"

  {:added "v3.31.0 (2022-10-27)"}
  [fixtures-map]
  (have? map?                         fixtures-map)
  ;; (have? [:ks<= #{:before :after}] fixtures-map)

  #?(:cljs fixtures-map ; Cljs supports a map with {:keys [before after]}
     :clj ; Clj wants a fn
     (let [{:keys [before after]} fixtures-map]
       (fn fixtures [f]
         (when before (before))
         (f)
         (when after (after))))))

(comment (test-fixtures {:before (fn [])}))

;;;; Type preds, etc.
;; - TODO Could really do with a portable ^boolean hint
;; - Some of these have slowly been getting added to Clojure core; make sure
;;   to :exclude any official preds using the same name

#?(:clj
   (do
     (defn stringy?    [x] (or (keyword? x) (string? x)))
     (defn ident?      [x] (or (keyword? x) (symbol? x)))
     (defn boolean?    [x] (instance? Boolean                           x))
     (defn uri?        [x] (instance? java.net.URI                      x))
     (defn indexed?    [x] (instance? clojure.lang.Indexed              x))
     (defn named?      [x] (instance? clojure.lang.Named                x))
     (defn editable?   [x] (instance? clojure.lang.IEditableCollection  x))
     (defn derefable?  [x] (instance? clojure.lang.IDeref               x))
     (defn throwable?  [x] (instance? Throwable                         x))
     (defn exception?  [x] (instance? Exception                         x))
     (defn error?      [x] (instance? Throwable                         x))
     (defn atom?       [x] (instance? clojure.lang.Atom                 x))
     (defn transient?  [x] (instance? clojure.lang.ITransientCollection x))
     (defn lazy-seq?   [x] (instance? clojure.lang.LazySeq              x))
     (defn re-pattern? [x] (instance? java.util.regex.Pattern           x))
     (defn can-meta?   [x] (instance? clojure.lang.IObj                 x)) ; Not IMeta

     (defn simple-ident?      [x] (and (ident?   x) (nil? (namespace x))))
     (defn qualified-ident?   [x] (and (ident?   x)       (namespace x) true))
     (defn simple-symbol?     [x] (and (symbol?  x) (nil? (namespace x))))
     (defn qualified-symbol?  [x] (and (symbol?  x)       (namespace x) true))
     (defn simple-keyword?    [x] (and (keyword? x) (nil? (namespace x))))
     (defn qualified-keyword? [x] (and (keyword? x)       (namespace x) true))

     (defn nempty-str? [x] (and (string? x) (not (.isEmpty ^String x))))
     (defn nblank-str? [x] (and (string? x) (not (str/blank? x))))
     (defn nblank?     [x]                  (not (str/blank? x)))
     (defn vec2?       [x] (and (vector? x) (= (count x) 2)))
     (defn vec3?       [x] (and (vector? x) (= (count x) 3))))

   :cljs
   (do
     (defn ^boolean stringy?    [x] (or (keyword? x) (string? x)))
     (defn ^boolean ident?      [x] (or (keyword? x) (symbol? x)))
     (defn ^boolean boolean?    [x] (or (true?    x) (false?  x)))
     ;; (defn uri?              [x])
     (defn ^boolean indexed?    [x] (satisfies?  IIndexed            x))
     (defn ^boolean named?      [x] (implements? INamed              x))
     (defn ^boolean editable?   [x] (implements? IEditableCollection x))
     (defn ^boolean derefable?  [x] (satisfies?  IDeref              x))
     ;; (defn throwable?        [x])
     ;; (defn exception?        [x])
     (defn ^boolean      error? [x] (instance?   js/Error             x))
     (defn ^boolean       atom? [x] (instance?   Atom                 x))
     (defn ^boolean  transient? [x] (instance?   ITransientCollection x))
     (defn ^boolean   lazy-seq? [x] (instance?   LazySeq              x))
     (defn ^boolean re-pattern? [x] (instance?   js/RegExp            x))
     (defn ^boolean   can-meta? [x] (implements? IWithMeta            x)) ; Not IMeta

     (defn ^boolean simple-ident?      [x] (and (ident?   x) (nil? (namespace x))))
     (defn ^boolean qualified-ident?   [x] (and (ident?   x)       (namespace x) true))
     (defn ^boolean simple-symbol?     [x] (and (symbol?  x) (nil? (namespace x))))
     (defn ^boolean qualified-symbol?  [x] (and (symbol?  x)       (namespace x) true))
     (defn ^boolean simple-keyword?    [x] (and (keyword? x) (nil? (namespace x))))
     (defn ^boolean qualified-keyword? [x] (and (keyword? x)       (namespace x) true))

     (defn ^boolean nempty-str? [x] (and (string? x) (not (= x ""))))
     (defn ^boolean nblank-str? [x] (and (string? x) (not (str/blank? x))))
     (defn ^boolean nblank?     [x]                  (not (str/blank? x)))
     (defn ^boolean vec2?       [x] (and (vector? x) (= (count x) 2)))
     (defn ^boolean vec3?       [x] (and (vector? x) (= (count x) 3)))))

;;; Number type naming conventions
;; Since Clojure usu. defaults to larger types (long>integer, double>long),
;; I'm appropriating the rarely-used smaller type names (int, float) to
;; refer to types of generic size.
;;
;; All fixed-precision:
;;   - int    - Generic  size: long   or integer, etc.
;;   - float  - Generic  size: double or float,   etc.
;;   - long   - Specific size: long   ; Only used when emphasizing specific size
;;   - double - Specific size: double ; Only used when emphasizing specific size

(defn #?(:clj finite-num? :cljs ^boolean finite-num?)
  "Returns true iff given a number (of standard type) that is:
  finite (excl. NaN and infinities)."
  [x]
  #?(:clj (and (number? x) (Double/isFinite x)) ; Works with other types, incl. ratio
     :cljs (js/Number.isFinite x)
     #_
     (and
       (not ^boolean (js/isNaN x))
       #_(not (identical? x js/Infinity))
       (not (identical? x js/Number.POSITIVE_INFINITY))
       (not (identical? x js/Number.NEGATIVE_INFINITY)))))

(defn #?(:clj int? :cljs ^boolean int?)
  "Returns true iff given a number (of standard type) that is:
  a fixed-precision integer."
  [x]
  #?(:clj
     (or
       (instance? Long    x)
       (instance? Integer x)
       (instance? Short   x)
       (instance? Byte    x))

     :cljs
     (and
       #_(number?   x)
       (finite-num? x)
       (== (js/parseFloat x) (js/parseInt x 10)))))

(defn #?(:clj float? :cljs ^boolean float?)
  "Returns true iff given a number (of standard type) that is:
  a fixed-precision floating-point (incl. NaN and infinities)."
  [x]
  #?(:clj (or (instance? Double x) (instance? Float x))
     :cljs
     (and
       (number?        x)
       #_(finite?-num? x)
       (not (== (js/parseFloat x) (js/parseInt x 10))))))

(comment (float? Double/NaN))

#?(:clj
   (do
     (defn      nneg? [x] (not (neg?    x)))
     (defn  zero-num? [x] (and (number? x)      (zero? x)))
     (defn nzero-num? [x] (and (number? x) (not (zero? x))))

     (defn nat-num?   [x] (and (number? x) (not (neg? x))))
     (defn pos-num?   [x] (and (number? x)      (pos? x)))
     (defn neg-num?   [x] (and (number? x)      (neg? x)))

     (defn nat-int?   [x] (and (int? x) (not (neg? x))))
     (defn pos-int?   [x] (and (int? x)      (pos? x)))
     (defn neg-int?   [x] (and (int? x)      (neg? x)))

     (defn nat-float? [x] (and (float? x) (not (neg? x))))
     (defn pos-float? [x] (and (float? x)      (pos? x)))
     (defn neg-float? [x] (and (float? x)      (neg? x)))

     (defn udt?       [x] (and (int? x) (not (neg? x)))))

   :cljs
   (do
     (defn ^boolean      nneg? [x] (not (neg? x)))
     (defn ^boolean  zero-num? [x]           (zero? x))
     (defn ^boolean nzero-num? [x] (and (not (zero? x))))

     (defn ^boolean nat-num?   [x] (not (neg? x)))
     (defn ^boolean pos-num?   [x]      (pos? x))
     (defn ^boolean neg-num?   [x]      (neg? x))

     (defn ^boolean nat-int?   [x] (and (int? x) (not (neg? x))))
     (defn ^boolean pos-int?   [x] (and (int? x)      (pos? x)))
     (defn ^boolean neg-int?   [x] (and (int? x)      (neg? x)))

     (defn ^boolean nat-float? [x] (and (float? x) (not (neg? x))))
     (defn ^boolean pos-float? [x] (and (float? x)      (pos? x)))
     (defn ^boolean neg-float? [x] (and (float? x)      (neg? x)))

     (defn ^boolean udt?       [x] (and (int? x) (not (neg? x))))))

(defn #?(:clj pnum? :cljs ^boolean pnum?)
  "Returns true iff given number in unsigned unit proportion interval ∈ℝ[0,1]."
  [x] (and (number? x) (let [n (double x)] (and (>= n 0.0) (<= n 1.0)))))

(defn #?(:clj rnum? :cljs ^boolean rnum?)
  "Returns true iff given number in signed unit proportion interval ∈ℝ[-1,1]."
  [x] (and (number? x) (let [n (double x)] (and (>= n -1.0) (<= n +1.0)))))

(compile-if have-core-async?
  (let [c ; Silly work-around for edge case described at `have-core-async`?
        (delay
          #?(:cljs cljs.core.async.impl.channels.ManyToManyChannel
             :clj
             (do
               (require       'clojure.core.async)
               (Class/forName "clojure.core.async.impl.channels.ManyToManyChannel"))))]

    #?(:clj  (defn          chan? [x] (instance? @c x))
       :cljs (defn ^boolean chan? [x] (instance? @c x))))
  (do        (defn          chan? [x] nil)))

(do
  ;; ClojureScript keywords aren't `identical?` and Clojure doesn't have
  ;; `keyword-identical?`. This util helps alleviate the pain of writing
  ;; cross-platform code, Ref. http://goo.gl/be8CGP
  #?(:clj  (def          kw-identical?         identical?)
     :cljs (def ^boolean kw-identical? keyword-identical?)))

;;;; Type coercions

(do
  ;; (defn not-blank     [s] (if (str/blank? s) nil s))
  ;; (defn not-empty-str [s] (if #?(:clj (.isEmpty ^String s) :cljs (= s "")) nil s))

  (defn as-?nzero  [x] (when (number?  x) (if (zero? x)      nil x)))
  (defn as-?nblank [x] (when (string?  x) (if (str/blank? x) nil x)))
  (defn as-?kw     [x] (cond (keyword? x)       x  (string? x) (keyword x)))
  (defn as-?name   [x] (cond (named?   x) (name x) (string? x)          x))
  (defn as-?qname  [x]
    (cond
      (named?  x) (let [n (name x)] (if-let [ns (namespace x)] (str ns "/" n) n))
      (string? x) x))

  (defn as-?nempty-str [x]
    (when (string? x)
      (if #?(:clj (.isEmpty ^String x) :cljs (= x "")) nil x)))

  (defn as-?nblank-trim [x]
    (when (string? x)
      (let [s (str/trim x)]
        (if #?(:clj (.isEmpty ^String s) :cljs (= s "")) nil s))))

  (comment (as-?nblank-trim " foo  "))

  (defn as-?int #_as-?long [x]
    (cond (number? x) (long x)
          (string? x)
          #?(:cljs (let [x (js/parseInt x 10)] (when-not (js/isNaN x) x))
             :clj
             (try (Long/parseLong x)
                  (catch NumberFormatException _
                    (try (long (Float/parseFloat x))
                         (catch NumberFormatException _ nil)))))))

  (defn as-?float #_as-?double [x]
    (cond (number? x) (double x)
          (string? x)
          #?(:cljs (let [x (js/parseFloat x)] (when-not (js/isNaN x) x))
             :clj  (try (Double/parseDouble x)
                        (catch NumberFormatException _ nil)))))

  (defn as-?udt       [x] (when-let [n (as-?int   x)] (when-not (neg? ^long   n) n)))
  (defn as-?nat-int   [x] (when-let [n (as-?int   x)] (when-not (neg? ^long   n) n)))
  (defn as-?pos-int   [x] (when-let [n (as-?int   x)] (when     (pos? ^long   n) n)))
  (defn as-?nat-float [x] (when-let [n (as-?float x)] (when-not (neg? ^double n) n)))
  (defn as-?pos-float [x] (when-let [n (as-?float x)] (when     (pos? ^double n) n)))

  (defn as-?pnum      [x] (when-let [^double f (as-?float x)] (if (> f 1.0) 1.0 (if (< f  0.0)  0.0 f))))
  (defn as-?rnum      [x] (when-let [^double f (as-?float x)] (if (> f 1.0) 1.0 (if (< f -1.0) -0.0 f))))

  (defn as-?bool [x]
    (cond
      (nil? x) nil
      (or (true? x) (false? x)) x
      (or (= x 0) (= x "false") (= x "FALSE") (= x "0")) false
      (or (= x 1) (= x "true")  (= x "TRUE")  (= x "1")) true))

  ;; Uses simple regex to test for basic "x@y.z" form:
  (let [regex #"^[^\s@]+@[^\s@]+\.\S*[^\.]$"]
    (defn as-?email
      ([        ?s] (as-?email 320 ?s))
      ([max-len ?s] (when-let [s (and ?s (str/trim ?s))]
                      (when (<= (count s) ^long max-len)
                        (re-find regex s))))))

  (defn as-?nemail
    ([        ?s] (when-let [email (as-?email         ?s)] (str/lower-case email)))
    ([max-len ?s] (when-let [email (as-?email max-len ?s)] (str/lower-case email))))

  (comment
    (do  (as-?nemail 11 "FOO@bar.com"))
    (mapv as-?nemail
      ["foo" "foo@" "foo@bar" "Foo@BAR.com"
       "foo@@bar.com" "foo@bar.com." "foo.baz@bar.com"])))

(declare assoc-some)

(defn- try-pred [pred x] (catching (pred x) _ false))

#?(:clj  (defn          when? [pred x] (when (try-pred pred x) x))
   :cljs (defn ^boolean when? [pred x] (when (try-pred pred x) x)))

(defn is!
  "Lightweight `have!` that provides less diagnostic info."
  ([     x     ] (is! some? x nil)) ; Nb different to single-arg `have`
  ([pred x     ] (is! pred  x nil))
  ([pred x data]
   (if (try-pred pred x)
     x
     (throw
       (ex-info (str "[encore/is!] " (str pred) " failed against arg: " (pr-str x))
         (assoc-some
           {:pred pred
            :arg  {:value x :type (type x)}}
           :data data))))))

(comment [(is! false) (is! nil) (is! string? 5) (when? string? "foo")])

(defn -as-throw [kind x]
  (throw
    (ex-info (str "[encore/as-" (name kind) "] failed against arg: " (pr-str x))
      {:pred-kind kind
       :arg {:value x :type (type x)}})))

(let [-as-throw -as-throw]
  (defn as-nzero             [x] (or (as-?nzero       x) (-as-throw :nzero       x)))
  (defn as-nblank            [x] (or (as-?nblank      x) (-as-throw :nblank      x)))
  (defn as-nblank-trim       [x] (or (as-?nblank-trim x) (-as-throw :nblank-trim x)))
  (defn as-nempty-str        [x] (or (as-?nempty-str  x) (-as-throw :nempty-str  x)))
  (defn as-kw                [x] (or (as-?kw          x) (-as-throw :kw          x)))
  (defn as-name              [x] (or (as-?name        x) (-as-throw :name        x)))
  (defn as-qname             [x] (or (as-?qname       x) (-as-throw :qname       x)))

  (defn as-email
    ([  x] (or (as-?email   x) (-as-throw :email x)))
    ([n x] (or (as-?email n x) (-as-throw :email x))))

  (defn as-nemail
    ([  x] (or (as-?nemail   x) (-as-throw :nemail x)))
    ([n x] (or (as-?nemail n x) (-as-throw :nemail x))))

  (defn as-udt         ^long [x] (or (as-?udt         x) (-as-throw :udt         x)))
  (defn as-int         ^long [x] (or (as-?int         x) (-as-throw :int         x)))
  (defn as-nat-int     ^long [x] (or (as-?nat-int     x) (-as-throw :nat-int     x)))
  (defn as-pos-int     ^long [x] (or (as-?pos-int     x) (-as-throw :pos-int     x)))
  (defn as-float     ^double [x] (or (as-?float       x) (-as-throw :float       x)))
  (defn as-nat-float ^double [x] (or (as-?nat-float   x) (-as-throw :nat-float   x)))
  (defn as-pos-float ^double [x] (or (as-?pos-float   x) (-as-throw :pos-float   x)))

  (defn as-pnum      ^double [x] (or (as-?pnum        x)         (-as-throw :pnum  x))) ; With auto coerce+clamp
  (defn as-rnum      ^double [x] (or (as-?rnum        x)         (-as-throw :rnum  x))) ; ''

  (defn as-pnum!     ^double [x] (if (pnum? x) (double x) (-as-throw :pnum! x))) ; Without auto coerce+clamp
  (defn as-rnum!     ^double [x] (if (rnum? x) (double x) (-as-throw :rnum! x))) ; ''

  (defn as-bool              [x] (let [?b (as-?bool   x)] (if-not (nil? ?b) ?b (-as-throw :bool x)))))

;;;; Validation

#?(:clj
   (defmacro check-some
     "Returns first logical false/throwing expression (id/form), or nil."
     ([test & more] `(or ~@(map (fn [test] `(check-some ~test)) (cons test more))))
     ([test       ]
      (let [[error-id test] (if (vector? test) test [nil test])]
        `(let [[test# err#] (catching [~test nil] err# [nil err#])]
           (when-not test# (or ~error-id '~test :check/falsey)))))))

#?(:clj
   (defmacro check-all
     "Returns all logical false/throwing expressions (ids/forms), or nil."
     ([test       ] `(check-some ~test))
     ([test & more]
      `(let [errors# (filterv identity
                       [~@(map (fn [test] `(check-some ~test)) (cons test more))])]
         (not-empty errors#)))))

(comment
  (check-some false [:bad-type (string? 0)] nil [:blank (str/blank? 0)])
  (check-all  false [:bad-type (string? 0)] nil [:blank (str/blank? 0)]))

;;;;

(defn unexpected-arg!
  "Throws runtime `ExceptionInfo` to indicate an unexpected argument.
  Takes optional kvs for merging into exception's data map.

    (let [mode :unexpected]
      (case mode
        :read  (do <...>)
        :write (do <...>)
        (unexpected-arg! mode
          :expected #{:read :write}))) =>

    Unexpected argument: :unexpected
    {:arg {:value :unexpected, :type clojure.lang.Keyword},
     :expected #{:read :write}}"

  {:added "v3.51.0 (2023-03-13)"}
  [arg & {:keys [msg] :as details}]
  (throw
    (ex-info (or msg (str "Unexpected argument: " arg))
      (let [m {:arg {:value arg, :type (type arg)}}]
        (if details
          (conj m details)
          (do   m))))))

(comment (unexpected-arg! :foo :expected '#{string?}))

(defn instance!
  "If (instance? class arg) is true, returns arg.
  Otherwise throws runtime `ExceptionInfo` with `unexpected-arg!`.
  See `unexpected-arg!` for more info."
  {:added "v3.51.0 (2023-03-13)"}
  [class arg & {:as details}]
  (if (instance? class arg)
    arg
    (unexpected-arg! arg
      (assoc details :expected `(~'instance? ~class ~'arg)))))

(comment (instance! String 5))

(defn satisfies!
  "If (satisfies? protocol arg) is true, returns arg.
  Otherwise throws runtime `ExceptionInfo` with `unexpected-arg!`.
  See `unexpected-arg!` for more info."
  {:added "v3.51.0 (2023-03-13)"}
  [protocol arg & {:as details}]
  (if (satisfies? protocol arg)
    arg
    (unexpected-arg! arg
      (assoc details :expected `(~'satisfies? ~protocol ~'arg)))))

;;;; Keywords

(defn explode-keyword [k] (str/split (as-qname k) #"[\./]"))
(comment (explode-keyword :foo.bar/baz))

(defn merge-keywords
  ([ks            ] (merge-keywords ks false))
  ([ks omit-slash?]
   (when (seq ks)
     (let [parts
           (reduce
             (fn [acc in]
               (if (nil? in)
                 acc
                 (reduce conj acc (explode-keyword in))))
             [] ks)]

       (when (seq parts)
         (if omit-slash?
           (keyword (str/join "." parts))
           (let [ppop (pop parts)]
             (keyword (when (seq ppop) (str/join "." ppop))
               (peek parts)))))))))

(comment (merge-keywords [:foo.bar nil "d.e/k" :baz.qux/end nil] true))

;;;; Bytes

#?(:clj
   (do
     (def ^:const bytes-class (Class/forName "[B"))
     (defn bytes? [x] (instance? bytes-class x)) ; Also introduced in Clojure v1.9-alpha5+
     (defn ba= [^bytes x ^bytes y] (java.util.Arrays/equals x y))
     (defn ba-hash      [^bytes x] (java.util.Arrays/hashCode x))

     ;; Java strings are UTF-16, but we'll use UTF-8 encoding when converting to/from bytes
     (defn utf8-ba->str {:added "v3.53.0 (2023-03-22)"} ^String [^bytes ba] (String. ba  java.nio.charset.StandardCharsets/UTF_8))
     (defn str->utf8-ba {:added "v3.53.0 (2023-03-22)"} ^bytes  [^String s] (.getBytes s java.nio.charset.StandardCharsets/UTF_8))

     (defn ba-concat ^bytes [^bytes ba1 ^bytes ba2]
       (let [l1  (alength ba1)
             l2  (alength ba2)
             out (byte-array (+ l1 l2))]
         (System/arraycopy ba1 0 out 0  l1)
         (System/arraycopy ba2 0 out l1 l2)
         (do                     out)))

     (defn ba-split [^bytes ba ^long idx]
       (if (zero? idx)
         [nil ba]
         (let [len (alength ba)]
           (when (> len idx)
             [(java.util.Arrays/copyOf      ba idx)
              (java.util.Arrays/copyOfRange ba idx len)]))))

     (comment
       (String. (ba-concat (.getBytes "foo") (.getBytes "bar")))
       (let [[x y] (ba-split (.getBytes "foobar") 5)] [(String. x) (String. y)]))

     (declare reduce-n)
     (defn const-ba=
       "Constant-time `ba=`.
       Useful to prevent timing attacks, etc."
       [ba1 ba2]
       (when (and ba1 ba2)
         (let [bax (byte-array [0 1])
               ^bytes ba1 ba1
               ^bytes ba2 ba2
               l1 (alength ba1)
               l2 (alength ba2)
               lmax (max l1 l2)
               lmin (min l1 l2)]

           (reduce-n
             (fn [acc ^long idx]
               (if (>= idx lmin)
                 (and (== (aget bax   0) (aget bax   1)) acc)
                 (and (== (aget ba1 idx) (aget ba2 idx)) acc)))
             true
             lmax))))))

(comment (const-ba= (byte-array [1 2 3 4]) (byte-array [])))

;;;; Reduce

(defn   convey-reduced [x] (if (reduced? x) (reduced x) x)) ; Double-wrap
(defn preserve-reduced "As `core/preserving-reduced`."
  [rf]
  (fn [acc in]
    (let [result (rf acc in)]
      (if (reduced? result)
        (reduced result)
        result))))

(defn reduce-kvs
  "Like `reduce-kv` but takes a flat sequence of kv pairs."
  [rf init kvs]
  (transduce (partition-all 2)
    (completing (fn [acc [k v]] (rf acc k v))) init kvs))

;; No longer so interesting with Clojure 1.7+
(defn reduce-n
  ([rf init       end     ] (reduce rf init (range       end)))
  ([rf init start end     ] (reduce rf init (range start end)))
  ([rf init start end step] (reduce rf init (range start end step))))

(comment (reduce-n conj [] 100 10 -1))

(declare counter)
(defn reduce-indexed
  "Like `reduce` but takes (rf [acc idx in]) with idx as in `map-indexed`.
    As `reduce-kv` against vector coll, but works on any seqable coll type."
  [rf init coll]
  (let [c (counter)]
    (reduce (fn [acc in] (rf acc (c) in)) init coll)))

(comment
  (reduce-indexed (fn [acc idx in] (assoc acc idx in)) {} [:a :b :c])
  (reduce-kv      (fn [acc idx in] (assoc acc idx in)) {} [:a :b :c]))

#?(:cljs
   (defn reduce-obj "Like `reduce-kv` but for JavaScript objects."
     [f init o]
     (reduce (fn [acc k] (f acc k (gobj/get o k nil))) init (js-keys o))))

(do
           (defn run!     [proc coll] (reduce     #(proc %2)    nil coll) nil)
           (defn run-kv!  [proc    m] (reduce-kv  #(proc %2 %3) nil    m) nil)
           (defn run-kvs! [proc  kvs] (reduce-kvs #(proc %2 %3) nil  kvs) nil)
  #?(:cljs (defn run-obj! [proc  obj] (reduce-obj #(proc %2 %3) nil  obj) nil)))

(do ; Faster variants using reduce/transduce
  (let [rf (fn [pred] (fn [_acc in] (when-let [p (pred in)] (reduced p))))]
    (defn rsome
      ([      pred coll] (reduce                      (rf pred)  nil coll))
      ([xform pred coll] (transduce xform (completing (rf pred)) nil coll))))

  (let [rf (fn [pred] (fn [_acc  k v]  (when-let [p (pred k v)] (reduced p))))
        tf (fn [pred] (fn [_acc [k v]] (when-let [p (pred k v)] (reduced p))))]
    (defn rsome-kv
        ([      pred coll] (reduce-kv                   (rf pred)  nil coll))
      #_([xform pred coll] (transduce xform (completing (tf pred)) nil coll))))

  (let [rf (fn [pred] (fn [_acc in] (when (pred in) (reduced in))))]
    (defn rfirst
      ([      pred coll] (reduce                      (rf pred)  nil coll))
      ([xform pred coll] (transduce xform (completing (rf pred)) nil coll))))

  (let [rf (fn [pred] (fn [_acc  k v]  (when (pred k v) (reduced [k v]))))
        tf (fn [pred] (fn [_acc [k v]] (when (pred k v) (reduced [k v]))))]
    (defn rfirst-kv
        ([      pred coll] (reduce-kv                   (rf pred)  nil coll))
      #_([xform pred coll] (transduce xform (completing (tf pred)) nil coll))))

  (let [rf (fn [pred] (fn [_acc in] (if (pred in) true (reduced false))))]
    (defn revery?
      ([      pred coll] (reduce                      (rf pred)  true coll))
      ([xform pred coll] (transduce xform (completing (rf pred)) true coll))))

  (let [rf (fn [pred] (fn [_acc  k v]  (if (pred k v) true (reduced false))))
        tf (fn [pred] (fn [_acc [k v]] (if (pred k v) true (reduced false))))]
    (defn revery-kv?
        ([      pred coll] (reduce-kv                   (rf pred)  true coll))
      #_([xform pred coll] (transduce xform (completing (tf pred)) true coll)))))

(comment
  (= (rfirst-kv (fn [k v] (number? v)) {:a :b :c 2}) [:c 2])

  (qb 1e4
    (some  #(when (string? %) %) [:a :b :c :d "boo"])
    (rsome #(when (string? %) %) [:a :b :c :d "boo"])
    (rfirst        string?       [:a :b :c :d "boo"])))

(defn reduce-zip
  "Reduces given sequential xs and ys as pairs (e.g. key-val pairs).
  Calls (rf acc x y) for each sequential pair.

  Useful, among other things, as a more flexible version of `zipmap`."
  {:added "v3.33.0 (2022-11-15)"}
  [rf init xs ys]
  (if (and
        (vector? xs)
        (vector? ys))

    (let [n (min (count xs) (count ys))]
      (reduce-n
        (fn [acc idx] (rf acc (get xs idx) (get ys idx)))
        init n))

    (loop [acc init
           xs (seq xs)
           ys (seq ys)]

      (if (and xs ys)
        (recur (rf acc (first xs) (first ys))
          (next xs)
          (next ys))
        acc))))

(comment :see-tests)

;;;; Math

(def ^:const max-long #?(:clj Long/MAX_VALUE :cljs  9007199254740991))
(def ^:const min-long #?(:clj Long/MIN_VALUE :cljs -9007199254740991))

(defn #?(:clj approx== :cljs ^boolean approx==)
  ([      x y] (< (Math/abs (- (double x) (double y))) 0.001))
  ([signf x y] (< (Math/abs (- (double x) (double y))) (double signf))))

(comment (qb 1e5 (approx== 0.01 3.141592 (/ 22 7))))

(defn clamp               [nmin nmax n]                                                             (if (< n nmin) nmin (if (> n nmax) nmax n))) ; Reflects
(defn clamp-int   ^long   [nmin nmax n] (let [nmin (long   nmin), nmax (long   nmax), n (long   n)] (if (< n nmin) nmin (if (> n nmax) nmax n))))
(defn clamp-float ^double [nmin nmax n] (let [nmin (double nmin), nmax (double nmax), n (double n)] (if (< n nmin) nmin (if (> n nmax) nmax n))))

(defn    pnum-complement ^double [pnum] (- 1.0 (double pnum)))
(defn as-pnum-complement ^double [x   ] (- 1.0 (as-pnum   x)))

#?(:clj
   (do ; These will pass primitives through w/o reflection
     (defmacro <=*    [x y z]       `(let [y# ~y] (and (<= ~x y#) (<= y# ~z))))
     (defmacro >=*    [x y z]       `(let [y# ~y] (and (>= ~x y#) (>= y# ~z))))
     (defmacro <*     [x y z]       `(let [y# ~y] (and (<  ~x y#) (<  y# ~z))))
     (defmacro >*     [x y z]       `(let [y# ~y] (and (>  ~x y#) (>  y# ~z))))
     (defmacro min*   [n1 n2]       `(let [n1# ~n1 n2# ~n2] (if (> n1# n2#) n2# n1#)))
     (defmacro max*   [n1 n2]       `(let [n1# ~n1 n2# ~n2] (if (< n1# n2#) n2# n1#)))
     (defmacro clamp* [nmin nmax n] `(let [nmin# ~nmin nmax# ~nmax n# ~n]
                                       (if (< n# nmin#) nmin# (if (> n# nmax#) nmax# n#))))))

(defn pow [n exp] (Math/pow n exp))
(defn abs [n]     (if (neg? n) (- n) n)) ; #?(:clj (Math/abs n)) reflects
(defn round* ; round
  ([             n] (round* :round nil n))
  ([kind         n] (round* kind   nil n))
  ([kind nplaces n]
   (let [n        (double n)
         modifier (when nplaces (Math/pow 10.0 nplaces))
         n*       (if modifier (* n ^double modifier) n)
         rounded
         (case kind
           ;;; Note same API for both #?(:clj _ :cljs: _)
           :round (Math/round n*) ; Round to nearest int or nplaces
           :floor (Math/floor n*) ; Round down to -inf
           :ceil  (Math/ceil  n*) ; Round up to +inf
           :trunc (long n*)       ; Round up/down toward zero
           (throw
             (ex-info "[encore/round*] Unexpected round kind (expected ∈ #{:round :floor :ceil :trunc})"
               {:kind {:value kind :type (type kind)}})))]

     (if-not modifier
       (long rounded)                        ; Returns long
       (/ (double rounded) ^double modifier) ; Returns double
       ))))

(comment
  [(round* :floor -1.5)
   (round* :trunc -1.5)
   (round* :floor 5 1.1234567)
   (round* :round 5 1.1234567)])

(do ; Optimized common cases
  (defn round0   ^long [n]            (Math/round    (double n)))
  (defn round1 ^double [n] (/ (double (Math/round (* (double n)  10.0)))  10.0))
  (defn round2 ^double [n] (/ (double (Math/round (* (double n) 100.0))) 100.0))
  (defn perc     ^long [n divisor] (Math/round (* (/ (double n) (double divisor)) 100.0))))

(defn exp-backoff "Returns binary exponential backoff value for n<=36."
  ([^long n-attempt] (exp-backoff n-attempt nil))
  ([^long n-attempt {:keys [min max factor] :or {factor 1000}}]
   (let [n (if (> n-attempt 36) 36 n-attempt) ; >2^36 excessive
         b (Math/pow 2 n)
         t (long (* (+ b ^double (rand b)) 0.5 (double factor)))
         t (long (if min (if (< t ^long min) min t) t))
         t (long (if max (if (> t ^long max) max t) t))]
     t)))

(comment (exp-backoff 128))

(defn chance [p] (< ^double (rand) (double p)))

(comment (chance 0.25))

;;;; Misc

;; js/foo      - `foo` in global object/ns (depends on *target*)
;; js/window   - `window` object: global ns in browsers
;; js/global   - `global` object: global ns in Node.js, etc.?
;; goog/global - Closure's environment-agnostic global object
;;
#?(:cljs (def node-target? (= *target* "nodejs")))
#?(:cljs (def js-?win (when (exists? js/window) js/window)))

(defn force-ref "Like `force` for refs." [x] (if (derefable? x) (deref x) x))
(defn force-var "Like `force` for vars." [x] (if (var?       x) (deref x) x))
(defn merge-meta   [x m] (with-meta x (merge (meta x) m)))
(defn without-meta [x] (if (meta x) (with-meta x nil) x))

(defn #?(:clj some= :cljs ^boolean some=)
  ([x y]        (and (some? x) (= x y)))
  ([x y & more] (and (some? x) (= x y) (revery? #(= % x) more))))

(comment (some= :foo :foo nil))

(defn nnil "Returns first non-nil arg, or nil."
  ([            ] nil)
  ([x           ] x)
  ([x y         ] (if (nil? x) y x))
  ([x y z       ] (if (nil? x) (if (nil? y) z y) x))
  ([x y z & more] (if (nil? x) (if (nil? y) (if (nil? z) (rfirst some? more) z) y) x)))

(comment
  (qb 1e6
    (or   nil nil nil false :a)
    (nnil nil nil nil false :a)))

(defn parse-version [x]
  (let [[s-version ?s-qualifier] (str/split (str x) #"-" 2)]
    {:version   (when-let [s (re-seq #"\d+" s-version)] (mapv as-?int s))
     :qualifier (when-let [s ?s-qualifier] (str/lower-case s))}))

(comment [(parse-version "40.32.34.8-foo") (parse-version 10.3)])

(defn assert-min-encore-version
  "Version check for dependency conflicts, etc."
  [min-version]
  (let [[xc yc zc] encore-version
        [xm ym zm] (if (vector? min-version) min-version (:version (parse-version min-version)))
        [xm ym zm] (mapv #(or % 0) [xm ym zm])]

    (when-not (or (> xc xm) (and (= xc xm) (or (> yc ym) (and (= yc ym) (>= zc zm)))))
      (throw
        (ex-info "Insufficient `com.taoensso/encore` version, you may have a dependency conflict: see https://www.taoensso.com/dependency-conflicts for solutions."
          {:min-version  (str/join "." [xm ym zm])
           :your-version (str/join "." [xc yc zc])})))))

(comment (assert-min-encore-version 3.10))

;;;; Collections

#?(:clj  (defn          queue? [x] (instance? clojure.lang.PersistentQueue x))
   :cljs (defn ^boolean queue? [x] (instance?    cljs.core.PersistentQueue x)))

(defn queue "Returns a PersistentQueue."
  ([coll] (into (queue) coll))
  ([] #?(:clj  clojure.lang.PersistentQueue/EMPTY
         :cljs    cljs.core.PersistentQueue.EMPTY)))

(defn queue* [& items] (queue items))
(defn ensure-vec [x] (if (vector? x) x (vec x)))
(defn ensure-set [x] (if (set?    x) x (set x)))

#?(:cljs
   (defn oset "Like `assoc` for JS objects."
     [o k v] (gobj/set (if (nil? o) (js-obj) o) (name k) v)))

#?(:cljs
   (let [sentinel (js-obj)]
     (defn oset-in
       "Experimental. Like `assoc-in` for JS objects."
       [o ks v]
       (let [o (if (nil? o) (js-obj) o)]
         (if-let [ks (seq ks)]
           (loop [o-next o, ks-next ks]
             (let [k1 (name (first ks-next))
                   o-next
                   (let [o-next* (gobj/get o-next k1 sentinel)]
                     (if (identical? o-next* sentinel)
                       (let [new-obj (js-obj)]
                         (do
                           (gobj/set o-next k1 new-obj)
                           (do                 new-obj)))
                       o-next*))]

               (if-let [ks-next (next ks-next)]
                 (recur        o-next ks-next)
                 (do (gobj/set o-next k1 v) o))))
           ;; Resolve nil => [nil] ambiguity in `assoc-in`
           o)))))

#?(:cljs
   (defn oget "Like `get` for JS objects."
     ([  k          ] (gobj/get js/window (name k)))
     ([o k          ] (gobj/get o         (name k) nil))
     ([o k not-found] (gobj/get o         (name k) not-found))))

#?(:cljs
   (let [sentinel (js-obj)]
     ;; Could also use `gobg/getValueByKeys`
     (defn oget-in "Like `get-in` for JS objects."
       ([  ks] (oget-in js/window ks nil))
       ([o ks] (oget-in o         ks nil))
       ([o ks not-found]
        (loop [o o
               ks (seq ks)]
          (if ks
            (let [o (gobj/get o (name (first ks)) sentinel)]
              (if (identical? o sentinel)
                not-found
                (recur o (next ks))))
            o))))))

(do
  (defn conj-some "Conjoins each non-nil value."
    ([             ] [])
    ([coll         ] coll)
    ([coll x       ] (if (nil? x) coll (conj coll x)))
    ([coll x & more] (reduce conj-some (conj-some coll x) more)))

  (defn conj-when "Conjoins each truthy value."
    ([             ] [])
    ([coll         ] coll)
    ([coll x       ] (if x (conj coll x) coll))
    ([coll x & more] (reduce conj-when (conj-when coll x) more))))

(comment (conj-some [] :a :b nil :c :d nil false :e))

(do
  (defn assoc-some "Assocs each kv iff its value is not nil."
    ([m k v      ] (if (nil? v) (if (nil? m) {} m) (assoc m k v)))
    ([m k v & kvs]
     (reduce-kvs
       (fn [m k v] (if (nil? v) m (assoc m k v)))
       (assoc-some m k v)
       kvs))

    ([m kvs]
     (reduce-kv
       (fn [m k v] (if (nil? v) m (assoc m k v)))
       (if (nil? m) {} m)
       kvs)))

  (defn assoc-when "Assocs each kv iff its val is truthy."
    ([m k v      ] (if-not v (if (nil? m) {} m) (assoc m k v)))
    ([m k v & kvs]
     (reduce-kvs
       (fn [m k v] (if-not v m (assoc m k v)))
       (assoc-when m k v)
       kvs))

    ([m kvs]
     (reduce-kv
       (fn [acc k v] (if-not v m (assoc m k v)))
       (if (nil? m) {} m)
       kvs)))

  (defn dis-assoc-some
    "Assocs each kv if its value is not nil, otherwise dissocs it."
    ([m k v      ] (if (nil? v) (if (nil? m) {} (dissoc m k)) (assoc m k v)))
    ([m k v & kvs]
     (reduce-kvs
       (fn [m k v] (if (nil? v) (dissoc m k) (assoc m k v)))
       (dis-assoc-some m k v)
       kvs))

    ([m kvs]
     (reduce-kv
       (fn [m k v] (if (nil? v) (dissoc m k) (assoc m k v)))
       (if (nil? m) {} m)
       kvs)))

  ;; Handy as l>r merge
  (defn assoc-nx "Assocs each kv iff its key doesn't already exist."
    ([m k v] (if (contains? m k) m (assoc m k v)))
    ([m k v & kvs] (reduce-kvs assoc-nx (assoc-nx m k v) kvs))
    ([m kvs]
     (reduce-kv
       (fn [m k v] (if (contains? m k) m (assoc m k v)))
       (if (nil? m) {} m)
       kvs))))

(comment
  (assoc-some {:a :A} :b nil :c :C :d nil :e :E)
  (assoc-some {:a :A} {:b :B :c nil :d :D :e false})
  (dis-assoc-some {:a :A :b :B} {:a :a :b nil})
  (reduce-kv assoc-nx {:a :A} {:a :a :b :b}))

(defn get-subvec
  "Like `subvec` but never throws (snaps to valid start and end indexes)."
  ([v ^long start]
   (let [start (if (< start 0) 0 start)
         vlen  (count v)]
     (if (>= start vlen)
       []
       (subvec v start vlen))))

  ([v ^long start ^long end]
   (let [start (if (< start 0) 0 start)
         vlen  (long (count v))
         end   (if (> end vlen) vlen end)]
     (if (>= start end)
       []
       (subvec v start end)))))

(defn get-subvector
  "Like `get-subvec` but:
    - Takes `length` instead of `end` (index).
    - -ive `start` => index from right of vector."
  ([v ^long start]
   (let [vlen (count v)]
     (if (< start 0)
       (let [start (+ start vlen)
             start (if (< start 0) 0 start)]
         (subvec v start vlen))
       (if (>= start vlen)
         []
         (subvec v start vlen)))))

  ([v ^long start ^long length]
   (if (<= length 0)
     []
     (let [vlen (long (count v))]
       (if (< start 0)
         (let [start (+ start vlen)
               start (if (< start 0) 0 start)
               end   (+ start length)
               end   (if (> end vlen) vlen end)]
           (subvec v start end))

         (let [end (+ start length)
               end (if (> end vlen) vlen end)]
           (if (>= start end)
             []
             (subvec v start end))))))))

(comment
  [(get-subvec    nil 2)
   (get-subvector nil 2)]

  (qb 1e6
    (subvec        [:a :b :c] 1)
    (get-subvec    [:a :b :c] 1)
    (get-subvector [:a :b :c] 1))
  ;; [60.01 63.91 58.6]
  )

(defn vnext        [v] (when (> (count v) 1) (subvec v 1)))
(defn vrest        [v] (if   (> (count v) 1) (subvec v 1) []))
(defn vsplit-last  [v] (let [c (count v)] (when (> c 0) [(when (> c 1) (pop v)) (peek v)])))
(defn vsplit-first [v] (let [c (count v)] (when (> c 0) (let [[v1] v] [v1 (when (> c 1) (subvec v 1))]))))

(comment
  (vsplit-first [:a :b :c])
  (vsplit-last  [:a :b :c]))

(defn- fsplit-last
  "Faster (f (vec (butlast xs)) (last x))."
  [xs f]
  (if (vector? xs)
    (let [[vn vl] (vsplit-last xs)] (f vn vl))
    (loop [butlast [] xs xs]
      (let [[x1 & xn] xs]
        (if xn
          (recur (conj butlast x1) xn)
          (f butlast x1))))))

(comment (let [v [:a :b :c :d]] (qb 1e6 (fsplit-last v vector) [(butlast v) (last v)])))

(defn takev [n coll] (if (vector? coll) (get-subvector coll 0 n) (into [] (take n) coll)))

(defn #?(:clj distinct-elements? :cljs ^boolean distinct-elements?)
  [x] (or (set? x) (= (count x) (count (ensure-set x)))))

(def seq-kvs "(seq-kvs {:a :A}) => (:a :A)." (partial reduce concat))
(defn mapply "Like `apply` but calls `seq-kvs` on final arg."
  [f & args] (apply f (fsplit-last args (fn [xs lx] (concat xs (seq-kvs lx))))))

(comment [(seq-kvs {:a :A :b :B}) (mapply str 1 2 3 {:a :A})])

(defn into-all "Like `into` but supports multiple \"from\"s."
  ([to from       ] (into to from))
  ([to from & more]
   (persistent!
     (reduce (fn [acc in] (reduce conj! acc in))
       (transient to)
       (cons from more)))))

(defn repeatedly-into
  "Like `repeatedly` but faster and `conj`s items into given collection."
  [coll ^long n f]
  (if (and (> n 10) (editable? coll))
    (persistent! (reduce-n (fn [acc _] (conj! acc (f))) (transient coll) n))
    (do          (reduce-n (fn [acc _] (conj  acc (f)))            coll  n))))

(comment (repeatedly-into [] 100 (partial rand-nth [1 2 3 4 5 6])))

(defn into!
  ([to       from] (reduce          conj! to from))
  ([to xform from] (transduce xform conj! to from)))

(defn xdistinct
  ([] (distinct)) ; core now has a distinct transducer
  ([keyfn]
   (fn [rf]
     (let [seen_ (volatile! (transient #{}))]
       (fn
         ([]    (rf))
         ([acc] (rf acc))
         ([acc input]
          (let [k (keyfn input)]
            (if (contains? @seen_ k)
              acc
              (do (vswap! seen_ conj! k)
                  (rf acc input))))))))))

(comment (into [] (xdistinct) [1 2 3 1 4 5 2 6 7 1]))

(let [p! persistent!, t transient] ; Note `mapv`-like nil->{} semantics
  (defn invert-map       [m]                 (p! (reduce-kv (fn [m k v] (assoc! m v    k))  (t {}) m)))
  (defn map-keys       [f m]                 (p! (reduce-kv (fn [m k v] (assoc! m (f k) v)) (t {}) m)))
  (defn map-vals       [f m] (if (nil? m) {} (p! (reduce-kv (fn [m k v] (assoc! m k (f v))) (t  m) m))))
  (defn filter-keys [pred m] (if (nil? m) {} (p! (reduce-kv (fn [m k v] (if (pred k) m (dissoc! m k))) (t m) m))))
  (defn filter-vals [pred m] (if (nil? m) {} (p! (reduce-kv (fn [m k v] (if (pred v) m (dissoc! m k))) (t m) m))))
  (defn remove-keys [pred m] (if (nil? m) {} (p! (reduce-kv (fn [m k v] (if (pred k) (dissoc! m k) m)) (t m) m))))
  (defn remove-vals [pred m] (if (nil? m) {} (p! (reduce-kv (fn [m k v] (if (pred v) (dissoc! m k) m)) (t m) m)))))

(defn rename-keys
  "Returns a map like the one given, replacing keys using
  the given {<old-new> <new-key>} replacements.
  O(min(n_replacements, n_m))."
  [replacements m]
  (cond
    (nil?   m) {}
    (empty? m) m ; Preserve metadata
    (empty? replacements) m

    (> (count m) (count replacements))
    (persistent!
      (reduce-kv
        (fn [acc old-k new-k]
          (if-let [e (find m old-k)]
            (assoc! (dissoc! acc old-k) new-k (val e))
            (do              acc)))
        (transient m)
        replacements))

    :else
    (persistent!
      (reduce-kv
        (fn [acc old-k v]
          (if-let [e (find replacements old-k)]
            (assoc! (dissoc! acc old-k) (val e) v)
            (do              acc)))
        (transient m)
        (do        m)))))

(comment (rename-keys {:a :X} {:a :A :b :B :c :C}))

(defn keys-by
  "Returns {(f x) x} map for xs in `coll`."
  [f coll]
  (persistent!
    (reduce (fn [acc x] (assoc! acc (f x) x))
      (transient {}) coll)))

(comment (keys-by :foo [{:foo 1} {:foo 2}]))

(do
  (defn #?(:clj ks=      :cljs ^boolean ks=)      [ks m] (=             (set (keys m)) (ensure-set ks)))
  (defn #?(:clj ks<=     :cljs ^boolean ks<=)     [ks m] (set/subset?   (set (keys m)) (ensure-set ks)))
  (defn #?(:clj ks>=     :cljs ^boolean ks>=)     [ks m] (set/superset? (set (keys m)) (ensure-set ks)))
  (defn #?(:clj ks-nnil? :cljs ^boolean ks-nnil?) [ks m] (revery?     #(some? (get m %))     ks)))

(comment
  (ks=      #{:a :b} {:a :A :b :B  :c :C})
  (ks<=     #{:a :b} {:a :A :b :B  :c :C})
  (ks>=     #{:a :b} {:a :A :b :B  :c :C})
  (ks-nnil? #{:a :b} {:a :A :b :B  :c nil})
  (ks-nnil? #{:a :b} {:a :A :b nil :c nil}))

(declare dissoc-in)
(defn update-in
  "Like `core/update-in` but:.
    - Empty ks will return (f m), not act like [nil] ks.
    - Adds support for `not-found`.
    - Adds support for special return vals: `:swap/dissoc`, `:swap/abort`."
  ([m ks           f] (update-in m ks nil f))
  ([m ks not-found f]
   (if (empty? ks)
     (f m) ; m would also be a sensible choice, but (f m) is more useful
     (let [old (get-in m ks not-found)
           new (f old)]
       (cond
         (kw-identical? new :swap/abort) m
         (kw-identical? new :swap/dissoc)
         (fsplit-last ks
           (fn [ks lk]
             (update-in m ks nil
               (fn [v]
                 (if v ; Assume associative
                   (dissoc v lk)
                   :swap/abort)))))

         :else (assoc-in m ks new))))))

(comment :see-tests)

(defn #?(:clj contains-in? :cljs ^boolean contains-in?)
  ([coll ks k] (contains? (get-in coll ks) k))
  ([coll ks  ]
   (if (empty? ks)
     false
     (fsplit-last ks (fn [ks lk] (contains-in? coll ks lk))))))

(comment :see-tests)

(defn dissoc-in
  ([m ks dissoc-k       ] (update-in m ks nil (fn [m] (if m (dissoc m dissoc-k) :swap/abort))))
  ([m ks dissoc-k & more]
   (update-in m ks nil
     (fn [m]
       (if m
         (reduce dissoc (dissoc m dissoc-k) more)
         :swap/abort))))

  ([m ks]
   (if (empty? m)
     (do       m)
     (fsplit-last ks (fn [ks lk] (dissoc-in m ks lk))))))

(comment :see-tests)

(defn node-paths
  ([          m      ] (node-paths associative? m nil))
  ([node-pred m      ] (node-paths node-pred    m nil))
  ([node-pred m basis]
   (let [basis (or basis [])]
     (persistent!
       (reduce-kv
         (fn [acc k v]
           (if-not (node-pred v)
             (conj! acc (conj basis k v))
             (let [paths-from-basis (node-paths node-pred v (conj basis k))]
               (reduce (fn [acc in] (conj! acc in)) acc paths-from-basis))))
         (transient [])
         m)))))

(comment
  (node-paths associative? {:a1 :A1 :a2 {:b1 :B1 :b2 {:c1 :C1 :c2 :C2}}} [:h])
  (node-paths [:a1 :a2 [:b1 :b2 [:c1 :c2] :b3] :a3 :a4]))

(defn interleave-all "Greedy version of `interleave`."
  ([     ] '())
  ([c1   ] (lazy-seq c1))
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

(defn vinterleave-all [c1 c2]
  (loop [v (transient []) s1 (seq c1) s2 (seq c2)]
    (cond
      (and s1 s2)
      (recur (conj! (conj! v (first s1)) (first s2)) (next s1) (next s2))
      s1    (persistent! (reduce conj! v s1))
      s2    (persistent! (reduce conj! v s2))
      :else (persistent! v))))

(comment
  (qb 1e5
    (vec (interleave-all [:a :b :c :d] [:a :b :c :d :e]))
        (vinterleave-all [:a :b :c :d] [:a :b :c :d :e])))

#?(:clj (defmacro new-object [] `(if-cljs (cljs.core/js-obj) (Object.))))

(let [not-found (new-object)]
  (defn -merge-with [nest? f maps]
    (reduce
      (fn [acc in]
        (if (nil? in)
          acc
          (reduce-kv
            (fn rf2 [acc k rv]
              (let [lv (get acc k not-found)]
                (cond
                  (identical? lv not-found)
                  (assoc acc k rv)

                  (kw-identical? rv :swap/dissoc)
                  (dissoc acc k)

                  (and nest? (map? rv) (map? lv))
                  (assoc acc k (reduce-kv rf2 lv rv))

                  :else
                  (let [new-rv (f lv rv)]
                    (if (kw-identical? new-rv :swap/dissoc)
                      (dissoc acc k)
                      (assoc  acc k new-rv))))))
            (or acc {})
            in)))
      nil
      maps)))

(do
  (defn merge "Like `core/merge` but faster, supports `:swap/dissoc` rvals."
    [& maps] (-merge-with false (fn [x y] y) maps))

  (defn merge-with "Like `core/merge-with` but faster, supports `:swap/dissoc` rvals."
    [f & maps] (-merge-with false f maps))

  (defn nested-merge "Like `merge` but does nested merging."
    [& maps] (-merge-with :nest (fn [x y] y) maps))

  (defn nested-merge-with "Like `merge-with` but does nested merging."
    [f & maps] (-merge-with :nest f maps)))

(comment
  [(nested-merge nil nil nil)
   (nested-merge nil nil {})
   (nested-merge
     {:a1 :A1 :b1 :B1  :c1 {:a2 :A2 :b2 {:a3 :A3 :b3 :B3  :d1 :D1 :e1 :E1}}}
     {        :b1 :B1* :c1 {        :b2 {        :b3 :B3* :d1 nil :e1 :swap/dissoc}}}
     nil
     {})]
  [nil {} {:a1 :A1, :b1 :B1*, :c1 {:a2 :A2, :b2 {:a3 :A3, :b3 :B3*, :d1 nil}}}])

(defn submap?
  "Returns true iff `sub` is a (possibly nested) submap of `m`,
  i.e. iff every (nested) value in `sub` has the same (nested) value in `m`.

  Warning: uses stack recursion, so supports only limited nesting."
  [m sub]
  (reduce-kv
    (fn [_ k v]
      (if (map? v)
        (let [sub* v
              m* (get m k)]
          (if-let [match? (and (map? m*) (submap? m* sub*))]
            true
            (reduced false)))

        (let [sval v
              mval (get m k ::nx)]

          (if-let [match?
                   (cond
                     ;;; Special svals currently undocumented
                     (kw-identical? sval :submap/nx)        (kw-identical? mval ::nx)
                     (kw-identical? sval :submap/ex)   (not (kw-identical? mval ::nx))
                     (kw-identical? sval :submap/some)              (some? mval)
                     :else (= sval mval))]
            true
            (reduced false)))))
    true
    sub))

(comment :see-tests)

(defn select-nested-keys
  "Like `select-keys` but supports nested key spec:

    (select-nested-keys
      {:a :A :b :B :c {:c1 :C1 :c2 :C2} :d :D} ; `src-map`
      [:a {:c [:c1], :d [:d1 :d2]}]) ; `key-spec`

      => {:a :A, :c {:c1 :C1}, :d :D}

  Note that as with the `{:d [:d1 :d2]}` spec in the example above,
  if spec expects a nested map but the actual value is not a map,
  the actual value will be included in output as-is.

  Has the same behaviour as `select-keys` when `key-spec` is a
  simple vector of keys."

  {:added "v3.34.0 (2022-11-16)"}
  [src-map key-spec]
  (persistent!
    (reduce
      (fn rf [acc spec-in]
        (if (map? spec-in)

          (reduce-kv
            (fn [acc k nested-spec-in]
              (if (contains? src-map k)
                (let [src-val (get src-map k)]
                  (if (map? src-val)
                    (assoc! acc k (select-nested-keys src-val nested-spec-in))
                    (assoc! acc k                     src-val)))
                acc))
            acc spec-in)

         (let [k spec-in]
           (if (contains? src-map k)
             (assoc!  acc k (get src-map k))
             (do      acc)))))

      (transient {}) key-spec)))

(comment :see-tests)
(comment
  (qb 1e5 ; [18.86 22.74]
    (select-nested-keys  {:a 1 :b 1 :c 1} [:a :c])
    (select-keys         {:a 1 :b 1 :c 1} [:a :c])))

;;;; Swap API
;; - reset-in!   ; Keys: 0, 1, n (general)
;; - reset-val!  ; Keys:    1    (optimized)
;;
;; - reset-in!?  ; Keys: 0, 1, n (general)
;; - reset-val!? ; Keys:    1    (optimized)
;; - reset!?     ; Keys: 0       (optimized)
;;
;; - swap-in!    ; Keys: 0, 1, n (general)
;; - swap-val!   ; Keys:    1    (optimized)
;;
;; - pull-val!   ; Keys:    1    (optimized, common transform)

(compile-if clojure.lang.IAtom
  (def ^:private ^:const atom-tag 'clojure.lang.IAtom)
  (def ^:private ^:const atom-tag 'clojure.lang.Atom))

#?(:clj
   (defmacro -if-cas! "Micro optimization, mostly for cljs."
     [atom_ old-val new-val then & [?else]]
     `(if-cljs
        (do (reset! ~atom_ ~new-val) ~then)
        (if (.compareAndSet ~(with-meta atom_ {:tag atom-tag}) ~old-val ~new-val)
          ~then
          ~?else))))

(defn- -reset-k0!
  "Impln. for 0-key resets"
  [return atom_ m1]
  (loop []
    (let [m0 @atom_]
      (-if-cas! atom_ m0 m1
        (return m0 m0 m1 m1) ; [m0 v0 m1 v1]
        (recur)))))

(defn- -reset-k1!
  "Impln. for 1-key resets"
  [return atom_ k not-found v1]
  (loop []
    (let [m0 @atom_
          m1 (assoc m0 k v1)]
      (-if-cas! atom_ m0 m1
        (return m0 (get m0 k not-found) m1 v1) ; [m0 v0/nx m1 v1]
        (recur)))))

(defn- -reset-kn!
  "Impln. for n-key resets"
  [return atom_ ks not-found v1]
  (if-let [ks-seq (seq ks)]
    (if (next ks-seq)
      (loop []
        (let [m0 @atom_
              m1 (assoc-in m0 ks v1)]
          (-if-cas! atom_ m0 m1
            (return m0 (get-in m0 ks not-found) m1 v1) ; [m0 v0/nx m1 v1]
            (recur))))

      (-reset-k1! return atom_ (nth ks 0) not-found v1))
    (-reset-k0!   return atom_                      v1)))

(let [return (fn [m0 v0 m1 v1] v0)]

  (defn reset-in! ; General case
    "Like `reset!` but supports `update-in` semantics, returns <old-key-val>."
    ([atom_              val] (-reset-k0! return atom_              val))
    ([atom_ ks           val] (-reset-kn! return atom_ ks nil       val))
    ([atom_ ks not-found val] (-reset-kn! return atom_ ks not-found val)))

  (defn reset-val! ; Optimized k1 case
    "Like `reset-in!` but optimized for single-key case."
    ([atom_ k           val] (-reset-k1! return atom_ k nil       val))
    ([atom_ k not-found val] (-reset-k1! return atom_ k not-found val))))

(let [sentinel (new-object)
      return (fn [m0 v0 m1 v1] (not= v0 v1))]
  
  (defn reset-in!? ; Keys: 0, 1, n (general)
    "Like `reset-in!` but returns true iff the atom's value changed."
    ([atom_              val] (-reset-k0! return atom_              val))
    ([atom_ ks           val] (-reset-kn! return atom_ ks sentinel  val))
    ([atom_ ks not-found val] (-reset-kn! return atom_ ks not-found val)))

  (defn reset-val!? ; Keys: 1 (optimized)
    "Like `reset-in!?` but optimized for single-key case."
    [atom_ k new-val]
    (let [v0 (reset-val! atom_ k sentinel new-val)]
      (not= v0 new-val))))

(comment
  (reset-in!? (atom :a) :b)
  (reset-in!? (atom {:a :A}) [:b] :B))

(defn reset!? ; Keys: 0 (optimized)
  "Atomically swaps value of `atom_` to `val` and returns
  true iff the atom's value changed. See also `reset-in!?`."
  [atom_ val]
  (loop []
    (let [old @atom_]
      (-if-cas! atom_ old val
        (not= old val)
        (recur)))))

(comment (let [a (atom nil)] [(reset!? a "foo") (reset!? a "foo") (reset!? a "bar")]))

(do
  (deftype       Swapped [newv    returnv])
  (defn swapped ^Swapped [new-val return-val] (Swapped. new-val return-val))
  (defn swapped-vec [x]
    (if (instance? Swapped x)
      [(.-newv ^Swapped x) (.-returnv ^Swapped x)]
      [x x]))

  #?(:clj  (defn          swapped? [x] (instance? Swapped x))
     :cljs (defn ^boolean swapped? [x] (instance? Swapped x)))

  (comment (qb 1e6 (.-newv (swapped "new" "return")))))

(defn- return-swapped [^Swapped sw m0 m1]
  (let [rv (.-returnv sw)]
    (case rv
      :swap/changed? (not= m1 m0)
      :swap/new            m1
      :swap/old               m0
      rv)))

(defn- -swap-k0!
  "Impln. for 0-key swaps"
  [return atom_ f]
  (loop []
    (let [m0  @atom_
          s1  (f m0)
          sw? (instance? Swapped s1)
          m1  (if sw? (.-newv ^Swapped s1) s1)]

      (if (kw-identical? m1 :swap/abort)
        (if sw?
          (return-swapped s1 m0 m1) ; rv
          (return m0 m0 m0 m0)) ; [m0 v0 m1 v1]

        (-if-cas! atom_ m0 m1
          (if sw?
            (return-swapped s1 m0 m1) ; rv
            (return m0 m0 m1 m1)) ; [m0 v0 m1 v1]
          (recur))))))

(defn- -swap-k1!
  "Impln. for 1-key swaps"
  [return atom_ k not-found f]
  (if (kw-identical? f :swap/dissoc)
    (loop []
      (let [m0 @atom_
            m1 (dissoc m0 k)]
        (-if-cas! atom_ m0 m1
          (return m0 (get m0 k not-found) m1 :swap/dissoc) ; [m0 v0/nx m1 v1]
          (recur))))

    (loop []
      (let [m0  @atom_
            v0  (get m0 k not-found) ; nx
            s1  (f v0)
            sw? (instance? Swapped s1)
            v1  (if sw? (.-newv ^Swapped s1) s1)]

        (if (kw-identical? v1 :swap/abort)
          (if sw?
            (return-swapped s1 m0 m0) ; rv
            (return m0 v0 m0 v0)) ; [m0 v0/nx m1 v1]

          (let [m1
                (if (kw-identical? v1 :swap/dissoc)
                  (dissoc m0 k)
                  (assoc  m0 k v1))]

            (-if-cas! atom_ m0 m1
              (if sw?
                (return-swapped s1 m0 m1) ; rv
                (return m0 v0 m1 v1)) ; [m0 v0/nx m1 v1]
              (recur))))))))

(defn- -swap-kn!
  "Impln. for n-key swaps"
  [return atom_ ks not-found f]
  (if-let [ks-seq (seq ks)]
    (if (next ks-seq)

      (if (kw-identical? f :swap/dissoc)
        (loop []
          (let [m0 @atom_
                m1 (dissoc-in m0 ks)]
            (-if-cas! atom_ m0 m1
              (return m0 (get-in m0 ks not-found) m1 :swap/dissoc) ; [m0 v0/nx m1 v1]
              (recur))))

        (loop []
          (let [m0  @atom_
                v0  (get-in m0 ks not-found) ; nx
                s1  (f v0)
                sw? (instance? Swapped s1)
                v1  (if sw? (.-newv ^Swapped s1) s1)]

            (if (kw-identical? v1 :swap/abort)
              (if sw?
                (return-swapped s1 m0 m0) ; rv
                (return m0 v0 m0 v0)) ; [m0 v0/nx m1 v1]

              (let [m1
                    (if (kw-identical? v1 :swap/dissoc)
                      (dissoc-in m0 ks)
                      (assoc-in  m0 ks v1))]

                (-if-cas! atom_ m0 m1
                  (if sw?
                    (return-swapped s1 m0 m1) ; rv
                    (return m0 v0 m1 v1)) ; [m0 v0/nx m1 v1]
                  (recur)))))))

      (-swap-k1! return atom_ (nth ks 0) not-found f))
    (-swap-k0!   return atom_                      f)))

(let [return (fn [m0 v0 m1 v1] v1)]
  (defn swap-in! ; Keys: 0, 1, n (general)
    "Like `swap!` but supports `update-in` semantics,
    returns <new-key-val> or <swapped-return-val>."
    ([atom_              f] (-swap-k0! return atom_              f))
    ([atom_ ks           f] (-swap-kn! return atom_ ks nil       f))
    ([atom_ ks not-found f] (-swap-kn! return atom_ ks not-found f)))

  (defn swap-val! ; Keys: 1 (optimized)
    "Like `swap-in!` but optimized for single-key case."
    ([atom_ k           f] (-swap-k1! return atom_ k nil       f))
    ([atom_ k not-found f] (-swap-k1! return atom_ k not-found f))))

(comment
  [(let [a_ (atom {:a :A :b :B})] [(swap-in! a_ [  ] (fn [m] (assoc m :c :C))) @a_])
   (let [a_ (atom {:a :A :b :B})] [(swap-in! a_ [  ] (fn [m] (swapped (assoc m :c :C) m))) @a_])
   (let [a_ (atom {:a {:b :B}})]  [(swap-in! a_ [:a] (fn [m] (assoc m :c :C))) @a_])
   (let [a_ (atom {:a {:b :B}})]  [(swap-in! a_ [:a] (fn [m] (swapped (assoc m :c :C) m))) @a_])
   (let [a_ (atom {:a {:b 100}})]  (swap-in! a_ [:a :b] inc)) ; => 101
   (let [a_ (atom {:a {:b :b1 :c :c1} :d :d1})] (swap-in! a_ [:a :c] #_:nx :swap/dissoc) @a_)
   (swap-in! (atom {:a {:b :b1}}) [:a :b] (fn [m] (swapped :swap/abort m)))
   (swap-in! (atom {:a {:b :b1}}) (fn [m] (swapped (assoc m :c :C) :swap/old)))]

  [[{:a :A, :b :B, :c :C} {:a :A, :b :B, :c :C}]
   [{:a :A, :b :B} {:a :A, :b :B, :c :C}]
   [{:b :B, :c :C} {:a {:b :B, :c :C}}]
   [{:b :B} {:a {:b :B, :c :C}}]
   101
   {:a {:b :b1}, :d :d1}
   :b1
   {:a {:b :b1}}])

(defn pull-val! ; Keys: 1 (optimized, common transform)
  "Removes and returns value mapped to key."
  ([atom_ k          ] (pull-val! atom_ k nil))
  ([atom_ k not-found]
   (swap-val! atom_ k not-found
     (fn [v0] (swapped :swap/dissoc v0)))))

(comment (pull-val! (atom {:a :A}) :b :nx))

;;;; Instants

(do
  #?(:clj (defmacro now-dt*   [] `(if-cljs           (js/Date.)  (java.util.Date.))))
  #?(:clj (defmacro now-udt*  [] `(if-cljs (.getTime (js/Date.)) (System/currentTimeMillis))))
  (defn  now-dt       [] (now-dt*))
  (defn now-udt ^long [] (now-udt*))

  #?(:clj (defn now-nano ^long [] (System/nanoTime))
     :cljs
     (def now-nano "Uses window context as epoch, Ref. http://goo.gl/mWZWnR"
       (if-let [perf (and (oget js-?win "performance"))]
         ;; Ref. http://goo.gl/fn84us
         (if-let [f (or (oget perf "now")  (oget perf "mozNow") (oget perf "msNow")
                        (oget perf "oNow") (oget perf "webkitNow"))]
           ;; JS call returns millisecs double, accurate to 1/1000th of a ms:
           (fn [] (* 1000000 (long (.call f perf))))
           (fn [] (* 1000000 (now-udt*))))
         (fn []   (* 1000000 (now-udt*))))))

  #?(:clj (defmacro now-nano* [] `(if-cljs (now-nano) (System/nanoTime)))))

;;;; Memoization

(defn memoize-last
  "Like `core/memoize` but only caches the fn's most recent call.
  Great for Reactjs render op caching on mobile devices, etc."
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

(defn fmemoize
  "For Clj: fastest possible memoize. Non-racey, 0-3 arity only.
  For Cljs: just passes through to `core/memoize`."
  [f]
  #?(:cljs (cljs.core/memoize f)
     :clj
     ;; Non-racey just as fast as racey, and protects against nils in maps
     (let [cache0_ (java.util.concurrent.atomic.AtomicReference. nil)
           cache1_ (java.util.concurrent.ConcurrentHashMap.)
           cachen_ (java.util.concurrent.ConcurrentHashMap.)
           nil-sentinel (Object.)]

       (fn
         ([ ]
          @(or
             (.get cache0_)
             (let [dv (delay (f))]
               (if (.compareAndSet cache0_ nil dv)
                 dv
                 (.get cache0_)))))

         ([x]
          (let [x* (if (nil? x) nil-sentinel x)]
            @(or
               (.get cache1_ x*)
               (let [dv (delay (f x))]
                 (or (.putIfAbsent cache1_ x* dv) dv)))))

         ([x1 x2]
          (let [xs [x1 x2]]
            @(or
               (.get cachen_ xs)
               (let [dv (delay (f x1 x2))]
                 (or (.putIfAbsent cachen_ xs dv) dv)))))

         ([x1 x2 x3]
          (let [xs [x1 x2 x3]]
            @(or
               (.get cachen_ xs)
               (let [dv (delay (f x1 x2 x3))]
                 (or (.putIfAbsent cachen_ xs dv) dv)))))))))

(defn- gc-now? [rate]
  #?(:clj  (<= (java.lang.Math/random) ^double rate)
     :cljs (<=       (.random js/Math)         rate)))

(comment (qb 1e6 (gc-now? 0.5)))

(deftype SimpleCacheEntry [delay ^long udt])
(deftype TickedCacheEntry [delay ^long udt ^long tick-lru ^long tick-lfu])

(declare top)

(defn -swap-val!
  "Used internally by memoization utils."
  [atom_ k f]
  (loop []
    (let [m0 @atom_
          v1 (f (get m0 k))
          m1 (assoc  m0 k v1)]
      (-if-cas! atom_ m0 m1
        v1
        (recur)))))

(defn cache
  "Returns a cached version of given referentially transparent function `f`.

  Like `core/memoize` but:
    - Often faster, depending on options.
    - Prevents race conditions on writes.
    - Supports cache invalidation by prepending args with:
      - `:mem/del`   ; Delete cached item for subsequent args, returns nil.
      - `:mem/fresh` ; Renew  cached item for subsequent args, returns new val.

    - Supports options:
      - `ttl-ms` ; Expire cached items after <this> many msecs.
      - `size`   ; Restrict cache size to <this> many items at the next garbage
                 ; collection (GC).

      - `gc-every` ; Run garbage collection (GC) approximately once every
                   ; <this> many calls to cached fn. If unspecified, GC rate
                   ; will be determined automatically based on `size`.

  See also `defn-cached`, `fmemoize`, `memoize-last`."

  {:added "v3.36.0 (2022-11-18)"}
  ([f] ; De-raced, commands
   #?(:cljs
      (let [cache_ (volatile! {})
            get-sentinel (js-obj)]

        (fn cached [& xs]
          (let [x1 (first xs)]
            (cond
              (kw-identical? x1 :mem/del)
              (let [xn (next  xs)
                    x2 (first xn)]
                (if (kw-identical? x2 :mem/all)
                  (vreset! cache_ {})
                  (vswap!  cache_ dissoc xn))
                nil)

              (kw-identical? x1 :mem/fresh)
              (let [xn (next xs)
                    v  (apply f xn)] (vswap! cache_ assoc xn v) v)

              :else
              (let [v (get @cache_ xs get-sentinel)]
                (if (identical? v get-sentinel)
                  (let [v (apply f xs)] (vswap! cache_ assoc xs v) v)
                  v))))))

      :clj
      (let [nil-sentinel (Object.)
            cache_ (java.util.concurrent.ConcurrentHashMap.)]

        (fn
          ([ ] @(or (.get cache_ nil-sentinel)
                    (let [dv (delay (f))]
                      (or (.putIfAbsent cache_ nil-sentinel dv) dv))))

          ([& xs]
           (let [x1 (first xs)]
             (cond
               (kw-identical? x1 :mem/del)
               (let [xn (next  xs)
                     x2 (first xn)]
                 (if (kw-identical? x2 :mem/all)
                   (.clear  cache_)
                   (.remove cache_ (or xn nil-sentinel)))
                 nil)

               (kw-identical? x1 :mem/fresh)
               @(let [xn (next xs)
                      dv (delay (apply f xn))]
                  (.put cache_ (or xn nil-sentinel) dv) dv)

               :else
               @(or (.get cache_ xs)
                  (let [dv (delay (apply f xs))]
                    (or (.putIfAbsent cache_ xs dv) dv))))))))))

  ([{:keys [size ttl-ms gc-every] :as opts} f]

   (have? [:ks<= #{:size :ttl-ms :gc-every}] opts)
   (have? [:or nil? pos-num?] size ttl-ms gc-every)

   (cond
     size ; De-raced, commands, ttl, gc, max-size
     (let [gc-now?  gc-now?
           ticker   (counter)
           cache_   (atom nil) ; {<args> <TickedCacheEntry>}
           latch_   (atom nil) ; Used to pause writes during gc
           ttl-ms   (long (or ttl-ms 0))
           ttl?     (not (zero? ttl-ms))
           size     (long size)
           gc-every (long (or gc-every (clamp-int 1000 16000 size)))]

       (fn cached [& args]
         (let [a1 (first args)]
           (cond
             (kw-identical? a1 :mem/del)
             (let [argn (next args)
                   a2   (first argn)]
               (if (kw-identical? a2 :mem/all)
                 (reset! cache_ nil)
                 (swap!  cache_ dissoc argn))
               nil)

             :else
             (let [^long tick (ticker) ; Always inc, even on reads
                   instant (if ttl? (now-udt*) 0)]

               (when (and
                       ;; We anyway have a tick, so may as well be exact
                       ;; (gc-now? gc-rate)
                       (== (rem tick gc-every) 0)
                       (>= (count @cache_) (* 1.1 size)))

                 (let [latch #?(:clj (CountDownLatch. 1) :cljs nil)]
                   (-if-cas! latch_ nil latch
                     (do
                       ;; First prune ttl-expired stuff
                       (when ttl?
                         (swap! cache_
                           (fn [m]
                             (persistent!
                               (reduce-kv
                                 (fn [acc k ^TickedCacheEntry e]
                                   (if (> (- instant (.-udt e)) ttl-ms)
                                     (dissoc! acc k)
                                     acc))
                                 (transient (or m {}))
                                 m)))))

                       ;; Then prune by ascending (worst) tick-sum:
                       (let [snapshot @cache_
                             n-to-gc  (- (count snapshot) size)]

                         (when (>= n-to-gc (* 0.1 size))
                           (let [ks-to-gc
                                 (top n-to-gc
                                   (fn [k]
                                     (let [e ^TickedCacheEntry (get snapshot k)]
                                       (+ (.-tick-lru e) (.-tick-lfu e))))
                                   (keys snapshot))]

                             (swap! cache_
                               (fn [m]
                                 (persistent!
                                   (reduce (fn [acc in] (dissoc! acc in))
                                     (transient (or m {})) ks-to-gc)))))))

                       #?(:clj (reset!     latch_ nil))
                       #?(:clj (.countDown latch))))))

               (let [fresh?(kw-identical? a1 :mem/fresh)
                     args  (if fresh? (next args) args)

                     _ #?(:clj (when-let [l @latch_] (.await ^CountDownLatch l)) :cljs nil)
                     ^TickedCacheEntry e
                     (-swap-val! cache_ args
                       (fn [?e]
                         (if (or (nil? ?e) fresh?
                                 (> (- instant (.-udt ^TickedCacheEntry ?e)) ttl-ms))
                           (TickedCacheEntry. (delay (apply f args)) instant tick 1)
                           (let [e ^TickedCacheEntry ?e]
                             (TickedCacheEntry. (.-delay e) (.-udt e)
                               tick (inc (.-tick-lfu e)))))))]

                 @(.-delay e)))))))

     ttl-ms ; De-raced, commands, ttl, gc
     (let [gc-now? gc-now?
           cache_  (atom nil) ; {<args> <SimpleCacheEntry>}
           latch_  (atom nil) ; Used to pause writes during gc
           ttl-ms  (long ttl-ms)
           gc-rate
           (let [gce (or gc-every 8e3)]
             (/ 1.0 (long gce)))]

       (fn cached [& args]
         (let [a1 (first args)]
           (cond
             (kw-identical? a1 :mem/del)
             (let [argn (next  args)
                   a2   (first argn)]
               (if (kw-identical? a2 :mem/all)
                 (reset! cache_ nil)
                 (swap!  cache_ dissoc argn))
               nil)

             :else
             (let [instant (now-udt*)]

               (when (gc-now? gc-rate)
                 (let [latch #?(:clj (CountDownLatch. 1) :cljs nil)]
                   (-if-cas! latch_ nil latch
                     (do
                       (swap! cache_
                         (fn [m]
                           (persistent!
                             (reduce-kv
                               (fn [acc k ^SimpleCacheEntry e]
                                 (if (> (- instant (.-udt e)) ttl-ms)
                                   (dissoc! acc k)
                                   acc))
                               (transient (or m {}))
                               m))))

                       #?(:clj (reset!     latch_ nil))
                       #?(:clj (.countDown latch))))))

               (let [fresh? (kw-identical? a1 :mem/fresh)
                     args   (if fresh? (next args) args)
                     _      #?(:clj (when-let [l @latch_] (.await ^CountDownLatch l)) :cljs nil)
                     ^SimpleCacheEntry e
                     (-swap-val! cache_ args
                       (fn [?e]
                         (if (or (nil? ?e) fresh?
                                 (> (- instant (.-udt ^SimpleCacheEntry ?e)) ttl-ms))
                           (SimpleCacheEntry. (delay (apply f args)) instant)
                           ?e)))]
                 @(.-delay e)))))))

     :else (cache f))))

(comment :see-tests)

(defn memoize
  "Alternative way to call `cache`, provided mostly for back compatibility.
  See `cache` docstring for details."
  ;; {:deprecated "v3.36.0 (2022-11-18)"}
  ([            f] (cache                             f))
  ([     ttl-ms f] (cache {           :ttl-ms ttl-ms} f))
  ([size ttl-ms f] (cache {:size size :ttl-ms ttl-ms} f)))

(comment
  (do
    (def f0 (fn [& args] (Thread/sleep 600) (rand)))
    (def f1 (clojure.core/memoize f0))
    (def f2 (memoize              f0))
    (def f3 (memoize 5000         f0))
    (def f4 (memoize 2 nil        f0))
    (def f5 (memoize 2 5000       f0))
    (def f6 (fmemoize             f0)))

  (qb 1e5 (f1)    (f2)    (f3)    (f4)    (f5)    (f6))    ; [ 5.4  3.95 16.56 13.98 18.01 3.97]
  (qb 1e5 (f1 :x) (f2 :x) (f3 :x) (f4 :x) (f5 :x) (f6 :x)) ; [14.71 8.96 31.27 37.37 45.42 6.76]

  (let [f1 (clojure.core/memoize (fn [] (Thread/sleep 5) (print "f1\n")))
        f2 (memoize              (fn [] (Thread/sleep 5) (print "f2\n")))]
    (println "---")
    (dotimes [_ 10]
      (future (f2)) ; Never prints >once
      (future (f1))))

  (do ; Test GC (monitor JVM memory)
    (defn f1 [_] (vec (repeatedly 1000 #(str (rand)))))
    (def  m1 (memoize 500 (ms :days 3) f1))
    (dotimes [n 1e5] (m1 (rand)))))

#?(:clj
   (defmacro defn-cached
     "Defines a cached function.
     Like (def <sym> (cache <cache-opts> <body...>)), but preserves
     :arglists (arity) metadata as with `defn`:

       (defn-cached ^:private my-fn {:ttl-ms 500}
         \"Does something interesting, caches resultes for 500 msecs\"
         [n]
         (rand-int n))"

     {:added "v3.36.0 (2022-11-18)"}
     [sym cache-opts & body]
     (let [arglists ; e.g. '([x] [x y])
           (let [[_ sigs] (name-with-attrs sym body)]
             (if (vector? (first sigs))
               (list      (first sigs))
               (map first sigs)))

           [sym body]
           (name-with-attrs sym body
             {:arglists `'~arglists})]

       (have? map?                               cache-opts)
       (have? [:ks<= #{:ttl-ms :size :gc-every}] cache-opts)

       `(def ~sym (cache ~cache-opts (fn ~@body))))))

(comment :see-tests)

;;;; Rate limits

(deftype LimitSpec  [^long n ^long ms])
(deftype LimitEntry [^long n ^long udt0])
(deftype LimitHits  [m worst-sid ^long worst-ms])

(let [limit-spec (fn [n ms] (have? pos-int? n ms) (LimitSpec. n ms))]
  (defn- coerce-limit-specs [x]
    (cond!
      (map?    x) (reduce-kv (fn [acc sid [n ms]] (assoc acc sid (limit-spec n ms))) {} x)
      (vector? x)
      (let [i (volatile! -1)]
        (reduce
          (fn [acc [n ms ?id]] ; ?id for back compatibility
            (assoc acc (or ?id (vswap! i (fn [i] (inc ^long i))))
              (limit-spec n ms))) {} x)))))

(comment (qb 1e5 (coerce-limit-specs [[10 1000] [20 2000]])))

(defn limiter*
  "Experimental. Like `limiter` but returns [<state_> <limiter>]."
  ([     specs] (limiter* nil specs))
  ([opts specs]
   (if (empty? specs)
     [nil (constantly nil)]
     (let [latch_ (atom nil) ; Used to pause writes during gc
           reqs_  (atom nil) ; {<rid> {<sid> <LimitEntry>}}
           specs  (coerce-limit-specs specs) ; {<sid> <LimitSpec>}

           {:keys [req-id-fn gc-every]
            :or   {req-id-fn identity
                   gc-every  1.6e4}}
           opts ; Undocumented

           gc-now? gc-now?
           gc-rate (let [gce (long gc-every)] (/ 1.0 gce))

           f1
           (fn [rid peek?]
             (let [instant (now-udt*)
                   rid (req-id-fn rid)]

               (when (and (not peek?) (gc-now? gc-rate))
                 (let [latch #?(:clj (CountDownLatch. 1) :cljs nil)]
                   (-if-cas! latch_ nil latch
                     (do
                       (swap! reqs_
                         (fn [reqs] ; {<rid> <entries>}
                           (persistent!
                             (reduce-kv
                               (fn [acc rid entries]
                                 (let [new-entries
                                       (reduce-kv
                                         (fn [acc sid ^LimitEntry e]
                                           (if-let [^LimitSpec s (get specs sid)]
                                             (if (>= instant (+ (.-udt0 e) (.-ms s)))
                                               (dissoc acc sid)
                                               acc)
                                             (dissoc acc sid)))
                                         entries ; {<sid <LimitEntry>}
                                         entries)]
                                   (if (empty? new-entries)
                                     (dissoc! acc rid)
                                     (assoc!  acc rid new-entries))))
                               (transient (or reqs {}))
                               reqs))))

                       #?(:clj (reset!     latch_ nil))
                       #?(:clj (.countDown latch))))))

               ;; Need to atomically check if all limits pass before
               ;; committing to any n increments:
               (loop []
                 (let [reqs        @reqs_     ; {<sid> <entries>}
                       entries (get reqs rid) ; {<sid> <LimitEntry>}
                       ?hits                  ; ?LimitHits
                       (if (nil? entries)
                         nil
                         (reduce-kv
                           (fn [^LimitHits acc sid ^LimitEntry e]
                             (if-let [^LimitSpec s (get specs sid)]
                               (if (< (.-n e) (.-n s))
                                 acc
                                 (let [tdelta (- (+ (.-udt0 e) (.-ms s)) instant)]
                                   (if (<= tdelta 0)
                                     acc
                                     (cond
                                       (nil? acc) (LimitHits. {sid tdelta} sid tdelta)

                                       (> tdelta (.-worst-ms acc))
                                       (LimitHits. (assoc (.-m acc) sid tdelta) sid tdelta)

                                       :else
                                       (LimitHits. (assoc (.-m acc) sid tdelta)
                                         (.-worst-sid acc)
                                         (.-worst-ms  acc))))))
                               acc))
                           nil
                           entries))]

                   (if (or peek? ?hits)
                     ;; No action (peeking, or hit >= 1 spec)
                     (when-let [^LimitHits h ?hits]
                       [(.-worst-sid h) (.-worst-ms h) (.-m h)])

                     ;; Passed all limit specs, ready to commit increments:
                     (if-let [l @latch_]
                       #?(:clj (do (.await ^CountDownLatch l) (recur)) :cljs nil)
                       (let [new-entries
                             (reduce-kv
                               (fn [acc sid ^LimitSpec s]
                                 (assoc acc sid
                                   (if-let [^LimitEntry e (get entries sid)]
                                     (let [udt0 (.-udt0 e)]
                                       (if (>= instant (+ udt0 (.-ms s)))
                                         (LimitEntry. 1 instant)
                                         (LimitEntry. (inc (.-n e)) udt0)))
                                     (LimitEntry. 1 instant))))
                               entries
                               specs)]

                         (-if-cas! reqs_ reqs (assoc reqs rid new-entries)
                           nil
                           (recur)))))))))]

       [reqs_
        (fn check-limits!
          ([          ] (f1 nil    false))
          ([    req-id] (f1 req-id false))
          ([cmd req-id]
           (cond
             (kw-identical? cmd :rl/reset)
             (do
               (if (kw-identical? req-id :rl/all)
                 (reset! reqs_ nil)
                 (swap!  reqs_ dissoc (req-id-fn req-id)))
               nil)

             (kw-identical? cmd :rl/peek)
             (f1 req-id true)

             :else
             (throw
               (ex-info "[encore/limiter*] Unexpected limiter command"
                 {:command {:value cmd :type (type cmd)}
                  :req-id req-id})))))]))))

(defn limiter ; rate-limiter
  "Takes {<spec-id> [<n-max-reqs> <msecs-window>]}, and returns a rate
  limiter (fn check-limits! [req-id]) -> nil (all limits pass), or
  [<worst-spec-id> <worst-backoff-msecs> {<spec-id> <backoff-msecs>}].

  Limiter fn commands:
    :rl/peek  <req-id> - Check limits w/o side effects.
    :rl/reset <req-id> - Reset all limits for given req-id."

  ([     specs] (limiter nil specs))
  ([opts specs]
   (let [[_ f] (limiter* opts specs)]
     f)))

(comment
  (let [[s_ rl1] (limiter* {:2s [1 2000] :5s [2 5000]})]
    (def s_  s_)
    (def rl1 rl1))

  (qb 1e6 (rl1)) ; 151.04
  (dotimes [n 1e6] (rl1 (str (rand)))) ; Test GC
  (count @s_))

;;;; Counters

#?(:clj
   (deftype Counter [^java.util.concurrent.atomic.AtomicLong n_]
     clojure.lang.IDeref (deref [_] (.get n_))
     clojure.lang.IFn
     (invoke [_    ] (.getAndIncrement n_))
     (invoke [_ add] (.getAndAdd       n_ (long add)))
     (invoke [_ action n]
       (let [n (long n)]
         (case action
           (:add)           (do (.addAndGet n_ n) nil)
           (:set)           (do (.set       n_ n) nil)
           (:set= :set-get) (do (.set       n_ n) n)
           (:=set :get-set) (do (.getAndSet n_ n))
           (:=+   :get-add) (do (.getAndAdd n_ n))
           (:+=   :add-get) (do (.addAndGet n_ n))))))

   :cljs
   (deftype Counter [^:mutable c]
     IDeref (-deref [_] c)
     IFn
     (-invoke [_    ] (let [o c] (set! c (inc c))   o))
     (-invoke [_ add] (let [o c] (set! c (+ c add)) o))
     (-invoke [_ action n]
       (case action
         (:add)           (do        (set! c (+ c n)) nil)
         (:set)           (do        (set! c n)       nil)
         (:set= :set-get) (do        (set! c n) n)
         (:=set :get-set) (let [o c] (set! c n) o)
         (:=+   :get-add) (let [o c] (set! c (+ c n)) o)
         (:+=   :add-get) (do        (set! c (+ c n)) c)))))

(defn counter
  "Returns a fast atomic Counter with `init` initial int value:
    - (<counter>    ) -> add 1, return old val
    - (<counter> <n>) -> add n, return old val

    Experimental 3-arity version takes an `action`:
      :add, :set, :set-get, :get-set, :get-add, :add-get"
  ([    ] (counter 0))
  ([init]
   #?(:clj  (Counter. (java.util.concurrent.atomic.AtomicLong. (long init)))
      :cljs (Counter.                                          (long init)))))

(comment (let [c (counter)] (dotimes [_ 100] (c 2)) (c)))

(defn- rc-deref [^long msecs ts_ n-skip_ gc-fn]
  (let [t1 (now-udt*)
        ^long n-skip0  @n-skip_
        ts             @ts_
        n-total  (count ts)
        ^long n-window
        (reduce
          (fn [^long n ^long t0]
            (if (<= (- t1 t0) msecs)
              (inc n)
              (do  n)))
          0
          (subvec ts n-skip0))

        n-skip1 (- n-total n-window)]

    ;; (println {:n-total n-total :n-window n-window :n-skip0 n-skip0 :n-skip1 n-skip1})
    (when (<            n-skip0 n-skip1)
      (-if-cas! n-skip_ n-skip0 n-skip1
        (when (> n-skip1 10000) ; Time to gc, amortised cost
          (gc-fn n-skip1))))

    n-window))

#?(:clj
   (deftype RollingCounter [^long msecs ts_ n-skip_ p_]
     clojure.lang.IFn
     (invoke [this]
       (when-let [p @p_] @p) ; Block iff latched
       (swap! ts_ (let [t1 (now-udt*)] (fn [v] (conj v t1))))
       this ; Return to allow optional deref
       )

     clojure.lang.IDeref
     (deref [_]
       (when-let [p @p_] @p) ; Block iff latched
       (rc-deref msecs ts_ n-skip_
         (fn gc [n-skip1]
           (let [p (promise)]
             (-if-cas! p_ nil p ; Latch
               (do
                 (swap! ts_ (fn [v] (subvec v n-skip1)))
                 (reset!  n-skip_ 0)
                 (reset!  p_ nil)
                 (deliver p  nil))))))))

   :cljs
   (deftype RollingCounter [^long msecs ts_ n-skip_]
     IFn
     (-invoke [this]
       (swap! ts_ (let [t1 (now-udt*)] (fn [v] (conj v t1))))
       this ; Return to allow optional deref
       )

     IDeref
     (-deref [_]
       (rc-deref msecs ts_ n-skip_
         (fn gc [n-skip1]
           (swap! ts_ (fn [v] (subvec v n-skip1)))
           (reset! n-skip_ 0))))))

(defn rolling-counter
  "Experimental. Returns a RollingCounter that you can:
    - Invoke to increment count in last `msecs` window and return RollingCounter.
    - Deref  to return    count in last `msecs` window."
  [msecs]
  (RollingCounter.
    (long (have pos-int? msecs))
    (atom [])
    (atom 0)
    #?(:clj (atom nil))))

(comment
  (def myrc (rolling-counter 4000))
  (dotimes [_ 100000] (myrc))
  @myrc)

;;;; Rolling sequentials

(defn rolling-vector
  "Returns a stateful fn of 2 arities:
    (fn [ ]) => Returns current sub/vector in O(1).
    (fn [x]) => Adds `x` to right of sub/vector, maintaining length <= `nmax`.
                Returns current sub/vector.

  Useful for maintaining limited-length histories, etc.
  See also `rolling-list` (Clj only)."

  {:added "v3.45.0 (2022-12-13)"}
  ([nmax                          ] (rolling-vector nmax nil))
  ([nmax {:keys [gc-every init-val]
          :or   {gc-every 16e3}}]

   (let [nmax     (long nmax)
         acc_     (atom (vec init-val))
         gc-every (when gc-every (long gc-every))
         ticker   (when gc-every (counter))
         latch_   (when gc-every (atom nil))]

     (fn rolling-vec-fn
       ([ ] @acc_)
       ([x]
        (when gc-every
          #?(:clj (when-let [l @latch_] (.await ^CountDownLatch l)))

          (let [^long tick (ticker)]
            (when-let [gc-now? (== (rem tick ^long gc-every) 0)]
              #?(:cljs (swap! acc_ (fn [sv] (into [] sv)))
                 :clj
                 (let [latch (CountDownLatch. 1)]
                   (when (compare-and-set! latch_ nil latch)
                     (swap! acc_ (fn [sv] (into [] sv)))
                     (reset!     latch_ nil)
                     (.countDown latch)))))))

        (swap! acc_
          (fn [acc]
            (let [new (conj acc x)]
              (if (> (count new) nmax)
                (subvec new 1)
                (do     new))))))))))

(comment (let [rv (rolling-vector 3), c (counter)] [(qb 1e6 (rv (c))) (rv)])) ; 189.66

#?(:clj
   (defn rolling-list
     "Returns a stateful fn of 2 arities:
       (fn [ ]) => Returns current array in O(n).
       (fn [x]) => Adds `x` to right of list, maintaining length <~ `nmax`.
                   Returns nil. Very fast (faster than `rolling-vector`).

     Useful for maintaining limited-length histories, etc.
     See also `rolling-vector`."

     {:added "v3.45.0 (2022-12-13)"}
     ([nmax                   ] (rolling-list nmax nil))
     ([nmax {:keys [init-val]}]
      (let [nmax (long nmax)
            ^java.util.LinkedList ll
            (if init-val
              (java.util.LinkedList. init-val)
              (java.util.LinkedList.))]

        (fn rolling-list-fn
          ([ ] (.toArray ll))
          ([x]
           (do                       (.addLast     ll x))
           (when (> (.size ll) nmax) (.removeFirst ll))
           nil))))))

(comment (let [rl (rolling-list 3), c (counter)] [(qb 1e6 (rl (c))) (vec (rl))])) ; 98.36

;;;; Strings

#?(:clj  (def ^String system-newline (System/getProperty "line.separator"))
   :cljs (def         system-newline "\n"))

#?(:clj  (defn          str-builder? [x] (instance?            StringBuilder x))
   :cljs (defn ^boolean str-builder? [x] (instance? goog.string.StringBuffer x)))

(def str-builder "For cross-platform string building"
  #?(:clj  (fn (^StringBuilder [      ] (StringBuilder.))
               (^StringBuilder [s-init] (StringBuilder. ^String s-init)))

     :cljs (fn ([      ] (goog.string.StringBuffer.))
               ([s-init] (goog.string.StringBuffer. s-init)))))

(defn sb-append "For cross-platform string building"
  #?(:clj  (^StringBuilder [^StringBuilder str-builder ^String s] (.append str-builder s))
     :cljs (               [               str-builder         s] (.append str-builder s)))
  ([str-builder s & more]
   (reduce (fn [acc in] (sb-append acc in))
     (sb-append str-builder s)
     more)))

(comment (str (sb-append (str-builder "a") "b" "c")))

(def str-rf "String builder reducing fn"
  (fn
    ([]       (str-builder))
    ([acc]               (if (str-builder? acc) acc (str-builder (str acc)))) ; cf
    ([acc in] (sb-append (if (str-builder? acc) acc (str-builder (str acc))) (str in)))))

(comment
  (qb 1e3 ; [358.45 34.6]
         (reduce str    (range 512))
    (str (reduce str-rf (range 512)))))

(defn str-join
  "Faster, transducer-based generalization of `clojure.string/join` with `xform`
  support."
  (^String [                coll] (str-join nil       nil coll))
  (^String [separator       coll] (str-join separator nil coll))
  (^String [separator xform coll]
   (if (and separator (not= separator ""))
     (let [sep-xform (interpose separator)
           str-rf*   (completing str-rf str)]
       (if xform
         (transduce (comp xform sep-xform) str-rf* coll)
         (transduce             sep-xform  str-rf* coll)))
     (if xform
       (transduce xform (completing str-rf str) coll)
       (str (reduce str-rf coll))))))

(comment
  (qb 1e5
    (str/join "," ["a" "b" "c" "d"])
    (str-join "," ["a" "b" "c" "d"])
    (str-join ""  ["a" "b" "c" "d"])) ; [29.37 23.63 13.34]
  (str-join "," (comp (filter #{"a" "c"}) (map str/upper-case)) ["a" "b" "c"]))

(defn #?(:clj str-contains? :cljs ^boolean str-contains?)
  [s substr]
  #?(:clj  (.contains ^String s ^String substr)
     :cljs (not= -1 (.indexOf s substr))))

(defn #?(:clj str-starts-with? :cljs ^boolean str-starts-with?)
  [s substr]
  #?(:clj  (.startsWith ^String s ^String substr)
     :cljs (zero? (.indexOf s substr))))

(defn #?(:clj str-ends-with? :cljs ^boolean str-ends-with?)
  [s substr]
  #?(:clj (.endsWith ^String s ^String substr)
     :cljs
     (let [s-len      (.-length s)
           substr-len (.-length substr)]
       (when (>= s-len substr-len)
         (not= -1 (.indexOf s substr (- s-len substr-len)))))))

(defn str-?index
  ([s substr          ] (str-?index s substr 0         false))
  ([s substr start-idx] (str-?index s substr start-idx false))
  ([s substr start-idx last?]
   (let [result
         (if last?
           #?(:clj  (.lastIndexOf ^String s ^String substr ^long start-idx)
              :cljs (.lastIndexOf         s         substr       start-idx))
           #?(:clj  (.indexOf     ^String s ^String substr ^long start-idx)
              :cljs (.indexOf             s         substr       start-idx)))]

     (when (not= result -1) result))))

(comment (qb 1000 (str-?index "hello there" "there")))

(defn get-substr-by-idx
  "Returns ?substring from given string.

  Like `subs` but:
    - Provides consistent clj/s behaviour.
    - Never throws (snaps to valid indexes).
    - Indexes may be -ive (=> indexed from end of string).

  Returns nil when requested substring would be empty."

  ([s start-idx        ] (get-substr-by-idx s start-idx nil))
  ([s start-idx end-idx]
   (when s
     (let [s #?(:clj ^String s :cljs s)
           full-len #?(:clj (.length s) :cljs (.-length s))
           ^long start-idx (if (nil? start-idx) 0        start-idx)
           ^long end-idx   (if (nil? end-idx)   full-len   end-idx)]

       (cond
         (and (== start-idx 0) (>= end-idx full-len)) s
         :let
         [start-idx (if (neg?  start-idx) (+ full-len start-idx) start-idx) ; Idx from right
          start-idx     (max 0 start-idx) ; Snap left

          end-idx     (if (neg? end-idx) (+ full-len end-idx) end-idx) ; Idx from right
          end-idx (min full-len end-idx) ; Snap right
          ]

         (>= start-idx end-idx) nil

         :else
         (.substring s start-idx end-idx))))))

(comment :see-tests)

(comment
  (qb 1e5
    (subs              "hello world"   0 11)
    (get-substr-by-idx "hello world" -10 11)))

(defn get-substr-by-len
  "Returns ?substring from given string.
  Like `get-substr-by-idx`, but takes a substring-length 3rd argument."
  ([s start-idx        ] (get-substr-by-len s start-idx nil))
  ([s start-idx sub-len]
   (when s
     (let [s #?(:clj ^String s :cljs s)
           full-len #?(:clj (.length s) :cljs (.-length s))

           ^long sub-len (if (nil? sub-len) full-len sub-len)]

       (cond
         (not (pos? sub-len)) nil
         :let [^long start-idx (if (nil? start-idx) 0 start-idx)]

         (and (== start-idx 0) (>= sub-len full-len)) s

         :let [start-idx (if (neg? start-idx) (+ full-len start-idx) start-idx) ; Idx from right
               start-idx (max 0    start-idx) ; Snap left

               end-idx (+ start-idx sub-len)
               end-idx (min full-len end-idx) ; Snap right
               ]

         (>= start-idx end-idx) nil

         :else
         (.substring s start-idx end-idx))))))

(comment :see-tests)

(defn
  #?(:clj           case-insensitive-str=
     :cljs ^boolean case-insensitive-str=)

  "Returns true iff given strings are equal, ignoring case."
  ;; Implementation detail:
  ;; Compares normalized chars 1 by 1, so often faster than naive comparison
  ;; of normalized strings.

  {:added "v3.25.0 (2022-10-13)"}
  [s1 s2]
  #?(:clj (.equalsIgnoreCase ^String s1 ^String s2)
     :cljs
     (or
       (identical? s1 s2)
       (let [l1 (.-length s1)
             l2 (.-length s2)]
         (and
           (== l1 l2)
           ;; (= (str/lower-case s1) (str/lower-case s2))
           ;; Still needs bench comparison:
           (reduce-n
             (fn [acc idx]
               (let [c1 (.toLowerCase (.charAt s1 idx))
                     c2 (.toLowerCase (.charAt s2 idx))]
                 (if (= c1 c2) true (reduced false))))
             true
             0
             l1))))))

(comment
  (qb 1e6
    (do                 (= "-abcdefghijklmnop" "_abcdefghijklmnop"))
    (case-insensitive-str= "-abcdefghijklmnop" "_abcdefghijklmnop")
    (=
      (str/lower-case "-abcdefghijklmnop")
      (str/lower-case "_abcdefghijklmnop"))))

#?(:clj
   (defn norm-str
     "Given a Unicode string, returns the normalized de/composed form.
     It's often a good idea to normalize strings before exchange or storage,
     especially if you're going to be querying against those string.

     `form` is ∈ #{:nfc :nfkc :nfd :nfkd <java.text.NormalizerForm>}.
     Defaults to :nfc as per W3C recommendation."

     ([     s] (norm-str :nfc s))
     ([form s]
      [s]
      (java.text.Normalizer/normalize s
        (case form
          :nfc  java.text.Normalizer$Form/NFC
          :nfkc java.text.Normalizer$Form/NFKC
          :nfd  java.text.Normalizer$Form/NFD
          :nfkd java.text.Normalizer$Form/NFKD
          (if (instance? java.text.Normalizer$Form form)
            form
            (throw
              (ex-info "[encore/norm-str] Unrecognized normalization form (expected ∈ #{:nfc :nfkc :nfd :nfkd <java.text.Normalizer$Form>})"
                {:form {:value form :type (type form)}}))))))))

(comment (qb 1e6 (norm-str :nfc "foo"))) ; 114

(defn str-replace
  "Like `str/replace` but provides consistent clj/s behaviour.

  Workaround for http://dev.clojure.org/jira/browse/CLJS-794,
                 http://dev.clojure.org/jira/browse/CLJS-911.

  Note that ClojureScript 1.7.145 introduced a partial fix for CLJS-911.
  A full fix could unfortunately not be introduced w/o breaking compatibility
  with the previously incorrect behaviour. CLJS-794 also remains unresolved."
  [s match replacement]
  #?(:clj (str/replace s match replacement)
     :cljs
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
       :else (throw (str "Invalid match arg: " match)))))

(do
  (defn nil->str "nil/undefined -> \"nil\"" [x]
    ;; Note (undefined? x) not needed for modern Cljs
    #?(:clj  (if                    (nil? x)  "nil" x)
       :cljs (if (or (undefined? x) (nil? x)) "nil" x)))

  (defn format*
    (#?(:clj ^String [      fmt args]
        :cljs        [      fmt args]) (format* nil->str fmt args))
    (#?(:clj ^String [xform fmt args]
        :cljs        [xform fmt args])
      (if (nil? fmt)
        "" ; Prevent NPE
        (let [args (if xform (mapv xform args) args)]
          #?(:clj  (String/format     fmt (to-array args))
             :cljs (apply gstr/format fmt           args))))))

  (defn format
    "Like `core/format` but:
      * Returns \"\" when fmt is nil rather than throwing an NPE.
      * Formats nil as \"nil\" rather than \"null\".
      * Provides ClojureScript support via goog.string.format (this has fewer
        formatting options than Clojure's `format`!)."
    [fmt & args] (format* fmt args)))

(defn str-join-once
  "Like `string/join` but skips duplicate separators."
  [separator coll]
  (let [sep separator]
    (if (str/blank? sep)
      (str (reduce str-rf "" coll))
      (let [acc-ends-with-sep?_ (volatile! false)
            acc-empty?_         (volatile! true)]
        (str
          (reduce
            (fn [acc in]
              (let [in (str in)
                    in-empty? (= in "")
                    in-starts-with-sep? (str-starts-with? in sep)
                    in-ends-with-sep?   (str-ends-with?   in sep)
                    acc-ends-with-sep?  @acc-ends-with-sep?_
                    acc-empty?          @acc-empty?_]

                (vreset! acc-ends-with-sep?_ in-ends-with-sep?)
                (when acc-empty? (vreset! acc-empty?_ in-empty?))

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
  form, tabs, spaces, etc.) to a single regular space."
  [s] (str/replace (str s) #"\s+" \space))

(defn count-words [s] (if (str/blank? s) 0 (count (str/split s #"\s+"))))
(comment (count-words "Hello this is a    test"))

(defn uuid-str
  "Returns a UUIDv4 string of form \"xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx\".
  Ref. http://www.ietf.org/rfc/rfc4122.txt,
       https://gist.github.com/franks42/4159427,
       https://github.com/clojure/clojurescript/pull/194"
  ([max-length] (get-substr-by-len (uuid-str) 0 max-length))
  ([]
   #?(:clj (str (java.util.UUID/randomUUID))
      :cljs
      (let [^string quad-hex
            (fn []
              (let [unpadded-hex ^string (.toString (rand-int 65536) 16)]
                (case (count   unpadded-hex)
                  1 (str "000" unpadded-hex)
                  2 (str "00"  unpadded-hex)
                  3 (str "0"   unpadded-hex)
                  (do          unpadded-hex))))

            ver-trip-hex ^string (.toString (bit-or 0x4000 (bit-and 0x0fff (rand-int 65536))) 16)
            res-trip-hex ^string (.toString (bit-or 0x8000 (bit-and 0x3fff (rand-int 65536))) 16)]

        (str (quad-hex) (quad-hex) "-" (quad-hex) "-" ver-trip-hex "-" res-trip-hex "-"
          (quad-hex) (quad-hex) (quad-hex))))))

(comment (qb 1e4 (uuid-str 5)))

(defn into-str
  "Simple Hiccup-like string templating to complement Tempura."
  [& xs]
  (str
    (reduce
      (fn rf [acc in]
        (if (sequential? in)
          (reduce rf acc in)
          (sb-append acc (str in))))
      (str-builder)
      xs)))

(comment
  (let [br "\n\n"]
    (into-str :a :b br :c (for [n (range 5)] [n br])
      (when true [:d :e [:f :g]]))))

(defn const-str=
  "Constant-time string equality checker.
  Useful to prevent timing attacks, etc."
  [s1 s2]
  (when (and s1 s2)

    #?(:clj
       (const-ba=
         (.getBytes ^String s1 "UTF-8")
         (.getBytes ^String s2 "UTF-8"))

       :cljs
       (let [vx ["0" "1"]
             v1 (vec   s1)
             v2 (vec   s2)
             n1 (count v1)
             n2 (count v2)
             nmax (max n1 n2)
             nmin (min n1 n2)]

         (reduce-n
           (fn [acc idx]
             (if (>= idx nmin)
               (and (= (get vx   0) (get vx   1)) acc)
               (and (= (get v1 idx) (get v2 idx)) acc)))
           true
           nmax)))))

(comment (const-str= "foo" ""))

;;;; Thread locals

#?(:clj
   (defmacro thread-local-proxy "Low-level, see `thread-local` instead."
     [& body] `(proxy [ThreadLocal] [] (initialValue [] (do ~@body)))))

#?(:clj
   (defn thread-local*
     "Low-level, see `thread-local` instead."
     {:added "v3.48.0 (2023-01-25)"}
     [init-val-fn]
     (let [p (proxy [ThreadLocal] [] (initialValue [] (init-val-fn)))]
       (reify clojure.lang.IDeref (deref [this] (.get p))))))

#?(:clj
   (defmacro thread-local
     "Given a body that returns an initial value for the current thread,
     returns a `ThreadLocal` proxy that can be derefed to get the current
     thread's current value.

     Commonly used to achieve thread safety during Java interop.
     In the common case, `body` will be a call to some Java constructor
     that returns a non-thread-safe instance.

     Example:
       (def thread-local-simple-date-format_
         \"Deref to return a thread-local `SimpleDateFormat`\"
         (thread-local (SimpleDateFormat. \"yyyy-MM-dd\")))

       (.format @thread-local-simple-date-format_ (Date.)) => \"2023-01-24\"

     NB: don't pass the derefed value to other threads!"
     {:added "v3.48.0 (2023-01-25)"}
     [& body] `(thread-local* (fn [] ~@body))))

(comment
  (def      thread-local-simple-date-format_ (thread-local (SimpleDateFormat. "yyyy-MM-dd")))
  (.format @thread-local-simple-date-format_ (Date.))

  ;; thread-local-proxy is a little faster ; [46.54 77.19]
  (let [tlp (thread-local-proxy "init-val")
        tl_ (thread-local       "init-val")]
    (qb 1e6 (.get ^ThreadLocal tlp) @tl_)))

;;;; (S)RNG, etc.

#?(:clj
   (compile-if (fn [] (java.security.SecureRandom/getInstanceStrong)) ; Java 8+, blocking
     (def ^:private srng* (thread-local-proxy (java.security.SecureRandom/getInstanceStrong)))
     (def ^:private srng* (thread-local-proxy (java.security.SecureRandom/getInstance "SHA1SRNG")))))

#?(:clj
   (do
     (defn secure-rng
       "Returns a thread-local `java.security.SecureRandom`.
       Favours security over performance. Automatically re-seeds occasionally.
       May block while waiting on system entropy!"
       ^java.security.SecureRandom []
       (let [rng ^java.security.SecureRandom (.get ^ThreadLocal srng*)]
         ;; Occasionally supplement current seed for extra security.
         ;; Otherwise an attacker could *theoretically* observe large amounts of
         ;; srng output to determine initial seed, Ref. https://goo.gl/MPM91w
         (when (< (.nextDouble rng) 2.44140625E-4) (.setSeed rng (.generateSeed rng 8)))
         rng))

     (defn secure-rng-mock!!!
       "Returns **INSECURE** `java.security.SecureRandom` mock instance backed by
       a seeded deterministic `java.util.Random`. Useful for testing, etc."
       {:added "v3.53.0 (2023-03-22)"}
       ^java.security.SecureRandom [long-seed]
       (let [long-seed    (long              long-seed)
             insecure-rng (java.util.Random. long-seed)]

         (proxy [java.security.SecureRandom] []
           (getAlgorithm [] (str "INSECURE deterministic, seed=" long-seed))
           (nextBytes [^bytes ba] (.nextBytes insecure-rng ba)))))))

(comment :see-tests)

(defn secure-rand-bytes
  "Returns `size` random bytes using `secure-rng` or `js/window.crypto`."
  #?(:clj (^bytes [size] (let [ba (byte-array size)] (.nextBytes (secure-rng) ba) ba))
     :cljs
     ([size]
      (when-let [crypto (.-crypto js/window)]
        (let [ba (js/Uint8Array. size)] (.getRandomValues crypto ba) ba)))))

(comment
  (qb  1e6 (secure-rand-bytes 21)) ; 1021.07
  (do (seq (secure-rand-bytes 21))))

(def           ^:const nanoid-alphabet "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_")
(def ^:private          parse-alphabet
  "Ref.
    Java impl.: https://bit.ly/3dtYv73,
      JS impl.: https://bit.ly/3fYv1zT,
    Motivation: https://bit.ly/2VhWuEO"
  (fmemoize
    (fn [alphabet len]
      (let [an   (count alphabet)
            len  (long  len)
            _    (when (or (< an 1) (> an 256)) (throw (ex-info "`alphabet`: must be ℕ∈[1,256]" {})))
            _    (when (<= len 0)               (throw (ex-info      "`len`: must be ℕ∈[0,∞)"   {})))
            ;;   (2 << (int) Math.floor(Math.log(alphabet.length - 1) / Math.log(2))) - 1;
            mask (dec (bit-shift-left 2 (int (Math/floor (/ (Math/log (dec an)) (Math/log 2))))))
            ;;   (int) Math.ceil(1.6 * mask * size / alphabet.length);
            step (long (Math/ceil (/ (* (* 1.6 mask) len) an)))]

        [mask step (mapv str alphabet)]))))

(comment (qb 1e6 (parse-alphabet nanoid-alphabet 21)))

(defn secure-rand-id
  "Experimental.
  Given `alphabet` (a string of possible characters), returns a securely
  random string of length `len`.

  Trying to do this the obvious/naive way (by repeatedly generating a secure
  random number and mapping it to an alphabet character with modulo) actually
  introduces bias into ids that can be exploited by an attacker.

  The method used here is designed to eliminate that bias.
  Based on https://bit.ly/3dtYv73."

  #?(:clj [alphabet ^long len] :cljs [alphabet len])
  (let [#?(:clj [^byte mask step v] :cljs [mask step v])
        (parse-alphabet alphabet len)

        an (count v)
        sb (str-builder)]

    (loop []
      (let [ba (secure-rand-bytes step)
            result
            (reduce-n
              (fn [acc idx]
                (let [alpha-idx (bit-and mask (aget ba idx))]
                  (if (>= alpha-idx an)
                    acc ; Out of alphabet range
                    (let [acc (sb-append acc (v alpha-idx))]
                      (if (== (count acc) len)
                        (reduced (str acc))
                        (do           acc))))))
              sb
              step)]
        (if (string? result) result (recur))))))

(let [alphabet (mapv str nanoid-alphabet)
      mask     0x3f]

  (defn nanoid
    "Experimental. Optimized variant of `secure-rand-id` that returns Nano IDs
    as in https://github.com/ai/nanoid."
    ([   ] (nanoid 21))
    ([len]
     (let [ba (secure-rand-bytes len) #_(byte-array [81 -125 -54 -45 -108 99])]
       (str
         (reduce-n
           (fn [acc idx]
             (sb-append acc (alphabet (bit-and mask (aget ba idx)))))
           (str-builder)
           (alength ba)))))))

(comment (qb 1e5 (secure-rand-id nanoid-alphabet 21) (nanoid) (uuid-str))) ; [343.31 205.98 82.86]

;;;; Hex strings

#?(:clj
   (defn ident-hex-str
     "Returns hex string of given Object's `identityHashCode` (e.g. \"0x5eeb49f2\")."
     {:added "v3.56.0 (2023-03-29)"}
     ^String [obj] (str "0x" (Integer/toHexString (System/identityHashCode obj)))))

#?(:clj
   (do
     (let [hex-chars (mapv identity "0123456789abcdef")
           byte->char-pair
           (fn [^StringBuilder sb b]
             (let [v (bit-and ^byte b 0xFF)]
               (.append sb (hex-chars (bit-shift-right v 4)))
               (.append sb (hex-chars (bit-and         v 0x0F)))))]

       (defn ba->hex-str
         "Returns byte[] for given hex string."
         {:added "v3.53.0 (2023-03-22)"}
         ^String [^bytes ba]
         (str (reduce (fn [sb b] (byte->char-pair sb b)) (StringBuilder.) ba))))

     (let [char-pair->byte
           (fn [[c1 c2]]
             (let [d1 (Character/digit ^char c1 16)
                   d2 (Character/digit ^char c2 16)]
               (unchecked-byte (+ (bit-shift-left d1 4) d2))))]

       (defn hex-str->ba
         "Returns hex string for given byte[]."
         {:added "v3.53.0 (2023-03-22)"}
         ^bytes [^String s]
         (if (even? (count s))
           (byte-array (into [] (comp (partition-all 2) (map char-pair->byte)) s))
           (throw
             (ex-info "Invalid hex string (length must be even)"
               {:given {:value s, :type (type s)}})))))

     ;; TODO Any way to auto select fastest implementation?
     ;; Ref. https://stackoverflow.com/a/58118078/1982742
     ;; Auto selection seems to cause problems with AOT and/or Graal

     #_
     (compile-if com.google.common.io.BaseEncoding
       (let [encoding (.lowerCase (com.google.common.io.BaseEncoding/base16))]
         (defn- ba->hex-str-guava ^String [^bytes ba] (.encode encoding ba))
         (defn- hex-str->ba-guava ^bytes  [^String s] (.decode encoding s))))

     #_
     (compile-if org.apache.commons.codec.binary.Hex
       (defn- ba->hex-str-commons ^String [^bytes ba] (org.apache.commons.codec.binary.Hex/encodeHexString ba))
       (defn- hex-str->ba-commons ^bytes  [^String s] (org.apache.commons.codec.binary.Hex/decodeHex       s)))

     #_
     (compile-if java.util.HexFormat ; Fastest, needs Java 17+
       (let [hf (java.util.HexFormat/of)]
         (defn- ba->hex-str-hf ^String [^bytes ba] (.formatHex hf ba))
         (defn- hex-str->ba-hf ^bytes  [^String s] (.parseHex  hf s))))))

(comment :see-tests)
(comment
  (vec (-hex-str->ba (-ba->hex-str (byte-array [1 2 3 4 5 6 120 -120 127]))))
  (let [ba (byte-array (range -128 128))]
    (qb 1e4 (hex-str->ba (ba->hex-str ba)))))

;;;; Sorting

#?(:cljs (defn rcompare "Reverse comparator." [x y] (compare y x))
   :clj  (defn rcompare "Reverse comparator."
           {:inline (fn [x y] `(. clojure.lang.Util compare ~y ~x))}
           [x y] (compare y x)))

(defn sortv
  "Like `core/sort` but:
    - Returns a vector.
    - `comparator` can be `:asc`, `:desc`, or an arbitrary comparator.
    - An optional `keyfn` may be provided, as in `core/sort-by`."
  ;; A little faster than `(vec (sort ...))` when `coll` very large
  ([                  coll] (sortv nil :asc       coll))
  ([       comparator coll] (sortv nil comparator coll))
  ([?keyfn comparator coll]
   (if-not (seq coll)
     []
     (let [comparator
           (case comparator
             :asc                    compare
             (:dsc :desc) (fn [x y] (compare y x))
             comparator)

           comparator
           (if-let [kfn (when (not= ?keyfn identity) ?keyfn)]
             (fn [x y] (comparator (kfn x) (kfn y)))
             comparator)

           a (to-array coll)]

       #?(:clj  (java.util.Arrays/sort a ^java.util.Comparator comparator)
          :cljs (garray/stableSort     a (fn->comparator       comparator)))

       (with-meta (vec a) (meta coll))))))

(comment
  (sortv second :desc [[1 10] [2 9] [3 8] [4 7] [5 6]])
  (let [v (vec (range 1e5))] (qb 1e2 (vec (sort v)) (sortv v) (sortv :desc v))))

(let [sentinel (new-object)
      nil->sentinel (fn [x] (if (nil? x) sentinel x))
      sentinel->nil (fn [x] (if (identical? x sentinel) nil x))]

  (defn reduce-top
    "Reduces the top `n` items from `coll` of N items.
    Clj impln is O(N.logn) vs O(N.logN) for (take n (sort-by ...))."
    ([n           rf init coll] (reduce-top n identity compare rf init coll))
    ([n keyfn     rf init coll] (reduce-top n keyfn    compare rf init coll))
    ([n keyfn cmp rf init coll]
     (let [coll-size (count coll)
           n (long (min coll-size (long n)))]

       (if-not (pos? n)
         init
         #?(:cljs ; TODO Real impl.
            (transduce (take n) (completing rf) init
              (sort-by keyfn cmp coll))

            :clj
            (let [pq (java.util.PriorityQueue. coll-size
                       (fn [x y] (cmp (keyfn (sentinel->nil x))
                                      (keyfn (sentinel->nil y)))))]

              (run! #(.offer pq (nil->sentinel %)) coll)
              (reduce-n (fn [acc _] (rf acc (sentinel->nil (.poll pq))))
                init n))))))))

(defn top-into
  "Conjoins the top `n` items from `coll` into `to` using `reduce-top`."
  ([to n           coll] (top-into to n identity compare coll))
  ([to n keyfn     coll] (top-into to n keyfn    compare coll))
  ([to n keyfn cmp coll]
   (if (editable? to)
     (persistent! (reduce-top n keyfn cmp conj! (transient to) coll))
     (do          (reduce-top n keyfn cmp conj             to  coll)))))

(defn top
  "Returns a sorted vector of the top `n` items from `coll` using `reduce-top`."
  ([n           coll] (top-into [] n identity compare coll))
  ([n keyfn     coll] (top-into [] n keyfn    compare coll))
  ([n keyfn cmp coll] (top-into [] n keyfn    cmp     coll)))

(comment [(top 20 [2 3 5 3 88 nil]) (sort [2 3 5 3 88 nil])])

;;;; Date & time

(defn secs->ms ^long [secs] (* (long secs)  1000))
(defn ms->secs ^long [ms]   (quot (long ms) 1000))
(defn ms "Returns ~number of milliseconds in period defined by given args."
  [& {:as opts :keys [years months weeks days hours mins secs msecs ms]}]
  (have? #{:years :months :weeks :days :hours :mins :secs :msecs :ms}
    :in (keys opts))
  (round0
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

#?(:clj
   (defmacro msecs "Compile-time version of `ms`" [& opts]
     (eval `(taoensso.encore/ms ~@opts))))

(comment (macroexpand '(msecs :weeks 3)))

#?(:clj
   (defn- -simple-date-format
     "Returns a SimpleDateFormat ThreadLocal proxy."
     [pattern locale timezone]
     (let [pattern
           (case pattern
             :iso8601 "yyyy-MM-dd'T'HH:mm:ss.SSSX"
             :rss2    "EEE, dd MMM yyyy HH:mm:ss z"
             pattern)

           locale
           (if (kw-identical? locale :jvm-default)
             nil ; (Locale/getDefault)
             locale)

           timezone
           (if (kw-identical? timezone :jvm-default)
             nil ; (TimeZone/getDefault)
             (if (kw-identical? timezone :utc)
               (TimeZone/getTimeZone "UTC")
               timezone))]

       (thread-local-proxy
         (let [^SimpleDateFormat sdf
               (if locale
                 (SimpleDateFormat. ^String pattern ^Locale locale)
                 (SimpleDateFormat. ^String pattern))]
           (when timezone (.setTimeZone sdf ^TimeZone timezone))
           sdf)))))

#?(:clj
   (defn simple-date-format*
     ^java.text.SimpleDateFormat [pattern locale timezone]
     (.get ^ThreadLocal (-simple-date-format pattern locale timezone))))

#?(:clj
   (defn simple-date-format "Returns a thread-local `java.text.SimpleDateFormat`."
     ^java.text.SimpleDateFormat [pattern & [{:keys [locale timezone] :as opts}]]
     (.get ^ThreadLocal (-simple-date-format pattern locale timezone))))

(comment (qb 1e5 (.format (simple-date-format "yyyy-MMM-dd") (Date.))))

;;;; Macro env

#?(:clj
   (defmacro get-env []
     (let [ks (reduce
                (fn [acc in]
                  (if (str-starts-with? (name in) "__") ; Hide privates
                    acc ; Strip primitive tags which can cause issues:
                    (conj acc (without-meta in))))
                [] (keys &env))]
       `(zipmap '~ks ~ks))))

(comment [(let [x :x] (get-env)) ((fn [^long x] (get-env)) 0)])

;;;; IO

#?(:clj (defn  get-sys-val ([id] (get-sys-val  id id)) ([prop-id env-id] (or (System/getProperty prop-id) (System/getenv env-id)))))
#?(:clj (defn read-sys-val ([id] (read-sys-val id id)) ([prop-id env-id] (when-let [s (get-sys-val prop-id env-id)] (read-edn s)))))

#?(:clj
   (defn get-sys-bool
     "If `prop-id` JVM property or `env-id` environment variable are set:
       - Returns `true`  if set value is ∈ #{\"true\"  \"1\" \"t\" \"T\" \"TRUE\"}
       - Returns `false` if set value is ∈ #{\"false\" \"0\" \"f\" \"F\" \"FALSE\"}
       - Otherwise throws

     Returns `default` if neither property nor environment variable is set."
     [default prop-id env-id]
     (if-let [sv (get-sys-val prop-id env-id)]
       (case  sv
         ("1" "t" "true"  "T" "TRUE")  true
         ("0" "f" "false" "F" "FALSE") false
         (throw
           (ex-info "[encore/get-sys-bool] Unexpected value (expected ∈ #{ \"true\" \"false\" ...})"
             {:value   sv
              :prop-id prop-id
              :env-id  env-id
              :default default})))

       default)))

#?(:clj
   (defn slurp-resource
     "Returns slurped named resource on classpath, or nil when resource not found."
     [rname]
     (when-let [r (io/resource rname)]
       (try
         (slurp (io/reader r))
         (catch Exception e
           (throw (ex-info "[encore/slurp-resource] Slurp failed" {:rname rname} e)))))))

(defn- auto-env [prop] (when prop (str-replace (str/upper-case prop) #"[.-]" "_")))
(comment (auto-env "foo.bar.baz.edn"))

#?(:clj
   (let [load-edn
         (fn [default-config edn source ?error-data]
           (let [clj
                 (try
                   (read-edn edn)
                   (catch Throwable t
                     (throw
                       (ex-info "[enc/load-edn-config] Error reading config EDN"
                         (conj {:edn edn :source source} ?error-data)
                         t))))

                 config
                 (if (symbol? clj)
                   (if-let [var
                            (or
                              (resolve clj)
                              (when-let [ns (namespace clj)]
                                (require ns)
                                (resolve clj)))]
                     @var
                     (throw
                       (ex-info "[enc/load-edn-config] Failed to resolve config symbol"
                         (conj {:edn edn :symbol symbol :source source} ?error-data))))
                   clj)]

             (if (map? config)
               {:source source
                :config
                (cond
                  (get    config :load/overwrite?) ; Undocumented
                  (dissoc config :load/overwrite?) ; Without merge

                  (empty?             default-config) config
                  :else (nested-merge default-config  config))}

               (throw
                 (ex-info "[enc/load-edn-config] Unexpected config value type"
                   (conj {:config {:value config :type (type config)}} ?error-data))))))]

     (defn load-edn-config
       "Attempts to read config as EDN from the following (in descending order):
         1. JVM property,   if opt is provided and value    is present.
         2. Env var,        if opt is provided and value    is present.
         3. Resource file,  if opt is provided and resource is present.

       Returns nil, or {:config           <loaded-config>,
                        :source <source-of-loaded-config>}.

       Throws if value/resource cannot be successfully read as EDN.
       The read EDN value must be a map, or a symbol that resolves to a map.

       Useful for libraries, etc. that want to provide easily modified config.
       Used by Timbre, Carmine, etc.

       Options:
         - default    ; Default config map into which a nested merge will be done
         - prop       ; Name of JVM property  to check (e.g. \"taoensso.timbre.config.edn\")
         - env        ; Name of Env var       to check (e.g. \"TAOENSSO_TIMBRE_CONFIG_EDN\")
         - res        ; Name of resource file to check (e.g. \"taoensso.timbre.config.edn\")
         - auto-env?  ; If true, `env` will be provided automatically based on `prop`
         - error-data ; Optional map to be added to ex-data if load fails"

       {:added "v3.39.0 (2022-11-23)"}
       [{:as   opts
         :keys [default prop env res auto-env? error-data]
         :or   {auto-env? true}}]

       (let [{:keys [res-prop res-env]} opts ; Undocumented
             env     (or     env (auto-env     prop))
             res-env (or res-env (auto-env res-prop))]

         (have? [:or nil? map?] default)

         (if-let [[edn source]
                  (or
                    (when-let [test-edn (get opts :test-edn)] [test-edn :test-edn])
                    (when prop (when-let [edn (System/getProperty prop)] [edn {:jvm-prop prop}]))
                    (when env  (when-let [edn (System/getenv       env)] [edn {:env-var   env}]))
                    (when-let
                        [res ; Configurable resource name, undocumented
                         (or
                           (when res-prop (System/getProperty res-prop))
                           (when res-env  (System/getenv      res-env))
                           res)]

                      (when-let [edn (slurp-resource res)]
                        [edn {:resource res}])))]

           (load-edn default edn source error-data)
           (when     default
             {:config  default
              :source :default}))))))

(comment
  (load-edn-config {})
  (load-edn-config
    {:default  {:default? true}
     :prop     "taoensso.timbre.config.edn"
     :res      "taoensso.timbre.config.edn"
     :test-edn (pr-edn {:test-edn? true})}))

#?(:clj
   (defn get-file-resource-?last-modified
     "Returns last-modified time for file backing given named resource, or nil
     if file doesn't exist."
     [rname]
     (when-let [file (try (->> rname io/resource io/file) (catch Exception _))]
       (.lastModified ^java.io.File file))))

#?(:clj
   (def file-resources-modified?
     "Returns true iff any files backing the given named resources have changed
     since last call."
     (let [udts_ (atom {}) ; {<rnames> <udt-or-udts>}
           swap! (fn [ks v] (swap-in! udts_ ks (fn [?v] (swapped v (when (not= v ?v) v)))))
           rnames->rgroup (fmemoize (fn [rnames] (into (sorted-set) rnames)))]
       (fn [rnames & [?id]]
         (let [rgroup (rnames->rgroup rnames)]
           (swap! [?id rgroup] (mapv get-file-resource-?last-modified rgroup)))))))

#?(:clj
   (def slurp-file-resource
     "Like `slurp-resource` but caches slurps against file's last-modified udt."
     (let [;; {<rname> [<content_> <last-modified-udt>]}
           cache_ (atom {})]
       (fn [rname]
         (let [curr-udt (or (get-file-resource-?last-modified rname) -1)]
           (force
             (swap-in! cache_ [rname]
               (fn [[?prev-content_ ?prev-udt :as ?v]]
                 (if (= curr-udt ?prev-udt)
                   (swapped ?v ?prev-content_)
                   (let [content_ (delay (slurp-resource rname))]
                     (swapped [content_ curr-udt] content_)))))))))))

(comment (slurp-file-resource "log4j.properties"))

#?(:clj
   (defn get-pom-version
     "Returns POM version string for given Maven dependency, or nil."
     [dep-sym]
     (let [path (clojure.core/format "META-INF/maven/%s/%s/pom.properties"
                  (or (namespace dep-sym)
                    (name      dep-sym))
                  (name dep-sym))]
       (when-let [props (io/resource path)]
         (with-open [stream (io/input-stream props)]
           (let [props (doto (java.util.Properties.) (.load stream))]
             (.getProperty props "version")))))))

(comment (get-pom-version 'com.taoensso/encore))

#?(:clj
   (defn get-hostname "Returns local hostname string, or nil."
     []
     (try (.getHostName (java.net.InetAddress/getLocalHost))
          (catch java.net.UnknownHostException _ nil))))

(comment (get-hostname))

;;;; Async

#?(:clj
   (let [ap   (fn [] (.availableProcessors (Runtime/getRuntime)))
         perc (fn [n] (max 1 (long (Math/floor (* (/ (double (ap)) 100.0) (double n))))))]

     (defn- get-num-threads [n-threads]
       (if (vector? n-threads)
         (let [[kind n] n-threads]
           (case kind
             :num  (have pos-int? n)
             :perc (perc          n)
             (unexpected-arg! kind
               :context  `get-num-threads
               :expected #{:num :perc})))
         (have pos-int? n-threads)))))

(comment (get-num-threads [:perc 90]))

#?(:clj
   (defn future-pool
     "Returns a simple semaphore-limited wrapper of Clojure's standard `future`:
       (fn
         [f] - Blocks to acquire a future, then executes (f) on that future.
         [ ] - Blocks to acquire all futures, then immediately releases them.
               Useful for blocking till all outstanding work completes.
     Timeout variants are also provided."

     ;; TODO Optionally use an independent pool (=> need for shutdown control)
     [n-threads]
     (let [n    (long (get-num-threads n-threads)) ; Undocumented special vec support
           s    (java.util.concurrent.Semaphore. n)
           msecs java.util.concurrent.TimeUnit/MILLISECONDS
           fp-call
           (fn [f]
             (if (fn? f)
               (future (try (f) (finally (.release s))))
               (do
                 (.release s)
                 (throw
                   (ex-info "[encore/future-pool] Unexpected arg type (expected function)"
                     {:arg {:value f :type (type f)}})))))]

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
            timeout-val))))))

(comment
  (time
    (let [fp (future-pool 2)]
      [(fp (fn [] (Thread/sleep 2000) (println "2000")))
       (fp (fn [] (Thread/sleep 500)  (println "500")))
       (fp 200 "timeout" (fn [] (Thread/sleep 900) (println "900")))
       (fp (fn [] (Thread/sleep 3000) (println "3000")))
       (fp)])))

#?(:clj
   (defn pre-cache
     "Given a nullary fn `f` that is non-idempotent and free of side effects,
     returns a wrapped version of `f` that asynchronously maintains a cache
     of up to `n-capacity` pre-computed return values of (f).

     Useful when `f` is expensive & may be called in a spikey fashion,
     e.g. ideal for cryptographic key generators."

     {:added "v3.49.0 (2023-02-01)"}
     ([n-capacity                 f] (pre-cache n-capacity 1 f))
     ([n-capacity fp-or-n-threads f]
      (let [queue (java.util.concurrent.ArrayBlockingQueue. n-capacity)
            f* (fn [] (try {:okay (f)} (catch Throwable t {:error t})))

            async-replenish!
            (let [a_ (agent nil)
                  fp
                  (if       (fn? fp-or-n-threads)
                    (do          fp-or-n-threads)
                    (future-pool fp-or-n-threads))]

              (fn async-replenish! []
                (send-off a_
                  (fn [_] (fp (fn [] (.offer queue (f*)))) nil))))]

        (dotimes [_ n-capacity] (async-replenish!)) ; Initialize cache

        (fn take1 []
          (let [f*-result
                (if-let [cached (.poll queue)]
                  (do (async-replenish!) cached)
                  ;; Block to call f synchronously on calling thread.
                  ;; Not limited by fp => pre-cached f will never be slower
                  ;; than uncached f, even if `n-threads` is small.
                  (f*))]

            (if-let [t (get f*-result :error)]
              (throw t)
              (get f*-result :okay))))))))

(comment :see-tests)
(comment
  (let [fp (future-pool 6)] ; Shared future pool
    (def f1 (pre-cache 3 fp (fn [] (Thread/sleep 1000) :f1)))
    (def f2 (pre-cache 4 fp (fn [] (Thread/sleep 500)  :f2)))))

;;;; Benchmarking

#?(:clj
   (defmacro time-ms "Returns number of milliseconds it took to execute body."
     [& body] `(let [t0# (now-udt*)] ~@body (- (now-udt*) t0#))))

#?(:clj
   (defmacro time-ns "Returns number of nanoseconds it took to execute body."
     [& body] `(let [t0# (now-nano*)] ~@body (- (now-nano*) t0#))))

#?(:clj
   (defmacro quick-bench
     "Simple util to benchmark/compare runtime of given form/s.

     Runs sets of laps for each given form, recording the total runtime of each set.
     Returns the the total runtime in msecs of the fastest set of laps for each form.

       (quick-bench [<num-sets> <num-laps>] <form1> <form2> <...>) =>
         [<total runtime msecs of fastest set of laps for form1>
          <total runtime msecs of fastest set of laps for form2>
          <...>]

        Total number of runs for each form is: `num-sets` * `num-laps`

     If omitted, the default `num-sets` is 6 (to include warmup):
       (quick-bench <num-laps> <form1> <form2> <...>)

     Example (comparing runtime of `first` and `nth` against vector):
       (let [v [:a]] (quick-bench 1e6 (first v) (nth v 0))) => [67.43 39.05]"

     ([spec form & more] (mapv (fn [form] `(quick-bench ~spec ~form)) (cons form more)))
     ([spec form]
      `(let [spec# ~spec
             ;; Default 3 warmup + 3 working sets:
             [num-sets# num-laps#] (if (vector? spec#) spec# [6 spec#])]
         (have? pos-num? num-sets# num-laps#)
         (round2
           (/ (double
                (reduce min
                  (for [_# (range num-sets#)]
                    (time-ns (dotimes [_# num-laps#] (do ~form))))))
             1e6))))))

#?(:clj (defalias qb quick-bench))

(comment (let [v [:a]] (qb [4 1e6] (first v) (nth v 0))))

#?(:clj
   (defn bench*
     "Repeatedly executes fn and returns time taken to complete execution."
     [nlaps {:keys [nlaps-warmup nthreads as-ns?]
             :or   {nlaps-warmup 0
                    nthreads     1}} f]
     (try
       (dotimes [_ nlaps-warmup] (f))
       (let [nanosecs
             (if (= nthreads 1)
               (time-ns (dotimes [_ nlaps] (f)))
               (let [nlaps-per-thread (/ nlaps nthreads)]
                 (time-ns
                   (let [futures (repeatedly-into [] nthreads
                                   (fn [] (future (dotimes [_ nlaps-per-thread] (f)))))]
                     (mapv deref futures)))))]
         (if as-ns? nanosecs (round0 (/ nanosecs 1e6))))
       (catch Throwable t
         (println (str "Bench failure: " (.getMessage t)))
         -1))))

#?(:clj (defmacro bench [nlaps opts & body] `(bench* ~nlaps ~opts (fn [] ~@body))))

;;;; Browser stuff

#?(:cljs
   (do ; Basic browser logging
     (def ^:private console-log
       (if-not (exists? js/console)
         (fn [& xs] nil)
         (fn [& xs] (when-let [f js/console.log]
                      (.apply f js/console (into-array xs))))))

     (def  log console-log) ; Raw args
     (defn logp [    & xs] (console-log (str-join " " (map nil->str) xs)))
     (defn sayp [    & xs] (js/alert    (str-join " " (map nil->str) xs)))
     (defn logf [fmt & xs] (console-log (format* fmt xs)))
     (defn sayf [fmt & xs] (js/alert    (format* fmt xs)))))

#?(:cljs
   (defn get-win-loc "Returns `js/window`'s current location as a map."
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
          }))))

#?(:cljs
   (do
     (def ^:private default-xhr-pool_ (delay (goog.net.XhrIoPool.)))
     (def ^:private js-form-data? (if (exists? js/FormData) (fn [x] (instance? js/FormData x)) (fn [x] nil)))
     (def ^:private js-file?      (if (exists? js/File)     (fn [x] (instance? js/File     x)) (fn [x] nil)))
     (def ^:private coerce-xhr-params "Returns [<uri> <?data>]"
       (let [url-encode
             (fn url-encode
               ([params]
                (when (seq params)
                  (-> params clj->js gquery-data/createFromMap .toString)))

               ([uri params]
                (let [qstr (url-encode params)
                      uri-with-query (if (str/blank? qstr) uri (str uri "?" qstr))]
                  [uri-with-query nil])))

             adaptive-encode
             (fn [uri params]
               (cond
                 (js-form-data? params) [uri params]
                 :do (have? map? params)

                 (and    (exists? js/FormData) (rsome js-file? (vals params)))
                 (let [form-data (js/FormData.)]
                   (doseq [[k v] params] (.append form-data (name k) v))
                   [uri form-data])

                 ;; Avoiding FormData as default since default Compojure
                 ;; middleware doesn't seem to keywordize FormData keys?
                 :else [uri (url-encode params)]))]

         (fn [uri method params]
           (have? [:or nil? map? js-form-data?] params)
           (case method
             :get  (url-encode      uri params)
             :post (adaptive-encode uri params)
             :put  (adaptive-encode uri params)))))))

#?(:cljs
   (defn ajax-lite
     "Alpha, subject to change.
     Simple, lightweight Ajax via Google Closure.

     Returns nil, or resulting `goog.net.XhrIo` instance if one was
     immediately available.

     (ajax-lite \"/my-post-route\"
       {:method     :post
        :params     {:username \"Rich Hickey\" :type \"Awesome\"}
        :headers    {\"Foo\" \"Bar\"}
        :resp-type  :text
        :timeout-ms 7000
        :with-credentials? false ; Enable if using CORS (requires xhr v2+)

        :xhr-pool       my-xhr-pool ; `goog.net.XhrIoPool` instance or delay
        :xhr-cb-fn      (fn [xhr])  ; Called with `XhrIo` from pool when available
        :xhr-timeout-ms 2500        ; Max msecs to wait on pool for `XhrIo`
       }
       (fn async-callback-fn [resp-map]
         (let [{:keys [success? ?status ?error ?content ?content-type]} resp-map]
           ;; ?status ; ∈ #{nil 200 404 ...}, non-nil iff server responded
           ;; ?error  ; ∈ #{nil <http-error-status-code> <exception> :timeout
                            :abort :http-error :exception :xhr-pool-depleted}
           (js/alert (str \"Ajax response: \" resp-map)))))"

     [uri
      {:as   opts
       :keys [method params headers timeout-ms resp-type with-credentials?
              xhr-pool xhr-cb-fn xhr-timeout-ms]
       :or
       {method         :get
        timeout-ms     10000
        resp-type      :auto
        xhr-pool       default-xhr-pool_
        xhr-timeout-ms 2500}}

      callback-fn]

     (have? [:or nil? nat-int?] timeout-ms)

     (let [xhr-pool (force xhr-pool)
           with-xhr
           (fn [xhr]
             (catching
               (let [timeout-ms (or (:timeout opts) timeout-ms) ; Deprecated opt
                     xhr-method (case method :get "GET" :post "POST" :put "PUT")

                     [xhr-uri xhr-?data]
                     (coerce-xhr-params uri method params)

                     xhr-headers
                     (let [headers (map-keys #(str/lower-case (name %)) headers)
                           headers (assoc-some headers "x-requested-with"
                                     (get headers "x-requested-with" "XMLHTTPRequest"))]
                       ;; `x-www-form-urlencoded`/`multipart/form-data` content-type
                       ;; will be added by Closure if a custom content-type isn't provided
                       (clj->js headers))

                     ?progress-listener
                     (when-let [pf (:progress-fn opts)]
                       (.setProgressEventsEnabled xhr true)
                       (gevents/listen xhr goog.net.EventType/PROGRESS
                         (fn [ev]
                           (let [length-computable? (.-lengthComputable ev)
                                 loaded (.-loaded ev)
                                 total  (.-total  ev)
                                 ?ratio (when (and length-computable? (not= total 0))
                                          (/ loaded total))]
                             (pf
                               {:?ratio ?ratio
                                :length-computable? length-computable?
                                :loaded loaded
                                :total  total
                                :ev     ev})))))]

                 (doto xhr
                   (gevents/listenOnce goog.net.EventType/READY
                     (fn [_] (.releaseObject xhr-pool xhr)))

                   (gevents/listenOnce goog.net.EventType/COMPLETE
                     (fn wrapped-callback-fn [resp]
                       (let [success? (.isSuccess xhr) ; true iff no error or timeout
                             -status  (.getStatus xhr) ; -1, 200, etc.

                             [?status ?content-type ?content]
                             (when (not= -status -1) ; Got a response from server
                               (let [;; Case insensitive get:
                                     ?content-type (.getResponseHeader xhr "content-type")
                                     ?content
                                     (let [resp-type
                                           (cond
                                             (not= resp-type :auto) resp-type
                                             (nil? ?content-type)   :text
                                             :else
                                             (let [cts (str/lower-case (str ?content-type))
                                                   match? (fn [s] (str-contains? cts s))]
                                               (cond
                                                 (match? "/edn")     :edn
                                                 (match? "/json")    :json
                                                 (match? "/xml")     :xml
                                                 ;; (match? "/html") :text
                                                 :else               :text)))]

                                       (catching
                                         (case resp-type
                                           :edn  (read-edn (.getResponseText xhr))
                                           :json           (.getResponseJson xhr)
                                           :xml            (.getResponseXml  xhr)
                                           :text           (.getResponseText xhr))

                                         _e ; Undocumented, subject to change:
                                         {:ajax/bad-response-type resp-type
                                          :ajax/resp-as-text (.getResponseText xhr)}))]

                                 [-status ?content-type ?content]))]

                         (when ?progress-listener
                           (gevents/unlistenByKey ?progress-listener))

                         (callback-fn
                           {:raw-resp      resp
                            :xhr           xhr ; = (.-target resp)
                            :success?      success?
                            :?status       ?status
                            :?content-type ?content-type
                            :?content      ?content
                            :?error
                            (if success?
                              nil
                              (cond
                                ?status ?status ; Http error status code (e.g. 404)
                                :else
                                (get {goog.net.ErrorCode/NO_ERROR   nil
                                      goog.net.ErrorCode/EXCEPTION  :exception
                                      goog.net.ErrorCode/HTTP_ERROR :http-error
                                      goog.net.ErrorCode/ABORT      :abort
                                      goog.net.ErrorCode/TIMEOUT    :timeout}
                                  (.getLastErrorCode xhr)
                                  :unknown)))})))))

                 (.setTimeoutInterval xhr (or timeout-ms 0)) ; nil = 0 = no timeout
                 (when with-credentials?
                   (.setWithCredentials xhr true)) ; Requires xhr v2+

                 (.send xhr xhr-uri xhr-method xhr-?data xhr-headers)

                 (when-let [cb xhr-cb-fn] (catching (cb xhr)))
                 xhr)

               e
               (do
                 (.releaseObject xhr-pool xhr)
                 (callback-fn {:?error e})
                 nil)))]

       (cond
         :if-let [xhr (.getObject xhr-pool)] ; Available immediately
         (with-xhr xhr)

         (or
           (nil?  xhr-timeout-ms)
           (zero? xhr-timeout-ms))
         (do (callback-fn {:?error :xhr-pool-depleted}) nil)

         :else
         (let [done?_ (atom false)]

           (.setTimeout js/window
             (fn xhr-timeout []
               (when (compare-and-set! done?_ false true)
                 (callback-fn {:?error :xhr-pool-timeout})))
             xhr-timeout-ms)

           (.getObject xhr-pool
             (fn xhr-cb [xhr]
               ;; We've acquired xhr after some time
               (if (compare-and-set! done?_ false true)
                 (with-xhr                xhr)
                 (.releaseObject xhr-pool xhr))))
           nil)))))

;;;; Ring

#?(:clj
   (defn session-swap
     "Small util to help correctly manage (modify) funtional sessions. Please use
     this when writing Ring middleware! It's *so* easy to get this wrong and end up
     with subtle, tough-to-diagnose issues."
     [rreq rresp f & args]
     (when rresp
       (let [base (get rresp :session (get rreq :session))
             new-session (if args (apply f base args) (f base))]
         (assoc rresp :session new-session)))))

#?(:clj
   (defn normalize-headers [rreq-or-rresp]
     (when rreq-or-rresp
       (assoc rreq-or-rresp :headers (map-keys str/lower-case (:headers rreq-or-rresp))))))

(comment (normalize-headers {:headers {"Foo1" "bar1" "FOO2" "bar2" "foo3" "bar3"}}))

#?(:clj
   (defn -ring-merge-headers
     "Experimental."
     [h1 h2]
     (reduce-kv
       (fn [m k2 v2]
         (if-let [e1 (find h1 k2)]
           (let [v1 (val e1)
                 v3
                 (if (vector? v1)
                   (if (vector? v2)
                     (if (:add (meta v2)) ; vec <- vec
                       (into v1 v2)
                       (do      v2))
                     (conj v1 v2))        ; vec <- el
                   (if (vector? v2)
                     (if (:add (meta v2)) ; el <- vec
                       (into [v1] v2)
                       (do        v2))
                     #_[v1 v2] v2))       ; el <- el
                 ]

             (assoc m k2 v3))
           (assoc   m k2 v2)))
       h1
       h2)))

(comment
  (-ring-merge-headers
    {"a" "A1" "b"        "B1"  "c" "C1"}
    {"a" "A2" "b" ^:add ["B2"] "d" "D2"}))

#?(:clj
   (do
     (defn ring-resp-map        [x] (when x (if (map? x) x {:body x})))
     (defn ring-set-body        [body    rresp] (assoc (ring-resp-map rresp) :body    body))
     (defn ring-set-status      [code    rresp] (assoc (ring-resp-map rresp) :status  code))
     (defn ring-set-headers     [headers rresp] (assoc (ring-resp-map rresp) :headers headers))
     (defn ring-default-headers [headers rresp] (assoc (ring-resp-map rresp) :headers (-ring-merge-headers headers (get rresp :headers))))
     (defn ring-merge-headers   [headers rresp] (assoc (ring-resp-map rresp) :headers (-ring-merge-headers (get rresp :headers) headers)))))

(comment
  (ring-merge-headers {"BAR" "baz"} {:body "foo"})
  (ring-merge-headers {"bar" "baz"} "foo"        )
  (ring-merge-headers {"bar" ^:add ["baz2"]} {:body "foo" :headers {"bar" "baz1"}}))

#?(:clj
   (defn ring-redirect-resp
     ([     url      ] (ring-redirect-resp :temp url nil))
     ([kind url      ] (ring-redirect-resp kind  url nil))
     ([kind url flash]
      {:headers {"location" url}
       :body    nil
       :flash   flash
       :status
       (case kind
         (301 :permanent :perm)     301
         (302 :temporary :temp nil) 302
         kind)})))

(comment (ring-redirect-resp 303 "/foo" "boo!"))

(defn url-encode "Based on https://goo.gl/fBqy6e"
  #?(:clj  [s & [encoding]] :cljs [s])
  (when s
    #?(:clj  (-> (str s)
               (java.net.URLEncoder/encode (str (or encoding "UTF-8")))
               (str/replace "*" "%2A") ; Cautious, https://stackoverflow.com/a/25149577/1982742
               (str/replace "+" "%20") ; Cautious, https://stackoverflow.com/a/40292770/1982742
               )
       :cljs (-> (str s)
               (js/encodeURIComponent s)
               (str/replace "*" "%2A")))))

(defn url-decode "Stolen from http://goo.gl/99NSR1"
  [s & [encoding]]
  (when s
    #?(:clj  (java.net.URLDecoder/decode (str s) (str (or encoding "UTF-8")))
       :cljs (js/decodeURIComponent      (str s)))))

(comment
  (url-decode (url-encode "Hello there"))
  (url-decode "hello+there"))

(defn format-query-string [m]
  (let [param (fn [k v]  (str (url-encode (as-qname k)) "="
                              (url-encode (or (as-?qname v) (str v)))))
        join  (fn [strs] (str/join "&" strs))]
    (if (empty? m)
      ""
      (join
        (for [[k v] m :when (some? v)]
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

(defn parse-query-params "Based on `ring-codec/form-decode`."
  [s & [keywordize? encoding]]
  (if (or (str/blank? s) (not (str-contains? s "=")))
    {}
    (let [;; For convenience (e.g. JavaScript win-loc :search)
          s (if (str-starts-with? s "?") (subs s 1) s)
          m (reduce
              (fn [m param]
                (if-let [[k v] (str/split param #"=" 2)]
                  (assoc-conj m (url-decode k encoding) (url-decode v encoding))
                  m))
              {}
              (str/split s #"&"))]
      (if-not keywordize?
        m
        (map-keys keyword m)))))

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

;;;; Stubs

(do
  #?(:cljs (defn -new-stubfn_ [name] (volatile! (fn [& args]  (throw (ex-info (str "[encore/stubfn] Attempted to call uninitialized stub fn (" name ")") {:stub name :args args}))))))
  #?(:cljs (defn -assert-unstub-val [f] (if (fn?     f) f (throw (ex-info "[encore/stubfn] Unexpected unstub type (expected fn)"     {:unstub {:value f :type (type f)}}))))
     :clj  (defn -assert-unstub-val [s] (if (symbol? s) s (throw (ex-info "[encore/stubfn] Unexpected unstub type (expected symbol)" {:unstub {:value s :type (type s)}})))))
  #?(:clj
     (defmacro -intern-stub [ns stub-sym stub-var src]
       (-assert-unstub-val src)
       `(let [src-var# (var ~src)
              dst-var# ~stub-var
              dst-meta#
              (merge
                (dissoc      (meta dst-var#) :declared :redef)
                (select-keys (meta src-var#) [:arglists :doc]))]
          (intern '~ns (with-meta '~stub-sym dst-meta#)
            (.getRawRoot src-var#))))))

#?(:clj
   (defmacro defstub
     "Experimental. Declares a stub var that can be initialized from any
     namespace with `unstub-<stub-name>`. Decouples a var's declaration
     (location) and its initialization (value). Handy for defining vars in a
     shared ns from elsewhere (e.g. a private or cyclic ns)."
     [sym]
     (let [   stub-sym  sym
            unstub-sym (symbol (str  "unstub-" (name stub-sym)))
           -unstub-sym (symbol (str "-unstub-" (name stub-sym)))]
       `(if-cljs ; No declare/intern support
            (let [~'stubfn_ (-new-stubfn_ ~(name stub-sym))]
              (defn ~-unstub-sym [~'f]        (vreset! ~'stubfn_ (-assert-unstub-val ~'f)))
              (defn  ~unstub-sym [~'f]        (~-unstub-sym ~'f))
              (defn    ~stub-sym [~'& ~'args] (apply      @~'stubfn_ ~'args)))
          (let [stub-var# (declare ~(with-meta stub-sym {:redef true}))]
            (defmacro ~(with-meta unstub-sym {:doc "Initializes stub"})
              [~'x] ; ~'sym for clj, ~'f for cljs
              `(if-cljs
                 ;; In Cljs, a macro+fn can have the same name. Preference will be
                 ;; given to the macro in contexts where both are applicable.
                 ;; So there's 3 cases to consider:
                 ;;   1. clj   stub: def var, clj macro
                 ;;   2. cljs  stub: def volatile, 2 fns
                 ;;   3. clj/s stub: def volatile, 2 fns, var, and clj/s macro
                 (~'~(symbol (str *ns*) (str (name -unstub-sym))) ~~'x)
                 (-intern-stub ~'~(symbol (str *ns*)) ~'~stub-sym
                   ~stub-var# ~~'x))))))))


(comment
  (defn- -foo ^long [y] (* y y))
  (macroexpand-all '(defstub foo))
  (defstub foo)
  (unstub-foo -foo)
  (qb 1e6 (-foo 5) (foo 5)) ; [68.49 71.88]
  (meta (first (:arglists (meta #'foo))))

  (do
    #?(:cljs (def cljs-thing "cljs-thing")
       :clj  (def clj-thing  "clj-thing"))

    #?(:clj (defmacro cljs-macro [] `(if-cljs cljs-thing clj-thing)))

    #?(:clj  (cljs-macro)
       :cljs (enc-macros/cljs-macro))

    #?(:cljs (enc-macros/defstub stub-test)
       :clj             (defstub stub-test))

    #?(:cljs (enc-macros/unstub-stub-test identity)
       :clj             (unstub-stub-test identity))))

;;;; Str filter

(let [always (fn always [?in-str] true)
      never  (fn never  [?in-str] false)

      wild-str->?re-pattern
      (fn [s]
        (when (str-contains? s "*")
          (re-pattern
            (-> (str "^" s "$")
              (str/replace "." "\\.")
              (str/replace "*" "(.*)")))))

      compile
      (fn compile [spec cache?] ; Returns (fn match? [in-str])
        (cond
          (#{:any "*"    } spec) always
          (#{:none #{} []} spec) never
          (re-pattern?     spec) (fn [in-str] (re-find spec in-str))
          (string?         spec)
          (cond
            ;; Ambiguous: "," meant as splitter or literal? Prefer coll.
            ;; (str-contains? spec ",") (recur (mapv str/trim (str/split spec #",")) cache?)
            :if-let [re-pattern (wild-str->?re-pattern spec)]

            (recur re-pattern cache?)
            :else (fn [in-str] (= in-str spec)))

          (or (vector? spec) (set? spec))
          (cond
            ;; (empty? spec)   never
            ((set spec) "*")   always
            (= (count spec) 1) (recur (first spec) cache?)
            :else
            (let [[fixed-strs re-patterns]
                  (reduce
                    (fn [[fixed-strs re-patterns] spec]
                      (if-let [re-pattern (if (re-pattern? spec) spec (wild-str->?re-pattern spec))]
                        [      fixed-strs       (conj re-patterns re-pattern)]
                        [(conj fixed-strs spec)       re-patterns            ]))
                    [#{} []]
                    spec)

                  fx-match (not-empty fixed-strs) ; #{"foo" "bar"}, etc.
                  re-match
                  (when-let [re-patterns (not-empty re-patterns)] ; ["foo.*", "bar.*"], etc.
                    (let [f (fn [in-str] (rsome #(re-find % in-str) re-patterns))]
                      (if cache? (fmemoize f) f)))]

              (cond!
                (and fx-match re-match) (fn [in-str] (or (fx-match in-str) (re-match in-str)))
                fx-match fx-match
                re-match re-match)))

          :else
          (throw
            (ex-info "[enc/compile-str-filter] Unexpected spec type"
              {:spec {:value spec :type (type spec)}}))))]

  (defn compile-str-filter
    "Compiles given spec and returns a fast (fn conform? [?in-str]).

    Spec may be:
      - A regex pattern. Will conform on match.
      - A string, in which any \"*\"s will act as wildcards (#\".*\").
        Will conform on match.

      - A vector or set of regex patterns or strings.
        Will conform on ANY match.
        If you need literal \"*\"s, use an explicit regex pattern instead.

      - {:allow <allow-spec> :deny <deny-spec> :cache? <bool>}.
        Will conform iff allow-spec matches AND deny-spec does NOT.

    Input may be: namespace strings, class names, etc.
    Useful as string allowlist (whitelist) and/or denylist (blacklist).

    Spec examples:
      #{}, \"*\", \"foo.bar\", \"foo.bar.*\", #{\"foo\" \"bar.*\"},
      {:allow #{\"foo\" \"bar.*\"} :deny #{\"foo.*.bar.*\"}}"

    [spec]
    (if-not (map? spec)
      (recur {:allow spec :deny nil})
      (let [cache?         (get spec :cache?)
            allow-spec (or (get spec :allow) (get spec :whitelist))
            deny-spec  (or (get spec :deny)  (get spec :blacklist))

            allow (when-let [as allow-spec] (compile as cache?))
            deny  (when-let [ds deny-spec]  (compile ds cache?))]

        (cond
          (= deny  always) never
          (= allow never)  never

          (and allow deny)
          (fn [?in-str]
            (let [in-str (str ?in-str)]
              (if (allow in-str)
                (if (deny in-str)
                  false
                  true)
                false)))

          allow (if (= allow always) always (fn [?in-str] (if (allow (str ?in-str)) true  false)))
          deny  (if (= deny  never)  always (fn [?in-str] (if (deny  (str ?in-str)) false true)))
          :else
          (throw
            (ex-info "[encore/compile-str-filter] `allow-spec` and `deny-spec` cannot both be nil"
              {:allow-spec allow-spec :deny-spec deny-spec})))))))

(comment
  (def sf? (compile-str-filter #{"foo.*" "bar"}))
  (qb 1e5 (sf? "foo")) ; 26

  (-> "foo" ((compile-str-filter nil)))           :ex
  (-> "foo" ((compile-str-filter :any)))          true
  (-> "foo" ((compile-str-filter #{"foo*"})))     true
  (-> "foo" ((compile-str-filter ["bar" "foo"]))) true
  (-> "foo" ((compile-str-filter ["bar" "f*"])))  true
  (-> "foo" ((compile-str-filter {:allow :any :deny :any}))) false
  (-> "foo" ((compile-str-filter {:allow "foo*"}))) true
  (-> "foo" ((compile-str-filter {:deny  "foo*"}))) false
  (-> "foo" ((compile-str-filter {:allow "*" :deny "foo*"}))) false
  )

;;;; Namespaces

#?(:clj
   (defn interns-overview
     "Returns {:keys [public private impl test]}, with each key mapped to
     an alphabetical list of the relevant vars in given namespace.

     \"impl\" vars are public vars with names that begin with \"-\" or \"_\",
     a naming convention commonly used to indicate vars intended to be treated
     as private implementation details even when public."
     ([  ] (interns-overview *ns*))
     ([ns]
      (map-vals sort
        (reduce-kv
          (fn [{:keys [public private impl test] :as m} k v]
            (let [mt (meta v)]
              (cond
                (:test    mt) (update m :test    conj k)
                (:private mt) (update m :private conj k)

                :if-let [impl?
                         (let [[n1 n2] (name (:name mt))]
                           (or              (#{\- \_} n1)
                             (and (= n1 \*) (#{\- \_} n2))))]
                (update m :impl conj k)

                :else (update m :public conj k))))
          {}
          (ns-interns ns))))))

(comment (interns-overview))

;;;; Scheduling
;; Considered also adding `call-at-interval` but decided against it since the
;; API we'd want for that would be less interesting and more impl specific;
;; i.e. the cost/benefit would be poor.

(do
  (defprotocol   ITimeoutImpl (-schedule-timeout [_ msecs f]))
  (deftype DefaultTimeoutImpl [#?(:clj ^java.util.Timer timer)]
                 ITimeoutImpl
    (-schedule-timeout [_ msecs f]
      #?(:cljs (.setTimeout js/window f msecs)
         :clj
         (let [tt (proxy [java.util.TimerTask] []
                    (run [] (catching (f))))]
           (.schedule timer tt (long msecs))))))

  (defonce default-timeout-impl_
    "Simple one-timeout timeout implementation provided by platform timer.
    O(logn) add, O(1) cancel, O(1) tick. Fns must be non-blocking or cheap.
    Similar efficiency to core.async timers (binary heap vs DelayQueue)."
    (delay
      (DefaultTimeoutImpl.
        #?(:clj (java.util.Timer. "encore/timer" true)))))

  (def ^:private -tout-pending   (new-object))
  (def ^:private -tout-cancelled (new-object))
  (defn- tout-result [result_]
    (if (kw-identical? result_ -tout-pending)
      :timeout/pending
      (if (kw-identical? result_ -tout-cancelled)
        :timeout/cancelled
        @result_))))

(defprotocol ITimeoutFuture
  (tf-state      [_] "Returns a map of timeout's public state.")
  (tf-poll       [_] "Returns :timeout/pending, :timeout/cancelled, or the timeout's completed result.")
  (tf-done?      [_] "Returns true iff the timeout is not pending (i.e. has a completed result or is cancelled).")
  (tf-pending?   [_] "Returns true iff the timeout is pending.")
  (tf-cancelled? [_] "Returns true iff the timeout is cancelled.")
  (tf-cancel!    [_] "Returns true iff the timeout was successfully cancelled (i.e. was previously pending)."))

#?(:cljs
   (deftype TimeoutFuture [f result__ udt]
     ITimeoutFuture
     (tf-state      [_] {:fn f :udt udt})
     (tf-poll       [_] (tout-result @result__))
     (tf-done?      [_] (not (kw-identical? @result__ -tout-pending)))
     (tf-pending?   [_]      (kw-identical? @result__ -tout-pending))
     (tf-cancelled? [_]      (kw-identical? @result__ -tout-cancelled))
     (tf-cancel!    [_] (compare-and-set! result__ -tout-pending -tout-cancelled))

     IPending (-realized?  [t] (tf-done? t))
     IDeref   (-deref      [t] (tf-poll  t))))

#?(:clj
   (deftype TimeoutFuture
     [f result__ ^long udt ^java.util.concurrent.CountDownLatch latch]
     ITimeoutFuture
     (tf-state      [_] {:fn f :udt udt})
     (tf-poll       [_] (tout-result @result__))
     (tf-done?      [_] (not (kw-identical? @result__ -tout-pending)))
     (tf-pending?   [_]      (kw-identical? @result__ -tout-pending))
     (tf-cancelled? [_]      (kw-identical? @result__ -tout-cancelled))
     (tf-cancel!    [_]
       (if (compare-and-set! result__ -tout-pending -tout-cancelled)
         (do (.countDown latch) true)
         false))

     clojure.lang.IPending (isRealized  [t] (tf-done? t))
     clojure.lang.IDeref   (deref       [_] (.await latch) (tout-result @result__))
     clojure.lang.IBlockingDeref
     (deref [_ timeout-ms timeout-val]
       (if (.await latch timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
         (tout-result @result__)
         timeout-val))

     java.util.concurrent.Future
     (isCancelled [t]   (tf-cancelled? t))
     (isDone      [t]   (tf-done?      t))
     (cancel      [t _] (tf-cancel!    t))))

#?(:clj  (defn          timeout-future? [x] (instance? TimeoutFuture x))
   :cljs (defn ^boolean timeout-future? [x] (instance? TimeoutFuture x)))

(defn call-after-timeout
  "Alpha, subject to change.
  Returns a TimeoutFuture that will execute `f` after given msecs.

  Does NOT do any automatic binding conveyance.

  Performance depends on the provided timer implementation (`impl_`).
  The default implementation offers O(logn) add, O(1) cancel, O(1) tick.

  See `ITimeoutImpl` for extending to arbitrary timer implementations."

  ;; Why no auto binding convyance? Explicit manual conveyance plays better
  ;; with cljs, and means less surprise with `future-fn`.
  ([      msecs f] (call-after-timeout default-timeout-impl_ msecs f))
  ([impl_ msecs f]
   (let [msecs (long msecs)
         udt   (+ (now-udt*) msecs) ; Approx instant to run
         result__ (atom -tout-pending)
         #?(:clj latch) #?(:clj (java.util.concurrent.CountDownLatch. 1))
         cas-f
         (fn []
           (let [result_ (delay (f))]
             (when (compare-and-set! result__ -tout-pending result_)
               @result_
               #?(:clj (.countDown latch)))))]

     (let [impl (force impl_)]
       (-schedule-timeout impl msecs cas-f))

     (TimeoutFuture. f result__ udt #?(:clj latch)))))

#?(:clj
   (defmacro after-timeout
     "Alpha, subject to change.
     Returns a TimeoutFuture that will execute body after timeout.
     Body must be non-blocking or cheap."
     [msecs & body] `(call-after-timeout ~msecs (fn [] ~@body))))

(comment
  @(after-timeout 500 (println "foo") "bar")
  (def ^:dynamic *foo* nil)
  (binding [*foo* "bar"] ; Note no auto conveyance
    ((:fn (tf-state (after-timeout 200 (println *foo*) *foo*))))))

;;;; DEPRECATED
;; {:deprecated "v<x.y.z> (<yyyy-mm-dd>)" :doc "Prefer `<replacement>`"}

#?(:clj
   (defmacro deprecated
     "Elides body when `taoensso.elide-deprecated` JVM property or
     `TAOENSSO_ELIDE_DEPRECATED` environment variable is ∈ #{\"true\" \"TRUE\"}."
     [& body]
     (if (get-sys-bool false
           "taoensso.elide-deprecated"
           "TAOENSSO_ELIDE_DEPRECATED")
       nil ; Elide
       `(do ~@body))))

(deprecated
  #?(:cljs (def regular-num?        finite-num?))
  #?(:cljs (def get-window-location get-win-loc))
  #?(:clj  (def srng                secure-rng))
  (def backport-run!   run!)
  (def fq-name         as-qname)
  (def qname           as-qname)
  (def merge-deep-with nested-merge-with)
  (def merge-deep      nested-merge)
  (def parse-bool      as-?bool)
  (def parse-int       as-?int)
  (def parse-float     as-?float)
  (def swapped*        swapped)
  (def memoize-a0_     memoize)
  (def memoize-a1_     memoize)
  (def a0-memoize_     memoize)
  (def a1-memoize_     memoize)
  (def memoize-1       memoize-last)
  (def memoize1        memoize-last)
  (def memoize*        memoize)
  (def memoize_        memoize)
  (def nnil?           some?)
  (def nneg-num?       nat-num?)
  (def nneg-int?       nat-int?)
  (def nneg-float?     nat-float?)
  (def uint?           nat-int?)
  (def pint?           pos-int?)
  (def nnil=           some=)
  (def as-?uint        as-?nat-int)
  (def as-?pint        as-?pos-int)
  (def as-?ufloat      as-?nat-float)
  (def as-?pfloat      as-?pos-float)
  (def as-uint         as-nat-int)
  (def as-pint         as-pos-int)
  (def as-ufloat       as-nat-float)
  (def as-pfloat       as-pos-float)
  (def run!*           run!)
  (def ?subvec<idx     (comp not-empty get-subvec))
  (def ?subvec<len     (comp not-empty get-subvector))
  (def nano-time       now-nano)
  (def -swap-cache!    -swap-val!)
  (def -unswapped      swapped-vec)
  (def -vswapped       swapped-vec)
  (def -swap-k!        -swap-val!)
  (def update-in*      update-in)
  (def idx-fn          counter)
  (def vec*            ensure-vec)
  (def set*            ensure-set)
  (def have-transducers? true)

  (def     pval?    pnum?)
  (def as-?pval as-?pnum)
  (def  as-pval  as-pnum)

  (defn get-substr
    {:deprecated "v3.26.0 (2022-10-14)"
     :doc "Prefer `get-substr-by-idx`"}
    ([s ^long start]
     #?(:cljs (.substring s start)
        :clj
        (let [start (if (< start 0) 0 start)
              slen  #?(:clj (.length ^String s) :cljs (.-length s))]
          (if (>= start slen)
            ""
            (.substring ^String s start slen)))))

    ([s ^long start ^long end]
     #?(:cljs (if (>= start end) "" (.substring s start end))
        :clj
        (let [start (if (< start 0) 0 start)
              slen  #?(:clj (long (.length ^String s)) :cljs (.-length s))
              end   (if (> end slen) slen end)]
          (if (>= start end)
            ""
            (.substring ^String s start end))))))

  (defn get-substring
    {:deprecated "v3.26.0 (2022-10-14)"
     :doc "Prefer `get-substr-by-len`"}
    ([s ^long start]
     #?(:cljs (as-?nempty-str (.substr s start))
        :clj
        (let [slen (.length ^String s)]
          (if (< start 0)
            (let [start (+ start slen)
                  start (if (< start 0) 0 start)]
              (.substring ^String s start) slen)
            (if (>= start slen)
              nil
              (.substring ^String s start slen))))))

    ([s ^long start ^long length]
     #?(:cljs (as-?nempty-str (.substr s start length))
        :clj
        (if (<= length 0)
          nil
          (let [slen (long (.length ^String s))]
            (if (< start 0)
              (let [start (+ start slen)
                    start (if (< start 0) 0 start)
                    end   (+ start length)
                    end   (if (> end slen) slen end)]
                (.substring ^String s start end))

              (let [end (+ start length)
                    end (if (> end slen) slen end)]
                (if (>= start end)
                  nil
                  (.substring ^String s start end)))))))))

  (def ?substr<idx (comp as-?nempty-str get-substr))
  (def ?substr<len (comp as-?nempty-str get-substring))

  ;; Used by old versions of Timbre, Tufte
  (let [nolist? #(contains? #{nil [] #{}} %)]

    (defn compile-ns-filter
      "Deprecated, prefer `compile-str-filter` instead."
      ([ns-pattern         ] (compile-ns-filter ns-pattern nil))
      ([whitelist blacklist]

       (if (and (nolist? whitelist) (nolist? blacklist))
         (fn [_] true) ; Unfortunate API choice
         (compile-str-filter {:allow whitelist :deny blacklist})))))

  #?(:clj (defn set-body      [rresp body]    (ring-set-body      body    rresp)))
  #?(:clj (defn set-status    [rresp code]    (ring-set-status    code    rresp)))
  #?(:clj (defn merge-headers [rresp headers] (ring-merge-headers headers rresp)))
  #?(:clj (def  redirect-resp ring-redirect-resp))

  #?(:clj
     (do
       (defmacro if-lets       [& args]  `(taoensso.encore/if-let        ~@args))
       (defmacro when-lets     [& args]  `(taoensso.encore/when-let      ~@args))
       (defmacro if-not*       [& args]  `(taoensso.encore/if-not        ~@args))
       (defmacro cond*         [& args]  `(taoensso.encore/cond          ~@args))
       (defmacro defonce*      [& args]  `(taoensso.encore/defonce       ~@args))
       (defmacro have-in       [a1 & an] `(taoensso.encore/have  ~a1 :in ~@an))
       (defmacro have-in!      [a1 & an] `(taoensso.encore/have! ~a1 :in ~@an))
       (defmacro cond-throw    [& args]  `(taoensso.encore/cond!         ~@args))
       (defmacro catch-errors* [& args]  `(taoensso.encore/catching      ~@args))
       (defmacro use-fixtures* [& args]  `(taoensso.encore/use-fixtures  ~@args))
       (defmacro nano-time*    [& args]  `(taoensso.encore/now-nano*     ~@args))
       (defmacro qbench        [& args]  `(taoensso.encore/quick-bench   ~@args))
       (defmacro catch-errors  [& body]
         `(catching [(do ~@body) nil] e# [nil e#]))

       (defmacro -vol!       [val]           `(volatile!     ~val))
       (defmacro -vol-reset! [vol_ val]      `(vreset! ~vol_ ~val))
       (defmacro -vol-swap!  [vol_ f & args] `(vswap!  ~vol_ ~f ~@args))

       (defmacro thrown
         {:deprecated "v3.31.0 (2022-10-27)"
          :doc "Prefer `throws`"}
         [& args] `(throws ~@args))))

  ;;; Prefer `str-join` when possible (needs Clojure 1.7+)
  #?(:cljs (defn undefined->nil [x] (if (undefined? x) nil x)))
  (defn spaced-str-with-nils [xs] (str/join " " (mapv nil->str xs)))
  (defn spaced-str [xs] (str/join " " #?(:clj xs :cljs (mapv undefined->nil xs))))

  ;; Arg order changed for easier partials, etc.:
  (defn round [n & [type nplaces]] (round* (or type :round) nplaces n))
  (defn approx=
    ([x y      ] (approx==       x y))
    ([x y signf] (approx== signf x y)))

  ;; & coll changed to coll:
  (defn join-once [sep & coll] (str-join-once sep coll))

  #?(:clj
     (do ;; Used by Carmine <= v2.7.0
       (defmacro repeatedly* [n & body] `(repeatedly-into* [] ~n ~@body))
       (defmacro repeatedly-into* "Deprecated" ; Used by Nippy < v2.10
         [coll n & body] `(repeatedly-into ~coll ~n (fn [] ~@body)))))
  
  (defn nnil-set [x] (disj (ensure-set x) nil))

  ;;; Arg order changed for easier partials
  (defn keys=      [m ks] (ks=      ks m))
  (defn keys<=     [m ks] (ks<=     ks m))
  (defn keys>=     [m ks] (ks>=     ks m))
  (defn keys=nnil? [m ks] (ks-nnil? ks m))

  (defn rate-limiter* "Deprecated, prefer `limiter`" [specs]
    (let [ids? (rsome (fn [[_ _ id]] id) specs)
          lfn  (limiter specs)]
      (fn [& args]
        (when-let [[worst-sid backoff-ms] (apply lfn args)]
          (if ids?
            [backoff-ms worst-sid]
             backoff-ms)))))

  (defn rate-limit [specs f]
    (let [rl (rate-limiter* specs)]
      (fn [& args]
        (if-let [backoff (rl)]
          [nil backoff]
          [(f) nil]))))

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

  #?(:cljs ; Used by Sente <= v1.1.0
     (defn set-exp-backoff-timeout! [nullary-f & [nattempt]]
       (when-let [js-win js-?win]
         (.setTimeout js-win nullary-f (exp-backoff (or nattempt 0))))))

  #?(:cljs
     (do ; Level-based Cljs logging (prefer Timbre v4+)
       (defonce ^:dynamic *log-level* "DEPRECATED" :debug)
       (def ^:private log?
         (let [->n {:trace 1 :debug 2 :info 3 :warn 4 :error 5 :fatal 6 :report 7}]
           (fn [level] (>= (->n level) (->n *log-level*)))))

       (defn tracef  [fmt & xs] (when (log? :trace)  (apply logf fmt xs)))
       (defn debugf  [fmt & xs] (when (log? :debug)  (apply logf fmt xs)))
       (defn infof   [fmt & xs] (when (log? :info)   (apply logf fmt xs)))
       (defn warnf   [fmt & xs] (when (log? :warn)   (apply logf (str "WARN: "  fmt) xs)))
       (defn errorf  [fmt & xs] (when (log? :error)  (apply logf (str "ERROR: " fmt) xs)))
       (defn fatalf  [fmt & xs] (when (log? :fatal)  (apply logf (str "FATAL: " fmt) xs)))
       (defn reportf [fmt & xs] (when (log? :report) (apply logf fmt xs)))))

  (defn greatest [coll & [?comparator]]
    (let [comparator (or ?comparator rcompare)]
      (reduce #(if (pos? (comparator %1 %2)) %2 %1) coll)))

  (defn least [coll & [?comparator]]
    (let [comparator (or ?comparator rcompare)]
      (reduce #(if (neg? (comparator %1 %2)) %2 %1) coll)))

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

  (defn memoized [cache f & args]
    (if-not cache ; {<args> <delay-val>}
      (apply f args)
      @(-swap-val! cache args (fn [?dv] (if ?dv ?dv (delay (apply f args)))))))

  (defn- translate-signed-idx [^long signed-idx ^long max-idx]
    (if (>= signed-idx 0)
      (min      signed-idx max-idx)
      (max 0 (+ signed-idx max-idx))))

  (comment (translate-signed-idx -3 5))

  (defn sub-indexes [x start-idx & {:keys [^long max-len ^long end-idx]}]
    (let [start-idx  ^long start-idx
          xlen       (count x) ; also = max-exclusive-end-idx
          ^long start-idx* (translate-signed-idx start-idx xlen)
          end-idx*   (long
                       (cond
                         max-len (min* (+ start-idx* max-len) xlen)
                         end-idx (inc ; Want exclusive
                                   ^long (translate-signed-idx end-idx xlen))
                         :else   xlen))]
      (if (> start-idx* end-idx*)
        ;; [end-idx* start-idx*] ; Allow wrapping
        [0 0] ; Disallow wrapping
        [start-idx* end-idx*])))

  (defn substr "Deprecated, prefer `get-substr-by-idx` or `get-substr-by-len`"
    [s start-idx & [?max-len]]
    (let [[start-idx* end-idx*] (sub-indexes s start-idx :max-len ?max-len)]
      #?(:clj  (.substring ^String s start-idx* end-idx*)
         :cljs (.substring         s start-idx* end-idx*))))

  (comment (substr "hello" -1 1))

  (defn subvec* "Deprecated, prefer `get-subvec` or `get-subvector`"
    [v start-idx & [?max-len]]
    (let [[start-idx* end-idx*] (sub-indexes v start-idx :max-len ?max-len)]
      (subvec v start-idx* end-idx*)))

  (def  sentinel (new-object))
  (defn sentinel?     [x] (identical? x sentinel))
  (defn nil->sentinel [x] (if (nil? x) sentinel x))
  (defn sentinel->nil [x] (if (sentinel? x) nil x))

  (defn   singleton? [coll] (if (counted? coll) (= (count coll) 1) (not (next coll))))
  (defn ->?singleton [coll] (when (singleton? coll) (let [[c1] coll] c1)))
  (defn ->vec [x] (cond (vector? x) x (sequential? x) (vec x) :else [x]))

  (defn fzipmap [ks vs]
    (loop [m  (transient {})
           ks (seq ks)
           vs (seq vs)]
      (if-not (and ks vs)
        (persistent! m)
        (recur (assoc! m (first ks) (first vs))
          (next ks)
          (next vs)))))

  (defn filter-kvs [pred m] (if (nil? m) {} (reduce-kv (fn [m k v] (if (pred k v)         m    (dissoc m k))) m m)))
  (defn remove-kvs [pred m] (if (nil? m) {} (reduce-kv (fn [m k v] (if (pred k v) (dissoc m k)         m))    m m)))

  (defn revery     [pred coll] (reduce    (fn [acc in]  (if (pred in)  coll (reduced nil))) coll coll))
  (defn revery-kv  [pred coll] (reduce-kv (fn [acc k v] (if (pred k v) coll (reduced nil))) coll coll))

  (def every revery)

  (defn replace-in [m & ops]
    (reduce
      (fn [m ?op]
        (if-not ?op
          m ; Support conditional ops
          (let [[type ks valf] ?op
                f (if (kw-identical? type :reset) (fn [_] valf) valf)]
            (update-in m ks nil f))))
      m
      ops))

  (let [return (fn [m0 v0 m1 v1] [v0 v1])]
    (defn swap-in!*
      "Deprecated, prefer `swap-in!` with `swapped` return value."
      ([atom_              f] (-swap-k0! return atom_              f))
      ([atom_ ks           f] (-swap-kn! return atom_ ks nil       f))
      ([atom_ ks not-found f] (-swap-kn! return atom_ ks not-found f)))

    (defn swap-val!*
      "Deprecated, prefer `swap-val!` with `swapped` return value."
      ([atom_ k           f] (-swap-k1! return atom_ k nil       f))
      ([atom_ k not-found f] (-swap-k1! return atom_ k not-found f))))

  (def dswap! swap-in!*)
  (def swap!* swap-in!*)

  #?(:clj (defalias taoensso.truss/get-dynamic-assertion-data))
  #?(:clj (defalias taoensso.truss/with-dynamic-assertion-data)))
