(ns taoensso.encore
  "Extended core library for Clojure/Script that emphasizes:
    * Cross-platform API
    * Flexibility
    * Performance
    * Backwards compatibility

  This lib's mostly for my own use and for advanced users that feel
  comfortable reading this source. Not providing much beginner-oriented
  documentation for this, sorry.

  Common naming conventions used across my libs:
    **foo** - Dynamic var
    foo!    - Fn with side-effects, or that should otherwise be used cautiously
    foo?    - Truthy val or fn that returns truthy val
    foo!?   - Fn that has side-effects (or requires caution) and that return
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
    [wip]   - Work-in-progress (still under development)

    [mod]   - Modify     behaviour (=>          breaking), [mod!], [mod!!], etc. for attention
    [fix]   - Fix broken behaviour (=> usu. non-breaking)
    [new]   - Add new    behaviour (=>      non-breaking)

    [doc]   - Documentation changes besides those better labelled as [mod], [fix], or [new]
    [nop]   - Other non-breaking changes (to implementation details, non-code changes, etc.)

    [x] [y] - Single commit with multiple tags (in priority order), try avoid

  Example commit messages:
    v1.0.0 (2022-01-27) ; Tagged release
    [new] [#122] Add new feature x (@contributor)

  Version numbers:
    Ver tables:  X.Y.Z (without backticks)
       Min ver: vX.Y.Z+
     Elsewhere: vX.Y.Z"

  {:author "Peter Taoussanis (@ptaoussanis)"}

  (:refer-clojure :exclude
   [if-let if-some if-not when when-not when-some when-let cond defonce
    run! some? ident? float? boolean? uri? indexed? bytes?
    int? pos-int? neg-int? nat-int? inst?
    simple-ident?   qualified-ident?
    simple-symbol?  qualified-symbol?
    simple-keyword? qualified-keyword?
    format update-in merge merge-with
    memoize abs ex-message ex-cause
    newline satisfies? uuid])

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
      [java.util.concurrent.atomic AtomicReference]
      [java.util.function UnaryOperator]
      ;; [org.apache.commons.codec.binary Base64]
      )

     :cljs
     (:require-macros
      [taoensso.encore :as enc-macros :refer
       [have have! have? compile-if try-eval
        if-let if-some if-not when when-not when-some when-let -cond cond
        def* defonce cond! try* catching -cas!? now-udt* now-nano* min* max*
        name-with-attrs deprecated new-object defalias throws throws?
        identical-kw? satisfies? satisfies! instance!]])))

(def encore-version [3 85 0])

(comment "∴ ∵ ℕ ℤ ℝ ∞ ≠ ∈ ∉ ⇒⇔ → × ⊃⊂ ⊇⊆ ≡ ¬ ∀ ∃ ∝"
  (set! *unchecked-math* :warn-on-boxed)
  (set! *unchecked-math* false))

#?(:clj
   (do ; Bootstrap Truss aliases
     (defmacro ^:private -have  [& args] `(taoensso.truss/have  ~@args))
     (defmacro ^:private -have? [& args] `(taoensso.truss/have? ~@args))))

(comment
  (remove-ns 'taoensso.encore)
  (:api (interns-overview))
  (test/run-tests))

;;;;

(defn unexpected-arg!
  "Throws runtime `ExceptionInfo` to indicate an unexpected argument.
  Takes optional kvs for merging into exception's data map.

    (let [mode :unexpected]
      (case mode
        :read  (do <...>)
        :write (do <...>)
        (unexpected-arg! mode
          :context  `my-function
          :param    'mode
          :expected #{:read :write}))) =>

    Unexpected argument: :unexpected
    {:arg {:value :unexpected, :type clojure.lang.Keyword},
     :context 'taoensso.encore/my-function
     :param 'mode
     :expected #{:read :write}}"

  {:added "Encore v3.51.0 (2023-03-13)"
   :arglists
   '([arg]
     [arg   {:keys [msg context param expected ...]}]
     [arg & {:keys [msg context param expected ...]}])}

  ([arg     ] (unexpected-arg! arg nil))
  ([arg opts]
   (throw
     (ex-info (or (get opts :msg) (str "Unexpected argument: " arg))
       (conj {:arg {:value arg, :type (type arg)}} (dissoc opts :msg)))))

  ([arg k1 v1                  ] (unexpected-arg! arg {k1 v1}))
  ([arg k1 v1 k2 v2            ] (unexpected-arg! arg {k1 v1, k2 v2}))
  ([arg k1 v1 k2 v2 k3 v3      ] (unexpected-arg! arg {k1 v1, k2 v2, k3 v3}))
  ([arg k1 v1 k2 v2 k3 v3 k4 v4] (unexpected-arg! arg {k1 v1, k2 v2, k3 v3, k4 v4})))

(comment (unexpected-arg! :arg :expected '#{string?}))

;;;; Core macros

#?(:clj
   (defmacro compile-if
     "Evaluates `test`. If it returns logical true (and doesn't throw), expands
     to `then`, otherwise expands to `else`."
     {:style/indent 1}
     ([test then     ] `(compile-if ~test ~then nil))
     ([test then else]
      (if (try (eval test) (catch Throwable _ false))
        `(do ~then)
        `(do ~else)))))

#?(:clj (defmacro compile-when {:style/indent 1} [test & body] `(compile-if ~test (do ~@body) nil)))

#?(:clj
   (defmacro try-eval
     "If `form` can be successfully evaluated at macro-expansion time, expands to `form`.
     Otherwise expands to `nil`."
     {:added "Encore v3.50.0 (2023-03-07)"}
     [form] `(compile-if ~form ~form nil)))

(comment (macroexpand '(try-eval (com.google.common.io.BaseEncoding/base16))))

#?(:clj
   (defmacro if-let
     "Supersets `core/if-let` functionality. Like `core/if-let` but supports multiple
     bindings, and unconditional bindings with `:let`:

     (if-let [x       (rand-nth [:x1 :x2 false nil    ])  ; Bind truthy x, or -> `else`
              :let [y (rand-nth [:y1 :y2 false nil x  ])] ; Bind any    y
              z       (rand-nth [:z1 :z2 false nil x y])  ; Bind truthy z, or -> `else`
              ]
       [:then-clause x y z]
       [:else-clause])"
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
     "Supersets `core/if-some` functionality. Like `core/if-some` but supports multiple
     bindings, and unconditional bindings with `:let`:

     (if-some [x       (rand-nth [:x1 :x2 false nil    ])  ; Bind non-nil x, or -> `else`
               :let [y (rand-nth [:y1 :y2 false nil x  ])] ; Bind any     y
               z       (rand-nth [:z1 :z2 false nil x y])  ; Bind non-nil z, or -> `else`
               ]
       [:then-clause x y z]
       [:else-clause])"
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
     "Supersets `core/if-not` functionality.
     Same as `encore/if-let` with `then` `and `else` forms swapped."
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
   (defmacro when-let
     "Supersets `core/when-let` functionality. Like `core/when-let` but supports multiple
     bindings, and unconditional bindings with `:let`:

     (when-let [x       (rand-nth [:x1 :x2 false nil    ])  ; Bind truthy x, or -> nil
                :let [y (rand-nth [:y1 :y2 false nil x  ])] ; Bind any    y
                z       (rand-nth [:z1 :z2 false nil x y])  ; Bind truthy z, or -> nil
                ]
       [:body x y z])"
     ;; Now a feature subset of all-case `when`
     {:style/indent 1}
     [bindings & body] `(if-let ~bindings (do ~@body))))

#?(:clj
   (defmacro when
     "Supersets `core/when` and `core/when-let` functionality. When `test-or-bindings` is
     a vector, same as `encore/when-let`. Otherwise same as `core/when`."
     {:style/indent 1}
     [test-or-bindings & body]
     (if (vector? test-or-bindings)
       `(if-let ~test-or-bindings (do ~@body) nil)
       `(if     ~test-or-bindings (do ~@body) nil))))

#?(:clj
   (defmacro when-not
     "Supersets `core/when-not` functionality.
     Same as `encore/if-let` with `body` as `else` form."
     {:style/indent 1}
     [test-or-bindings & body]
     (if (vector? test-or-bindings)
       `(if-let ~test-or-bindings nil (do ~@body))
       `(if     ~test-or-bindings nil (do ~@body)))))

#?(:clj
   (defmacro when-some
     "Supersets `core/when-some` functionality. Like `core/when-some` but supports multiple
     bindings, and unconditional bindings with `:let`:

     (when-some [x       (rand-nth [:x1 :x2 false nil    ])  ; Bind non-nil x, or -> `else`
                 :let [y (rand-nth [:y1 :y2 false nil x  ])] ; Bind any     y
                 z       (rand-nth [:z1 :z2 false nil x y])  ; Bind non-nil z, or -> `else`
                 ]
       [:body x y z])"
     {:style/indent 1}
     [test-or-bindings & body]
     (if (vector? test-or-bindings)
       `(if-some       ~test-or-bindings  (do ~@body) nil)
       `(if      (nil? ~test-or-bindings) nil (do ~@body)))))

(comment
  (if-let   [a :a b (= a :a)] [a b] "else")
  (if-let   [a :a b (= a :b)] [a b] "else")
  (if-some  [a :a b (= a :b)] [a b] "else")
  (when-let [a :a b nil] "true")
  (when-let [:let [a :a b :b] c (str a b)] c))

;; (:ns &env) is nnil iff compiling for Cljs. This gives macros a way to produce
;; different code depending on target (Clj/s), something reader conditionals cannot do.
#?(:clj (defmacro if-clj  {:style/indent 1} [then & [else]] (if (:ns &env) else then)))
#?(:clj (defmacro if-cljs {:style/indent 1} [then & [else]] (if (:ns &env) then else)))
#?(:clj
   (defn compiling-cljs?
     "Return truthy iff currently generating Cljs code.
     See also `if-cljs`, `if-clj`."
     []
     (when-let [ns (find-ns 'cljs.analyzer)]
       (when-let [v (ns-resolve ns '*cljs-file*)]
         (boolean @v)))))

(comment (compiling-cljs?))

#?(:clj
   (defmacro ^:no-doc -cond [throw? & clauses]
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
     "Supersets `core/cond` functionality. Like `core/cond` but supports implicit
     final `else` clause, and special clause keywords for advanced behaviour:

     (cond
       :let     [x   \"x\"] ; Establish let     binding/s for remaining forms
       :binding [*x* \"x\"] ; Establish dynamic binding/s for remaining forms
       :do      (println (str \"x value: \" x)) ; Eval expr for side-effects

       :if-let [y \"y\"
                z nil]
       \"y and z were both truthy\"

       :if-some [y \"y\"
                 z nil]
       \"y and z were both non-nil\")

     `:let` support inspired by <https://github.com/Engelberg/better-cond>.
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

#?(:cljs
   (defn ^boolean some?
     "Same as `core/some?` (added in Clojure v1.6)."
     [x] (if (nil? x) false true))

   :clj
   (defn some?
     "Same as `core/some?` (added in Clojure v1.6)."
     {:inline (fn [x] `(if (identical? ~x nil) false true))}
     [x]               (if (identical?  x nil) false true)))

(defmacro or-some
  "Like `or`, but returns the first non-nil form (may be falsey)."
  {:added "Encore v3.67.0 (2023-09-08)"}
  ([        ] nil)
  ([x       ] x)
  ([x & next] `(let [x# ~x] (if (identical? x# nil) (or-some ~@next) x#))))

(comment (or-some nil nil false true))

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
   (defmacro def*
     "Like `core/def` but supports attrs map."
     {:style/indent 1
      :added "Encore v3.67.0 (2023-09-08)"}
     [sym & args]
     (let [[sym body] (name-with-attrs sym args)]
       `(def ~sym ~@body))))

#?(:clj
   (defmacro defonce
     "Like `core/defonce` but supports docstring and attrs map."
     {:style/indent 1}
     [sym & args]
     (let [[sym body] (name-with-attrs sym args)]
       (if (:ns &env)
         `(cljs.core/defonce    ~sym ~@body)
         `(clojure.core/defonce ~sym ~@body)))))

#?(:clj
   (defmacro identical-kw?
     "Returns true iff two keywords are identical.
     Portable and maximally fast.
       For Clj  this expands to: `(identical?         x y)`
       For Cljs this expands to: `(keyword-identical? x y)`"
     {:added "Encore v3.67.0 (2023-09-08)"}
     [x y]
     (if (:ns &env)
       `(cljs.core/keyword-identical? ~x ~y)
       `(identical?                   ~x ~y))))

#?(:clj
   (defmacro case-eval
     "Like `case` but test expressions are evaluated for their compile-time value."
     {:style/indent 1}
     [expr & clauses]
     (let [default (when (odd? (count clauses)) (last clauses))
           clauses (if default (butlast clauses) clauses)]
       `(case ~expr
          ~@(map-indexed (fn [i# form#] (if (even? i#) (eval form#) form#)) clauses)
          ~(when default default)))))

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

;;;;

(def ^:private core-merge     #?(:clj clojure.core/merge     :cljs cljs.core/merge))
(def ^:private core-update-in #?(:clj clojure.core/update-in :cljs cljs.core/update-in))
(declare merge update-in)

;;;;

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

#?(:clj (def ^:private alias-src-attrs [:doc :arglists :private :macro :added :deprecated :inline]))
#?(:clj
   (defn ^:no-doc -alias-link-var [dst-var src-var dst-attrs]
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
     "Defines a local alias for the var identified by given qualified
     source symbol: (defalias my-map clojure.core/map), etc.

     For Clj:
       - Source var's metadata will be preserved (docstring, arglists, etc.).
       - Changes to source var's value will also be applied to alias."

     ([          src-sym                       ] `(defalias nil        ~src-sym nil          nil))
     ([alias-sym src-sym                       ] `(defalias ~alias-sym ~src-sym nil          nil))
     ([alias-sym src-sym alias-attrs           ] `(defalias ~alias-sym ~src-sym ~alias-attrs nil))
     ([alias-sym src-sym alias-attrs alias-body]
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
            `(def ~alias-sym ~(or alias-body src-sym)))

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
                alias-sym   (with-meta  alias-sym final-attrs)
                alias-body  (or alias-body `@~src-var)]

            `(do
               ;; Need `alter-meta!` to reliably retain ?macro status, Ref. Timbre #364
               (alter-meta!                  (def ~alias-sym ~alias-body) conj ~final-attrs)
               (when ~link? (-alias-link-var (var ~alias-sym) ~src-var         ~alias-attrs))
               (do                           (var ~alias-sym)))))))))

#?(:clj
   (defmacro defaliases
     "Bulk version of `defalias`.
     Takes source symbols or {:keys [alias src attrs body]} maps:
       (defaliases
         {:alias my-map, :src map, :attrs {:doc \"My `map` alias\"}}
         {:alias my-vec, :src vec, :attrs {:doc \"My `vec` alias\"}})"

     {:added "Encore v3.58.0 (2023-04-09)"
      :arglists '([{:keys [alias src attrs body]} ...])}

     [& clauses]
     `(do
        ~@(map
            (fn [x]
              (cond
                (symbol? x) `(defalias ~x)
                (map?    x)
                (let [{:keys [alias src attrs body]
                       :or   {attrs (dissoc x :alias :src)}} x]
                  `(defalias ~alias ~src ~attrs ~body))

                :else
                (unexpected-arg! x
                  {:context  `defaliases
                   :param    'alias-clause
                   :expected '#{symbol map}})))

            clauses))))

(comment
  (defn src "src doc 1" [] "val1")
  (defalias ^{:doc "alias doc 1"} src* src {:doc "alias doc 2"})
  [(src*) (meta #'src*)]
  (macroexpand '(defaliases {:alias map2 :src map :doc "map2"}))
  (do           (defaliases {:alias map2 :src map :doc "map2"}))

  (defn myfn "myfn doc" [x y] (Thread/sleep 2000) (+ x y))
  (defaliases {:alias myfn2 :src myfn :body (fmemoize myfn)})
  (myfn2 1 1))

#?(:clj
   (defmacro ^:no-doc deftype-print-methods
     "Private, don't use."
     {:added "Encore v3.57.0 (2023-03-29)"}
     [& types]
     `(do
        ~@(map
            (fn [type]
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

(declare map-keys)

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
             (if-not (identical-kw? readers ::dynamic)
               readers
               #?(:clj  clojure.core/*data-readers*

                  ;; Unfortunate (slow), but faster than gc'd memoization in most cases:
                  :cljs (map-keys symbol @cljs.reader/*tag-table*)))

             default
             (if-not (identical-kw? default ::dynamic)
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

;;;; Forms
;; Useful for macros, etc.

(declare rsome revery?)

#?(:clj
   (do
     (defn ^:no-doc call-form?
       "Private, don't use.
       Returns true if given a list or Cons."
       {:added "Encore v3.75.0 (2024-01-29)"}
       [x]
       (or (list? x) (instance? #?(:clj clojure.lang.Cons #_:cljs #_cljs.core/Cons) x)))

     (defn ^:no-doc call-in-form?
       "Private, don't use.
       Returns true if given a call form, or coll form containing a call form."
       {:added "Encore v3.75.0 (2024-01-29)"}
       [x]
       (when x
         (cond
           (call-form? x) true
           (coll?      x) (if (rsome call-in-form? x) true false)
           :else          false)))

     (defn ^:no-doc runtime-form? "Private, don't use." {:added "Encore v3.67.0 (2023-09-08)"} [form]    (or (symbol? form) (call-form? form)))
     (defn ^:no-doc const-form?   "Private, don't use." {:added "Encore v3.67.0 (2023-09-08)"} [form]    (not    (runtime-form? form)))
     (defn ^:no-doc const-form    "Private, don't use." {:added "Encore v3.67.0 (2023-09-08)"} [form]    (when   (const-form?   form) form))
     (defn ^:no-doc const-forms?  "Private, don't use." {:added "Encore v3.68.0 (2023-09-25)"} [& forms] (revery? const-form?   forms))
     (defn ^:no-doc const-forms   "Private, don't use." {:added "Encore v3.68.0 (2023-09-25)"} [& forms] (mapv    const-form    forms))))

;;;; Errors

(defn ex-message
  "Same as `core/ex-message` (added in Clojure v1.10)."
  {:added "Encore v3.41.0 (2022-12-03)"}
  [ex]
  #?(:clj  (when (instance? Throwable ex) (.getMessage ^Throwable ex))
     :cljs (when (instance? js/Error  ex) (.-message              ex))))

(defn ex-cause
  "Same as `core/ex-cause` (added in Clojure v1.10)."
  {:added "Encore v3.41.0 (2022-12-03)"}
  [ex]
  #?(:clj  (when (instance? Throwable     ex) (.getCause ^Throwable ex))
     :cljs (when (instance? ExceptionInfo ex) (.-cause              ex))))

#?(:clj
   (defmacro try*
     "Like `try`, but `catch` clause classnames can be the special keywords
       `:any` or `:common` for cross-platform catching. Addresses CLJ-1293."

     {:added "Encore v3.67.0 (2023-09-08)"
      :arglists '([expr catch-clause-or-clauses ?finally-clause])}

     [expr & clauses]
     (let [cljs? (some? (:ns &env))
           clauses
           (mapv
             (fn [in]
               (cond
                 (not (call-form? in)) in
                 :let [[s1 s2 & more]  in]

                 (not= s1 'catch)    in
                 (not (keyword? s2)) in

                 :else
                 (let [s2
                       (case s2
                         ;; Note unfortunate naming of `:default` in Cljs to refer to any error type
                         (:any    :all)     (if cljs? :default  `Throwable)
                         (:common :default) (if cljs? 'js/Error `Exception)
                         (throw
                           (ex-info "Unexpected `taoensso.encore/try*` catch clause keyword"
                             {:given    {:value s2, :type (type s2)}
                              :expected '#{:any :common}})))]
                   (list* 'catch s2 more))))
             clauses)]

       `(try ~expr ~@clauses))))

(comment (macroexpand '(try* (/ 1 0) (catch :any t t 1 2 3) (finally 1 2 3))))

#?(:clj
   (defmacro catching
     "Terse, cross-platform `try/catch/finally`.
     See also `try*` for more flexibility."
     ([try-expr                                             ] `(try* ~try-expr (catch :any        ~'_        nil)))
     ([try-expr            error-sym catch-expr             ] `(try* ~try-expr (catch :any        ~error-sym ~catch-expr)))
     ([try-expr            error-sym catch-expr finally-expr] `(try* ~try-expr (catch :any        ~error-sym ~catch-expr) (finally ~finally-expr)))
     ([try-expr error-type error-sym catch-expr finally-expr] `(try* ~try-expr (catch ~error-type ~error-sym ~catch-expr) (finally ~finally-expr)))))

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

(declare submap? str-contains? re-pattern?)

(defn matching-error
  "Given an error (e.g. Throwable) and matching criteria.
  Returns the error if it matches all criteria, otherwise returns nil.

  `kind` may be:
    - A predicate function, (fn match? [x]) -> bool
    - A class (e.g. `ArithmeticException`, `AssertionError`, etc.)
    - `:all`     => any    platform error (Throwable or js/Error, etc.)
    - `:common`  => common platform error (Exception or js/Error)
    - `:ex-info` => a `ExceptionInfo` as created by `ex-info`
    - A set of `kind`s as above, at least one of which must match

  `pattern` may be:
    - A string or Regex against which `ex-message` must match
    - A map             against which `ex-data`    must match using `submap?`
    - A set of `pattern`s as above, at least one of which must match

  When an error with (nested) causes doesn't match, a match will be attempted
  against its (nested) causes.

  Low-level util, see also `throws`, `throws?`."
  {:added "Encore v3.70.0 (2023-10-17)"}
  ([     err] err)
  ([kind err]
   (when-let [match?
              (cond
                (fn?  kind) (kind err)                           ; pred kind
                (set? kind) (rsome #(matching-error % err) kind) ; set  kind
                :else
                (case kind
                  (:all    :any)     #?(:clj (instance? Throwable                  err) :cljs (some?                   err))
                  (:common :default) #?(:clj (instance? Exception                  err) :cljs (instance? js/Error      err))
                  :ex-info           #?(:clj (instance? clojure.lang.ExceptionInfo err) :cljs (instance? ExceptionInfo err))
                  (do                        (instance? kind                       err))))]
     err))

  ([kind pattern err]
   (if-let [match?
            (and
              (matching-error kind err)
              (cond
                (nil?        pattern) true
                (set?        pattern) (rsome #(matching-error kind % err) pattern)
                (string?     pattern) (str-contains?   (ex-message   err) pattern)
                (re-pattern? pattern) (re-find pattern (ex-message   err))
                (map?        pattern) (when-let [data (ex-data err)] (submap? data pattern))
                :else
                (unexpected-arg! pattern
                  {:context  `matching-error
                   :expected '#{nil set string re-pattern map}})))]
     err
     ;; Try match cause
     (when-let [cause (ex-cause err)]
       (matching-error kind pattern cause)))))

#?(:clj
   (defmacro throws
     "Evals `form` and if it throws an error that matches given criteria using
     `matching-error`, returns the matching error. Otherwise returns nil.

     See also `matching-error`, `throws?`."
     {:added "Encore v3.31.0 (2022-10-27)"}
     ([             form] `                               (catching (do ~form nil) ~'t ~'t))
     ([kind         form] `(matching-error ~kind          (catching (do ~form nil) ~'t ~'t)))
     ([kind pattern form] `(matching-error ~kind ~pattern (catching (do ~form nil) ~'t ~'t)))))

#?(:clj
   (defmacro throws?
     "Evals `form` and if it throws an error that matches given criteria using
     `matching-error`, returns true. Otherwise returns false.

     Useful for unit tests, e.g.:
       (is (throws? {:a :b} (throw (ex-info \"Test\" {:a :b :c :d}))))

     See also `matching-error`, `throws`."
     {:added "Encore v3.31.0 (2022-10-27)"}
     ([             form] `(boolean (throws                ~form)))
     ([kind         form] `(boolean (throws ~kind          ~form)))
     ([kind pattern form] `(boolean (throws ~kind ~pattern ~form)))))

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
    {:added "Encore v3.32.0 (2022-11-07)"}
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

(defn catching-xform
  "Like `catching-rf`, but applies to a transducer (`xform`).

  Makes debugging transductions much easier by greatly improving
  the information provided in any errors thrown by `xform` or the
  reducing fn:

    (transduce
      (catching-xform (comp (filter even?) (map inc))) ; Modified xform
      <reducing-fn>
      <...>)"

  {:added "Encore v3.32.0 (2022-11-07)"}
  ([error-fn xform] (comp (fn [rf] (catching-rf error-fn rf)) xform))
  ([         xform] (comp           catching-rf               xform)))

;;;; Vars, etc.

#?(:clj
   (let [resolve-clj clojure.core/resolve ; Returns Var
         resolve-cljs ; Returns analysis map
         (when-let [ns (find-ns 'cljs.analyzer.api)
                    v  (ns-resolve ns 'resolve)]
           @v)

         resolve-auto
         (fn [macro-env sym]
           (when-let [m
                      (if (:ns macro-env)
                        (when resolve-cljs (catching (resolve-cljs macro-env sym)))
                        (do                    (meta (resolve-clj  macro-env sym))))]
             (symbol (str (:ns m)) (name (:name m)))))]

     ;; Note a couple fundamental limitations:
     ;;   - Macros targeting Cljs cannot expand to a Cljs symbol in an unrequired namespace.
     ;;   - Macros targeting Cljs cannot eval      a Cljs symbol for its value at macro time.

     (defn resolve-sym
       "Returns resolved qualified Clj/s symbol, or nil."
       {:added "Encore v3.63.0 (2023-07-31)"}
       ([          sym                ] (resolve-sym nil       sym false)) ; Clj only
       ([macro-env sym                ] (resolve-sym macro-env sym false))
       ([macro-env sym may-require-ns?]
        (when (symbol? sym)
          (if-not may-require-ns?
            (resolve-auto macro-env sym)
            (or
              (resolve-auto macro-env sym)
              (when-let [ns (namespace sym)]
                (when (catching (do (require (symbol ns)) true))
                  (resolve-auto macro-env sym))))))))))

(comment (resolve-sym nil 'string?))

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

     {:added "Encore v3.61.0 (2023-07-07)"}
     [& body] `(with-meta (do ~@body) (meta ~'&form))))

#?(:clj
   (defn get-source
     "Returns {:keys [ns line column file]} source location given a macro's
     compile-time `&form` and `&env` vals. See also `keep-callsite`."
     {:added "Encore v3.61.0 (2023-07-07)"}
     [macro-form macro-env]
     (let [{:keys [line column file]} (meta macro-form)
           file
           (if-not (:ns macro-env)
             *file* ; Compiling clj
             (or    ; Compiling cljs
               (when-let [url (and file (catching (io/resource file)))]
                 (catching (.getPath (io/file url)))
                 (do                 (str     url)))
               file))]

       {:ns     (str *ns*)
        :line   line
        :column column
        :file
        (when (string? file)
          (when-not (contains? #{"NO_SOURCE_PATH" "NO_SOURCE_FILE" ""} file)
            file))})))

(comment (io/resource "taoensso/encore.cljc"))

#?(:clj
   (defmacro update-var-root!
     "Updates root binding (value) of the var identified by given symbol, and returns
     the new value:
       (update-var-root! my-var (fn [old-root-val] <new-root-val>)) => <new-root-val>

     Similar to `alter-var-root` but cross-platform and takes a symbol rather than a var.
     See also `set-var-root!`."
     {:added "Encore v3.68.0 (2023-09-25)"}
     [var-sym update-fn]
     (if (:ns &env)
       `(set!                ~var-sym (~update-fn ~var-sym))
       `(alter-var-root (var ~var-sym) ~update-fn))))

#?(:clj
   (defmacro set-var-root!
     "Sets root binding (value) of the var identified by given symbol, and returns
     the new value. Cross-platform. See also `update-var-root!`."
     {:added "Encore v3.75.0 (2024-01-29)"}
     [var-sym root-val]
     (if (:ns &env)
       `(set!                ~var-sym           ~root-val)
       `(alter-var-root (var ~var-sym) (fn [_#] ~root-val)))))

;;;; Tests

(defn test-fixtures
  "Given a {:before ?(fn []) :after ?(fn [])} map, returns cross-platform
  test fixtures for use by both `clojure.test` and `cljs.test`:

    (let [f (test-fixtures {:before (fn [] (test-setup))})]
      (clojure.test/use-fixtures :once f)
         (cljs.test/use-fixtures :once f))"

  {:added "Encore v3.31.0 (2022-10-27)"}
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
;; - Could really do with a portable ^boolean hint
;; - Some of these have slowly been getting added to Clojure core,
;;   make sure to :exclude any official preds using the same name

#?(:clj
   (do
     (defn stringy?    [x] (or (keyword? x) (string? x)))
     (defn ident?      [x] (or (keyword? x) (symbol? x)))
     (defn boolean?    [x] (instance? Boolean                           x))
     (defn uri?        [x] (instance? java.net.URI                      x))
     (defn indexed?    [x] (instance? clojure.lang.Indexed              x))
     (defn named?      [x] (instance? clojure.lang.Named                x))
     (defn nameable?   [x] (or (named? x) (string? x)))
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
     (defn inst?       [x] (instance? java.time.Instant                 x))

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
     (defn ^boolean indexed?    [x] (implements? IIndexed            x))
     (defn ^boolean named?      [x] (implements? INamed              x))
     (defn ^boolean nameable?   [x] (or (named? x) (string? x)))
     (defn ^boolean editable?   [x] (implements? IEditableCollection x))
     (defn ^boolean derefable?  [x] (implements? IDeref              x))
     ;; (defn throwable?        [x])
     ;; (defn exception?        [x])
     (defn ^boolean error?      [x] (instance?   js/Error             x))
     (defn ^boolean atom?       [x] (instance?   Atom                 x))
     (defn ^boolean transient?  [x] (instance?   ITransientCollection x))
     (defn ^boolean lazy-seq?   [x] (instance?   LazySeq              x))
     (defn ^boolean re-pattern? [x] (instance?   js/RegExp            x))
     (defn ^boolean can-meta?   [x] (implements? IWithMeta            x)) ; Not IMeta
     (defn ^boolean inst?       [x] (instance?   js/Date              x))

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

(defn finite-num?
  "Returns true iff given a number (of standard type) that is:
  finite (excl. NaN and infinities)."
  #?(:cljs {:tag 'boolean})
  [x]
  #?(:clj  (and (number? x) (Double/isFinite x)) ; Works with other types, incl. ratio
     :cljs (js/Number.isFinite x)
     #_
     (and
       (not ^boolean (js/isNaN x))
       #_(not (identical? x js/Infinity))
       (not (identical? x js/Number.POSITIVE_INFINITY))
       (not (identical? x js/Number.NEGATIVE_INFINITY)))))

(defn int?
  "Returns true iff given a number (of standard type) that is:
  a fixed-precision integer."
  #?(:cljs {:tag 'boolean})
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

(defn float?
  "Returns true iff given a number (of standard type) that is:
  a fixed-precision floating-point (incl. NaN and infinities)."
  #?(:cljs {:tag 'boolean})
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

(defn pnum?
  "Returns true iff given number in unsigned unit proportion interval ∈ℝ[0,1]."
  #?(:cljs {:tag 'boolean})
  [x] (and (number? x) (let [n (double x)] (and (>= n 0.0) (<= n 1.0)))))

(defn rnum?
  "Returns true iff given number in signed unit proportion interval ∈ℝ[-1,1]."
  #?(:cljs {:tag 'boolean})
  [x] (and (number? x) (let [n (double x)] (and (>= n -1.0) (<= n +1.0)))))

(def ^:const have-core-async?
  "Is `clojure.core.async` present (not necessarily loaded)?"
  (compile-if
    (or
      (io/resource "clojure/core/async.cljc")
      (io/resource "clojure/core/async.clj"))
    true
    false))

(defn chan?
  "Returns true iff given a `clojure.core.async` channel."
  #?(:cljs {:tag 'boolean})
  [x]
  ;; Avoid actually loading `core.async`
  #?(:clj  (=     "clojure.core.async.impl.channels.ManyToManyChannel" (.getName (class x)))
     :cljs (instance? cljs.core.async.impl.channels.ManyToManyChannel                   x)))

(comment (chan? (clojure.core.async/chan)))

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
(defn when? #?(:cljs {:tag 'boolean}) [pred x] (when (try-pred pred x) x))

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

(defn ^:no-doc -as-throw [kind x]
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

#?(:clj (declare caching-satisfies?))
#?(:clj
   (defmacro satisfies?
     "Faster `satisfies?` to work around CLJ-1814 until a proper upstream fix.
     May cache, so possibly inappropriate for dynamic work."
     {:added "Encore v3.75.0 (2024-01-29)"}
     [protocol x]
     (if (:ns &env)
       ;; `(cljs.core/implements? ~protocol ~x)
       `(cljs.core/satisfies?     ~protocol ~x)
       `(caching-satisfies?       ~protocol ~x))))

#?(:clj
   (defmacro satisfies!
     "If (satisfies? protocol arg) is true, returns arg.
     Otherwise throws runtime `ExceptionInfo` with `unexpected-arg!`.
     See `unexpected-arg!` for more info."
     {:added "Encore v3.51.0 (2023-03-13)"
      :arglists
      '([protocol arg]
        [protocol arg   {:keys [msg context param ...]}]
        [protocol arg & {:keys [msg context param ...]}])}

     ([protocol arg          ] `(satisfies! ~protocol ~arg nil))
     ([protocol arg k1 & more] `(satisfies! ~protocol ~arg ~(apply hash-map k1 more)))
     ([protocol arg opts]
      (let [opts (conj {:expected `(quote (satisfies? ~protocol ~'arg))} opts)]
        `(let [arg# ~arg]
           (if (satisfies? ~protocol arg#)
             arg#
             (unexpected-arg! arg# ~opts)))))))

(comment (macroexpand '(satisfies! my-protocol arg :k1 :v1 :k2 :v2)))

#?(:clj
   (defmacro instance!
     "If (instance? class arg) is true, returns arg.
     Otherwise throws runtime `ExceptionInfo` with `unexpected-arg!`.
     See `unexpected-arg!` for more info."
     {:added "Encore v3.51.0 (2023-03-13)"
      :arglists
      '([class arg]
        [class arg   {:keys [msg context param ...]}]
        [class arg & {:keys [msg context param ...]}])}

     ([class arg          ] `(instance! ~class ~arg nil))
     ([class arg k1 & more] `(instance! ~class ~arg ~(apply hash-map k1 more)))
     ([class arg opts]
      (let [opts (conj {:expected `(quote (instance? ~class ~'arg))} opts)]
        `(let [arg# ~arg]
           (if (instance? ~class arg#)
             arg#
             (unexpected-arg! arg# ~opts)))))))

(comment (macroexpand '(instance! String 5 :k1 :v1 :k2 :v2)))

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
     (defn bytes?
       "Returns true iff given byte[] argument."
       ;; Also introduced in Clojure v1.9-alpha5+
       [x] (instance? bytes-class x))

     (defn ba=
       "Returns true iff given two byte[]s with the same content."
       [^bytes x ^bytes y] (java.util.Arrays/equals x y))

     (defn ba-hash
       "Returns hash int of given byte[]."
       ^long [^bytes x] (java.util.Arrays/hashCode x))

     ;; Java strings are UTF-16, but we'll use UTF-8 encoding when converting to/from bytes
     (defn utf8-ba->str {:added "Encore v3.53.0 (2023-03-22)"} ^String [^bytes ba] (String. ba  java.nio.charset.StandardCharsets/UTF_8))
     (defn str->utf8-ba {:added "Encore v3.53.0 (2023-03-22)"} ^bytes  [^String s] (.getBytes s java.nio.charset.StandardCharsets/UTF_8))

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

;;;; Reductions

(defn   convey-reduced [x] (if (reduced? x) (reduced x) x)) ; Double-wrap
(defn preserve-reduced
  "Public version of `core/preserving-reduced`."
  [rf]
  (fn [acc in]
    (let [result (rf acc in)]
      (if (reduced? result)
        (reduced result)
        (do      result)))))

(defn reduce-kvs
  "Like `reduce-kv` but takes a flat sequence of kv pairs."
  [rf init kvs]
  (transduce (partition-all 2)
    (completing (fn [acc [k v]] (rf acc k v))) init kvs))

(defn reduce-n
  ;; No longer so interesting with Clojure 1.7+
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
     [f init o] (reduce (fn [acc k] (f acc k (gobj/get o k nil))) init (js-keys o))))

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
  {:added "Encore v3.33.0 (2022-11-15)"}
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
        (let [result (rf acc (first xs) (first ys))]
          (if (reduced? result)
            @result
            (recur result
              (next xs)
              (next ys))))
        acc))))

(do
  (deftype ^:no-doc Tup2 [x y  ])
  (deftype ^:no-doc Tup3 [x y z]))

(defn reduce-multi
  "Like `reduce` but supports separate simultaneous accumulators
  as a micro-optimisation when reducing a large collection multiple
  times."
  ;; Faster than using volatiles
  {:added "Encore v3.66.0 (2023-08-23)"}
  ([rf  init            coll] (reduce rf init coll))
  ([rf1 init1 rf2 init2 coll]
   (let [^Tup2 tuple
         (reduce
           (fn [^Tup2 tuple in]
             (let [x   (.-x tuple)
                   y   (.-y tuple)
                   rx? (reduced? x)
                   ry? (reduced? y)]

               (if (and rx? ry?)
                 (reduced tuple)
                 (let [x (if rx? x (rf1 x in))
                       y (if ry? y (rf2 y in))]
                   (Tup2. x y)))))
           (Tup2. init1 init2)
           coll)]

     [(unreduced (.-x tuple))
      (unreduced (.-y tuple))]))

  ([rf1 init1 rf2 init2 rf3 init3 coll]
   (let [^Tup3 tuple
         (reduce
           (fn [^Tup3 tuple in]
             (let [x   (.-x tuple)
                   y   (.-y tuple)
                   z   (.-z tuple)
                   rx? (reduced? x)
                   ry? (reduced? y)
                   rz? (reduced? z)]

               (if (and rx? ry? rz?)
                 (reduced tuple)
                 (let [x (if rx? x (rf1 x in))
                       y (if ry? y (rf2 y in))
                       z (if rz? z (rf3 z in))]
                   (Tup3. x y z)))))
           (Tup3. init1 init2 init3)
           coll)]

     [(unreduced (.-x tuple))
      (unreduced (.-y tuple))
      (unreduced (.-z tuple))])))

(defn reduce-interleave-all
  "Reduces sequence of elements interleaved from given `colls`.
  (reduce-interleave-all conj [] [[:a :b] [1 2 3]]) => [:a 1 :b 2 3]"
  {:added "Encore v3.66.0 (2023-08-23)"}
  [rf init colls]
  (if (empty? colls)
    init
    (loop [acc init, colls colls]
      (let [^Tup2 tuple
            (reduce
              (fn [^Tup2 tuple in]
                (if (empty? in)
                  tuple
                  (let [[in1 & next-in] in
                        acc (.-x tuple)
                        ncs (.-y tuple)
                        res (rf acc in1)]

                    (if (reduced? res)
                      (reduced (Tup2. @res nil))
                      (do      (Tup2. res
                                 (if next-in
                                   (conj (or ncs []) next-in)
                                   (do       ncs))))))))
              (Tup2. acc nil)
              colls)

            acc        (.-x tuple)
            next-colls (.-y tuple)]

        (if next-colls
          (recur acc next-colls)
          (do    acc))))))

;;;; Math

(def ^:const max-long #?(:clj Long/MAX_VALUE :cljs  9007199254740991))
(def ^:const min-long #?(:clj Long/MIN_VALUE :cljs -9007199254740991))

(defn approx==
  #?(:cljs {:tag 'boolean})
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
(defn abs [n]     (if (neg? n) (- n) n))
(defn round*
  ([               n] (round* :round nil n))
  ([kind           n] (round* kind   nil n))
  ([kind precision n]
   (let [n        (double n)
         modifier (when precision (Math/pow 10.0 precision))
         n*       (if modifier (* n ^double modifier) n)
         rounded
         (case kind
           :round (Math/round n*)
           :floor (Math/floor n*)
           :ceil  (Math/ceil  n*)
           :trunc (long       n*)
           (throw
             (ex-info "[encore/round*] Unexpected round kind (expected ∈ #{:round :floor :ceil :trunc})"
               {:kind {:value kind :type (type kind)}})))]

     (if-not modifier
       (do (long   rounded))                  ; Returns long
       (/  (double rounded) ^double modifier) ; Return double
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
  (defn roundn ^double [precision n]
    (let [p (Math/pow 10.0 (long precision))]
      (/ (double (Math/round (* (double n) p))) p)))

  (defn perc ^long [n divisor] (Math/round (* (/ (double n) (double divisor)) 100.0))))

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

(defn chance
  "Returns true with given probability ∈ ℝ[0,1]."
  {:tag #?(:cljs 'boolean :default nil)
   :inline
   (fn [prob]
     (if (const-form? prob)
       `(< (Math/random) ~(as-pnum! prob))
       `(< (Math/random)  (double  ~prob))))}

  [prob] (< (Math/random) (double prob)))

(comment (chance 1.2))

;;;; Cljs basics

;; js/foo      - `foo` in global object/ns (depends on *target*)
;; js/window   - `window` object: global ns in browsers
;; js/global   - `global` object: global ns in Node.js, etc.?
;; goog/global - Closure's environment-agnostic global object

#?(:cljs (def node-target? (= *target* "nodejs")))
#?(:cljs (def ^:no-doc js-?window  (when (exists? js/window)  js/window)))  ; Present iff in browser
#?(:cljs (def ^:no-doc js-?process (when (exists? js/process) js/process))) ; Present iff in Node.js
#?(:cljs (def ^:no-doc js-?crypto
           (or
             (when (exists? js/crypto) js/crypto)
             (when (exists? js/window) (gobj/get js/window "crypto")))))

;;;; Misc

(defn force-ref "Like `force` for refs." [x] (if (derefable? x) (deref x) x))
(defn force-var "Like `force` for vars." [x] (if (var?       x) (deref x) x))
(defn merge-meta   [x m] (with-meta x (merge (meta x) m)))
(defn without-meta [x] (if (meta x) (with-meta x nil) x))

(defn some=
  #?(:cljs {:tag 'boolean})
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
        (ex-info "Insufficient `com.taoensso/encore` version, you may have a dependency conflict: see `https://www.taoensso.com/dependency-conflicts` for solutions."
          {:min-version  (str/join "." [xm ym zm])
           :your-version (str/join "." [xc yc zc])})))))

(comment (assert-min-encore-version 3.10))

;;;; Collections

(defn map-entry
  "Returns a `MapEntry` with given key and value."
  {:added  "Encore v3.75.0 (2024-01-29)"
   :inline #?(:clj (fn [k v] `(clojure.lang.MapEntry/create ~k ~v)) :default nil)}
  [k v]
  #?(:clj  (clojure.lang.MapEntry/create k v)
     :cljs              (MapEntry.       k v nil)))

(defn queue?
  "Returns true iff given a `PersistentQueue`."
  #?(:cljs {:tag 'boolean})
  [x]
  #?(:clj  (instance? clojure.lang.PersistentQueue x)
     :cljs (instance?    cljs.core.PersistentQueue x)))

(defn queue
  "Returns a new `PersistentQueue`."
  ([coll] (into (queue) coll))
  ([    ]
   #?(:clj clojure.lang.PersistentQueue/EMPTY
      :cljs   cljs.core.PersistentQueue.EMPTY)))

(defn queue*
  "Returns a new `PersistentQueue` given items."
  [& items] (queue items))

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
     ([  k          ] (gobj/get js-?window (name k)))
     ([o k          ] (gobj/get o          (name k) nil))
     ([o k not-found] (gobj/get o          (name k) not-found))))

#?(:cljs
   (let [sentinel (js-obj)]
     ;; Could also use `gobg/getValueByKeys`
     (defn oget-in "Like `get-in` for JS objects."
       ([  ks          ] (oget-in js-?window ks nil))
       ([o ks          ] (oget-in o          ks nil))
       ([o ks not-found]
        (loop [o o, ks (seq ks)]
          (if ks
            (let [o (gobj/get o (name (first ks)) sentinel)]
              (if (identical? o sentinel)
                not-found
                (recur o (next ks))))
            o))))))

(defn get1
  "Like `get` but returns val for first key that exists in map.
  Useful for key aliases or fallbacks. See also `get*`."
  {:added "Encore v3.67.0 (2023-09-08)"}
  ([m k                 ] (get m k))
  ([m k        not-found] (get m k not-found))
  ([m k1 k2    not-found] (if-let [e (and m (or (find m k1) (find m k2)))            ] (val e) not-found))
  ([m k1 k2 k3 not-found] (if-let [e (and m (or (find m k1) (find m k2) (find m k3)))] (val e) not-found)))

#?(:clj
   (defmacro get*
     "Macro version of `get` that:

      1. Avoids unnecessary evaluation of `not-found`.
         Useful when `not-found` is expensive or contains side-effects.

      2. Supports multiple prioritized keys (k1, k2, etc.). Returns val for first
         key that exists in map. Useful for key aliases or fallbacks.

    Equivalent to:

      (cond
        (contains? m k1) (get m k1)
        (contains? m k2) (get m k2)
        ...
        :else            not-found)"

     {:added "Encore v3.82.0 (2024-02-23)"}
     ([m k                 ]                                      `(get  ~m ~k))
     ([m k        not-found]              `(if-let [e#             (find ~m ~k)                               ] (val e#) ~not-found))
     ([m k1 k2    not-found] `(let [m# ~m] (if-let [e# (and m# (or (find m# ~k1) (find m# ~k2)))              ] (val e#) ~not-found)))
     ([m k1 k2 k3 not-found] `(let [m# ~m] (if-let [e# (and m# (or (find m# ~k1) (find m# ~k2) (find m# ~k3)))] (val e#) ~not-found)))))

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
    ([m k v      ] (if (contains? m k) m (assoc m k v)))
    ([m k v & kvs] (reduce-kvs assoc-nx (assoc-nx m k v) kvs))
    ([m       kvs]
     (reduce-kvs
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

(defn distinct-elements?
  #?(:cljs {:tag 'boolean})
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
  "Like `core/into` but assumes `to!` is a transient, and doesn't call
  `persist!` when done. Useful as a performance optimization in some cases."
  #_([            ]                        [])
  ([to!           ]                        to!)
  ([to!       from] (reduce          conj! to! from))
  ([to! xform from] (transduce xform conj! to! from)))

(defn xdistinct
  "Returns a stateful transducer like (core/distinct) that supports an optional
  key function. Retains only items with distinct (keyfn <item>)."
  ([     ] (distinct)) ; core now has a distinct transducer
  ([keyfn]
   (fn [rf]
     (let [seen_ (volatile! (transient #{}))]
       (fn
         ([      ] (rf))
         ([acc   ] (rf acc))
         ([acc in]
          (let [k (keyfn in)]
            (if (contains? @seen_ k)
              acc
              (do
                (vswap! seen_ conj! k)
                (rf acc in))))))))))

(comment (into [] (xdistinct identity) [1 2 3 1 4 5 2 6 7 1]))

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
  given {<old-new> <new-key>} replacements. O(min(n_replacements, n_m))."
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
  (defn ks=      #?(:cljs {:tag 'boolean}) [ks m] (=             (set (keys m)) (ensure-set ks)))
  (defn ks<=     #?(:cljs {:tag 'boolean}) [ks m] (set/subset?   (set (keys m)) (ensure-set ks)))
  (defn ks>=     #?(:cljs {:tag 'boolean}) [ks m] (set/superset? (set (keys m)) (ensure-set ks)))
  (defn ks-nnil? #?(:cljs {:tag 'boolean}) [ks m] (revery?    #(some? (get  m %))           ks)))

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
    - Adds support for special return vals: `:update/dissoc`, `:update/abort`."
  ([m ks           f] (update-in m ks nil f))
  ([m ks not-found f]
   (if (empty? ks)
     (f m) ; m would also be a sensible choice, but (f m) is more useful
     (let [old (get-in m ks not-found)
           new (f old)]
       (case new
         (:update/abort  :swap/abort) m
         (:update/dissoc :swap/dissoc)
         (fsplit-last ks
           (fn [ks lk]
             (update-in m ks nil
               (fn [v]
                 (if v ; Assume associative
                   (dissoc v lk)
                   :update/abort)))))

         (assoc-in m ks new))))))

(defn contains-in?
  #?(:cljs {:tag 'boolean})
  ([coll ks k] (contains? (get-in coll ks) k))
  ([coll ks  ]
   (if (empty? ks)
     false
     (fsplit-last ks (fn [ks lk] (contains-in? coll ks lk))))))

(defn dissoc-in
  ([m ks dissoc-k       ] (update-in m ks nil (fn [m] (if m (dissoc m dissoc-k) :update/abort))))
  ([m ks dissoc-k & more]
   (update-in m ks nil
     (fn [m]
       (if m
         (reduce dissoc (dissoc m dissoc-k) more)
         :update/abort))))

  ([m ks]
   (if (empty? m)
     (do       m)
     (fsplit-last ks (fn [ks lk] (dissoc-in m ks lk))))))

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

(defn interleave-all
  "Like `interleave` but includes all items (i.e. stops when the longest
  rather than shortest coll has been consumed)."
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

(comment (interleave-all [:a1 :a2 :a3] [:b1 :b2] [:c1 :c2 :c3 :c4]))

(defn vinterleave-all
  "Like `interleave`, but:
    - Returns a vector rather than lazy seq (=> greedy).
    - Includes all items (i.e. stops when the longest rather than
      shortest coll has been consumed).

  Single-arity version takes a coll of colls."
  {:added "Encore v3.66.0 (2023-08-23) for !=2 arities"}
  ([colls] (if (empty? colls) [] (persistent! (reduce-interleave-all conj! (transient []) colls))))
  ([c1 c2] ; Optimized common case
   (loop [v (transient []) s1 (seq c1) s2 (seq c2)]
     (cond
       (and s1 s2)
       (recur (conj! (conj! v (first s1)) (first s2)) (next s1) (next s2))
       s1    (persistent! (reduce conj! v s1))
       s2    (persistent! (reduce conj! v s2))
       :else (persistent! v))))

  ([c1 c2 c3        ] (vinterleave-all       [c1 c2 c3]))
  ([c1 c2 c3 & colls] (vinterleave-all (into [c1 c2 c3] colls))))

(comment
  (qb 1e5
    (vec (interleave-all [:a :b :c :d] [:a :b :c :d :e]))
        (vinterleave-all [:a :b :c :d] [:a :b :c :d :e])))

#?(:clj (defmacro new-object [] (if (:ns &env) `(cljs.core/js-obj) `(Object.))))

(defn ^:no-doc -merge-with
  "Private, don't use. Low-level merge function, flexible and optimized."
  ([nest? f maps] (reduce (partial -merge-with nest? f) nil maps))
  ([nest? f m1 m2]
   (cond
     :let  [n2  (count m2)]
     (zero? n2) (or m1 m2)

     :if-let [e (find m2 :merge/replace?)] ; Currently undocumented
     (let [m2 (dissoc m2 :merge/replace?)]
       (if (val e)
         (do                     m2)
         (-merge-with nest? f m1 m2)))

     :let [n1 (count m1)]

     (>= n1 n2)
     (reduce-kv
       (fn [acc1 k2 v2] ; m2 kvs into m1
         (cond
           (case v2 (:merge/dissoc :swap/dissoc) true false) (dissoc acc1 k2)
           :let [v1 (get m1 k2 ::nx)]
           (and nest? (map? v1) (map? v2)) (assoc acc1 k2 (-merge-with true f v1 v2))
           (identical-kw? v1 ::nx)         (assoc acc1 k2 v2)
           :else
           (let [v3 (f v1 v2)]
             (if (case v3 (:merge/dissoc :swap/dissoc) true false)
               (dissoc acc1 k2)
               (assoc  acc1 k2 v3)))))
       m1 m2)

     :else
     (reduce-kv
       (fn [acc2 k1 v1] ; m1 kvs into m2
         (cond
           :let [v2 (get m2 k1 ::nx)]
           (case v2 (:merge/dissoc :swap/dissoc) true false) (dissoc acc2 k1)
           (and nest? (map? v1) (map? v2)) (assoc acc2 k1 (-merge-with true f v1 v2))
           (identical-kw? v2 ::nx)         (assoc acc2 k1 v1)
           :else
           (let [v3 (f v1 v2)]
             (if (case v3 (:merge/dissoc :swap/dissoc) true false)
               (dissoc acc2 k1)
               (assoc  acc2 k1 v3)))))
       m2 m1))))

(let [-merge-with -merge-with]

  (defn merge
    "Like `core/merge` but faster, supports `:merge/dissoc` rvals."
    [& maps] (-merge-with false (fn [x y] y) maps))

  (defn merge-with
    "Like `core/merge-with` but faster, supports `:merge/dissoc` rvals."
    [f & maps] (-merge-with false f maps))

  (defn nested-merge
    "Like `merge` but does nested merging."
    [& maps] (-merge-with true (fn [x y] y) maps))

  (defn nested-merge-with
    "Like `merge-with` but does nested merging."
    [f & maps] (-merge-with true f maps)))

(defn fast-merge
  "Like `core/merge` but faster.
  Doesn't support zero arity, single arity case takes a collection of maps."
  ([maps] (reduce fast-merge nil maps))
  ([m1 m2]
   (let [n2 (count m2)]
     (if (zero? n2)
       (or m1 m2)
       (let [n1 (count m1)]
         (if (>= n1 n2)
           (reduce-kv assoc m1 m2) ; Unexpectedly faster than (conj m1 m2)
           (reduce-kv (fn [m k v] (if (contains? m k) m (assoc m k v))) m2 m1))))))

  ([m1 m2 m3   ] (-> (fast-merge m1 m2) (fast-merge m3)))
  ([m1 m2 m3 m4] (-> (fast-merge m1 m2) (fast-merge m3) (fast-merge m4))))

(comment
  (qb 1e6 ; [382.24 327.81 171.19] ~worst case
    (core-merge {:a :A1} {:a :A2} {:a :A3})
    (merge      {:a :A1} {:a :A2} {:a :A3})
    (fast-merge {:a :A1} {:a :A2} {:a :A3})))

(deftype Pred [pred-fn]) ; Note: can support any arity (unary, kv, etc.)
(defn pred
  "Experimental, subject to change without notice!
  Wraps given predicate fn to return `Pred` for use with `submap?`, etc.
  Arity of predicate fn depends on context in which it'll be used.
  See also `pred-fn`."
  {:added "Encore v3.75.0 (2024-01-29)"}
  ^Pred [pred-fn] (Pred. pred-fn))

(defn pred-fn
  "Experimental, subject to change without notice!
  Returns unwrapped predicate fn when given `Pred`, otherwise returns nil.
  See also `pred`."
  {:added "Encore v3.75.0 (2024-01-29)"}
  [pred]
  (when (instance? Pred pred)
    (.-pred-fn ^Pred pred)))

(defn submap?
  "Returns true iff `sub` is a (possibly nested) submap of `m`,
  i.e. iff every (nested) value in `sub` has the same (nested) value in `m`.
  Uses stack recursion so supports only limited nesting."
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
                   (if-let [pf (pred-fn sval)]
                     (pf mval) ; Arb pred
                     (case sval
                       ;; Special svals currently undocumented
                       :submap/nx      (identical-kw? mval ::nx)
                       :submap/ex (not (identical-kw? mval ::nx))
                       :submap/some            (some? mval)
                       (= sval mval)))]
            true
            (reduced false)))))
    true
    sub))

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

  {:added "Encore v3.34.0 (2022-11-16)"}
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

(comment
  (qb 1e5 ; [18.86 22.74]
    (select-nested-keys  {:a 1 :b 1 :c 1} [:a :c])
    (select-keys         {:a 1 :b 1 :c 1} [:a :c])))

;;;; LightAtom

#?(:cljs
   (deftype LightAtom [^:mutable state]
     IDeref (-deref  [_    ] state)
     IReset (-reset! [_ new] (set! state new) new)
     ISwap  (-swap!  [t swap-fn] (t swap-fn))
     IFn
     (-invoke [_          ] state)
     (-invoke [_   swap-fn] (let [new (swap-fn state)] (set! state new) new))
     (-invoke [_ k swap-fn]
       (let [old-map state
             new-val (swap-fn (get old-map k))
             new-map (assoc old-map k new-val)]
         (set! state new-map)
         (do         new-val))))

   :clj
   (deftype LightAtom [^AtomicReference aref]
     clojure.lang.IDeref (deref [_] (.get aref))
     clojure.lang.IAtom
     (compareAndSet [_ old new] (.compareAndSet aref old new))
     (reset         [_     new] (.set aref new) new)
     (swap          [t swap-fn] (t swap-fn))

     clojure.lang.IFn
     (invoke [_          ] (.get          aref))
     (invoke [_   swap-fn] (.updateAndGet aref (reify UnaryOperator (apply [_ old] (swap-fn old)))))
     (invoke [_ k swap-fn]
       (let [new-map
             (.updateAndGet aref
               (reify UnaryOperator
                 (apply [_ old-map]
                   (let [new-val (swap-fn (get old-map k))
                         new-map (assoc old-map k new-val)]
                     new-map))))]
         (get new-map k)))))

(defn ^:no-doc ^LightAtom latom
  "Private, don't use. Micro-optimized lightweight `atom`.
  Up to 30% faster than standard atoms, with the same atomicity guarantees."
  {:added "Encore v3.67.0 (2023-09-08)"}
  [init-state]
  (LightAtom.
    #?(:clj (AtomicReference. init-state)
       :cljs                  init-state)))

#?(:clj
   (let [atom-tag (compile-if clojure.lang.IAtom 'clojure.lang.IAtom 'clojure.lang.Atom)]
     (defmacro ^:no-doc -cas!?
       "Private, don't use. Micro-optimized `compare-and-set!`."
       {:added "Encore v3.67.0 (2023-09-08)"}
       [atom_ old-val new-val]
       (if (:ns &env)
         `(compare-and-set!          ~atom_                  ~old-val ~new-val)
         `(.compareAndSet ~(with-meta atom_ {:tag atom-tag}) ~old-val ~new-val)))))

(comment (let [a (atom nil)] (qb 1e6 (compare-and-set! a 0 1) (-cas!? a 0 1)))) ; [50.06 35.64]

(comment
  (let [a (atom 0), v (volatile! 0), l (latom 0)]
    {:deref (qb 2e6 @a @v (l))
     :new   (qb 2e6 (atom 0) (volatile! 0) (latom 0))
     :swap  (qb 2e6 (swap! a inc) (vswap! v inc) (l inc))
     :cas   (qb 2e6 (compare-and-set! a 0 1) (-cas!? l 0 1))})

  {:deref [85.75  83.82  63.23], ; ~25% faster
   :new   [115.45 83.92  94.42], ; ~20% faster
   :swap  [144.15 110.94 99.53], ; ~30% faster
   :cas   [102.42        67.85]} ; ~35% faster
  )

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

(defn- -reset-k0!
  "Impln. for 0-key resets"
  [return atom_ m1]
  (loop []
    (let [m0 @atom_]
      (if (-cas!? atom_ m0 m1)
        (return m0 m0 m1 m1) ; [m0 v0 m1 v1]
        (recur)))))

(defn- -reset-k1!
  "Impln. for 1-key resets"
  [return atom_ k not-found v1]
  (loop []
    (let [m0 @atom_
          m1 (assoc m0 k v1)]
      (if (-cas!? atom_ m0 m1)
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
          (if (-cas!? atom_ m0 m1)
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
      (if (-cas!? atom_ old val)
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

  (defn swapped? #?(:cljs {:tag 'boolean}) [x] (instance? Swapped x))
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

      (if (identical-kw? m1 :swap/abort)
        (if sw?
          (return-swapped s1 m0 m1) ; rv
          (return m0 m0 m0 m0)) ; [m0 v0 m1 v1]

        (if (-cas!? atom_ m0 m1)
          (if sw?
            (return-swapped s1 m0 m1) ; rv
            (return m0 m0 m1 m1)) ; [m0 v0 m1 v1]
          (recur))))))

(defn- -swap-k1!
  "Impln. for 1-key swaps"
  [return atom_ k not-found f]
  (if (identical-kw? f :swap/dissoc)
    (loop []
      (let [m0 @atom_
            m1 (dissoc m0 k)]
        (if (-cas!? atom_ m0 m1)
          (return m0 (get m0 k not-found) m1 :swap/dissoc) ; [m0 v0/nx m1 v1]
          (recur))))

    (loop []
      (let [m0  @atom_
            v0  (get m0 k not-found) ; nx
            s1  (f v0)
            sw? (instance? Swapped s1)
            v1  (if sw? (.-newv ^Swapped s1) s1)]

        (if (identical-kw? v1 :swap/abort)
          (if sw?
            (return-swapped s1 m0 m0) ; rv
            (return m0 v0 m0 v0)) ; [m0 v0/nx m1 v1]

          (let [m1
                (if (identical-kw? v1 :swap/dissoc)
                  (dissoc m0 k)
                  (assoc  m0 k v1))]

            (if (-cas!? atom_ m0 m1)
              (if sw?
                (return-swapped s1 m0 m1) ; rv
                (return m0 v0 m1 v1)) ; [m0 v0/nx m1 v1]
              (recur))))))))

(defn- -swap-kn!
  "Impln. for n-key swaps"
  [return atom_ ks not-found f]
  (if-let [ks-seq (seq ks)]
    (if (next ks-seq)

      (if (identical-kw? f :swap/dissoc)
        (loop []
          (let [m0 @atom_
                m1 (dissoc-in m0 ks)]
            (if (-cas!? atom_ m0 m1)
              (return m0 (get-in m0 ks not-found) m1 :swap/dissoc) ; [m0 v0/nx m1 v1]
              (recur))))

        (loop []
          (let [m0  @atom_
                v0  (get-in m0 ks not-found) ; nx
                s1  (f v0)
                sw? (instance? Swapped s1)
                v1  (if sw? (.-newv ^Swapped s1) s1)]

            (if (identical-kw? v1 :swap/abort)
              (if sw?
                (return-swapped s1 m0 m0) ; rv
                (return m0 v0 m0 v0)) ; [m0 v0/nx m1 v1]

              (let [m1
                    (if (identical-kw? v1 :swap/dissoc)
                      (dissoc-in m0 ks)
                      (assoc-in  m0 ks v1))]

                (if (-cas!? atom_ m0 m1)
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

;;;;

#?(:clj
   (let [cache_ (latom {})]
     (defn ^:no-doc caching-satisfies?
       "Private, don't use."
       [protocol x]
       (let [t (if (fn? x) ::fn (type x))]
         (or
           (get (cache_) t)
           (if-let [uncachable-type? (re-find #"\d" (str t))]
             (do               (clojure.core/satisfies? protocol x))
             (cache_ t (fn [_] (clojure.core/satisfies? protocol x)))))))))

;;;; Instants

#?(:clj
   (defn now-udt
     "Returns current system instant as milliseconds since Unix epoch."
     {:inline (fn [] `(System/currentTimeMillis))}
     ^long        []  (System/currentTimeMillis))

   :cljs
   (defn now-udt
     "Returns current system insant as milliseconds since Unix epoch."
     [] (js/Date.now)))

#?(:clj
   (defn now-dt
     "Returns current system instant as `java.util.Date`."
     {:inline   (fn  [] `(java.util.Date.))}
     ^java.util.Date []  (java.util.Date.))

   :cljs
   (defn now-dt
     "Returns current system instant as `js/Date`."
     [] (js/Date.)))

#?(:clj
   (defn now-inst
     "Returns current system instant as `java.time.Instant`."
     {:added "Encore v3.66.0 (2023-08-23)"
      :inline       (fn [] `(java.time.Instant/now))}
     ^java.time.Instant []  (java.time.Instant/now))

   :cljs
   (defn now-inst
     "Returns current system instant as `js/Date`."
     {:added "Encore v3.66.0 (2023-08-23)"}
     [] (js/Date.)))

#?(:clj
   (defn now-nano
     "Returns current value of best-resolution time source as nanoseconds."
     {:inline (fn [] `(System/nanoTime))}
     ^long        []  (System/nanoTime))

   :cljs
   (def now-nano
     "Returns current value of best-resolution time source as nanoseconds."
     (if-let [perf (oget js-?window "performance")
              pf   (or
                     (oget perf "now")   (oget perf "mozNow") (oget perf "webkitNow")
                     (oget perf "msNow") (oget perf "oNow"))]

       (fn [] (Math/floor (* 1e6 (.call pf perf))))
       (fn []             (* 1e6 (js/Date.now))))))

;; Specialist macros to force expanded form (useful for Cljs, other macros, etc.).
#?(:clj (defmacro ^:no-doc now-udt*    "When possible prefer `now-udt`."   [] (if (:ns &env) `(js/Date.now) `(System/currentTimeMillis))))
#?(:clj (defmacro ^:no-doc now-dt*     "When possible prefer `now-dt`."    [] (if (:ns &env) `(js/Date.)    `(java.util.Date.))))
#?(:clj (defmacro ^:no-doc now-inst*   "When possible prefer `now-inst` ." [] (if (:ns &env) `(js/Date.)    `(java.time.Instant/now))))
#?(:clj (defmacro ^:no-doc now-nano*   "When possible prefer `now-nano`."  [] (if (:ns &env) `(now-nano)    `(System/nanoTime))))

;;;; Memoization

(defn memoize-last
  "Like `core/memoize` but only caches the fn's most recent call.
  Great for Reactjs render op caching on mobile devices, etc."
  [f]
  (let [cache_ (latom {})]
    (fn [& args]
      @(or (get (cache_) args)
           (get (cache_
                  (fn swap-fn [cache]
                    (if (get cache args)
                      cache
                      {args (delay (apply f args))})))
             args)))))

(defn fmemoize
  "For Clj: fastest possible memoize. Non-racey, 0-7 arity only.
  For Cljs: same as `core/memoize`."
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
          (let [x* (if (identical? x nil) nil-sentinel x)]
            @(or
               (.get cache1_ x*)
               (let [dv (delay (f x))]
                 (or (.putIfAbsent cache1_ x* dv) dv)))))

         ([x1 x2               ] (let [xs [x1 x2]               ] @(or (.get cachen_ xs) (let [dv (delay (f x1 x2))               ] (or (.putIfAbsent cachen_ xs dv) dv)))))
         ([x1 x2 x3            ] (let [xs [x1 x2 x3]            ] @(or (.get cachen_ xs) (let [dv (delay (f x1 x2 x3))            ] (or (.putIfAbsent cachen_ xs dv) dv)))))
         ([x1 x2 x3 x4         ] (let [xs [x1 x2 x3 x4]         ] @(or (.get cachen_ xs) (let [dv (delay (f x1 x2 x3 x4))         ] (or (.putIfAbsent cachen_ xs dv) dv)))))
         ([x1 x2 x3 x4 x5      ] (let [xs [x1 x2 x3 x4 x5]      ] @(or (.get cachen_ xs) (let [dv (delay (f x1 x2 x3 x4 x5))      ] (or (.putIfAbsent cachen_ xs dv) dv)))))
         ([x1 x2 x3 x4 x5 x6   ] (let [xs [x1 x2 x3 x4 x5 x6]   ] @(or (.get cachen_ xs) (let [dv (delay (f x1 x2 x3 x4 x5 x6))   ] (or (.putIfAbsent cachen_ xs dv) dv)))))
         ([x1 x2 x3 x4 x5 x6 x7] (let [xs [x1 x2 x3 x4 x5 x6 x7]] @(or (.get cachen_ xs) (let [dv (delay (f x1 x2 x3 x4 x5 x6 x7))] (or (.putIfAbsent cachen_ xs dv) dv)))))))))

(comment
  (let [f0 (fmemoize (fn []))
        f1 (fmemoize (fn [x1]))
        f2 (fmemoize (fn [x1 x2]))]

    (qb 1e6 ; [30.14 38.72 65.09]
      (f0) (f1 :x1) (f2 :x1 :x2))))

(defn- gc-now? [rate]
  #?(:clj  (<= (java.lang.Math/random) ^double rate)
     :cljs (<=       (.random js/Math)         rate)))

(comment (qb 1e6 (gc-now? 0.5)))

(deftype SimpleCacheEntry [delay ^long udt])
(deftype TickedCacheEntry [delay ^long udt ^long tick-lru ^long tick-lfu])

(declare top)

(defn cache
  "Returns a cached version of given referentially transparent function `f`.

  Like `core/memoize` but:
    - Often faster, depending on options.
    - Prevents race conditions on writes.
    - Supports cache invalidation by prepending args with:
      - `:cache/del`   ; Delete cached item for subsequent args, returns nil.
      - `:cache/fresh` ; Renew  cached item for subsequent args, returns new val.

    - Supports options:
      - `ttl-ms` ; Expire cached items after <this> many msecs.
      - `size`   ; Restrict cache size to <this> many items at the next garbage
                 ; collection (GC).

      - `gc-every` ; Run garbage collection (GC) approximately once every
                   ; <this> many calls to cached fn. If unspecified, GC rate
                   ; will be determined automatically based on `size`.

  See also `defn-cached`, `fmemoize`, `memoize-last`."

  {:added "Encore v3.36.0 (2022-11-18)"}
  ([f] ; De-raced, commands
   #?(:cljs
      (let [cache_ (volatile! {})
            get-sentinel (js-obj)]

        (fn cached [& xs]
          (let [x1 (first xs)]
            (case x1

              (:cache/del :mem/del)
              (let [xn (next  xs)
                    x2 (first xn)]
                (if (identical-kw? x2 :mem/all)
                  (vreset! cache_ {})
                  (vswap!  cache_ dissoc xn))
                nil)

              (:cache/fresh :mem/fresh)
              (let [xn (next xs)
                    v  (apply f xn)]
                (vswap! cache_ assoc xn v) v)

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
             (case x1

               (:cache/del :mem/del)
               (let [xn (next  xs)
                     x2 (first xn)]
                 (if (identical-kw? x2 :mem/all)
                   (.clear  cache_)
                   (.remove cache_ (or xn nil-sentinel)))
                 nil)

               (:cache/fresh :mem/fresh)
               @(let [xn (next xs)
                      dv (delay (apply f xn))]
                  (.put cache_ (or xn nil-sentinel) dv) dv)

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
           cache_   (latom nil) ; {<args> <TickedCacheEntry>}
           latch_   (latom nil) ; Used to pause writes during gc
           ttl-ms   (long (or ttl-ms 0))
           ttl?     (not (zero? ttl-ms))
           size     (long size)
           gc-every (long (or gc-every (clamp-int 1000 16000 size)))]

       (fn cached [& args]
         (let [a1 (first args)]
           (case a1
             (:cache/del :mem/del)
             (let [argn (next args)
                   a2   (first argn)]
               (if (case a2 (:cache/all :mem/all) true false)
                 (reset! cache_ nil)
                 (cache_ #(dissoc % argn)))
               nil)

             (let [^long tick (ticker) ; Always inc, even on reads
                   instant (if ttl? (now-udt) 0)]

               (when (and
                       ;; We anyway have a tick, so may as well be exact
                       ;; (gc-now? gc-rate)
                       (== (rem tick gc-every) 0)
                       (>= (count (cache_)) (* 1.1 size)))

                 (let [latch #?(:clj (CountDownLatch. 1) :default nil)]

                   (when (-cas!? latch_ nil latch)
                     ;; First prune ttl-expired stuff
                     (when ttl?
                       (cache_
                         (fn swap-fn [m]
                           (persistent!
                             (reduce-kv
                               (fn [acc k ^TickedCacheEntry e]
                                 (if (> (- instant (.-udt e)) ttl-ms)
                                   (dissoc! acc k)
                                   acc))
                               (transient (or m {}))
                               m)))))

                     ;; Then prune by ascending (worst) tick-sum:
                     (let [snapshot (cache_)
                           n-to-gc  (- (count snapshot) size)]

                       (when (>= n-to-gc (* 0.1 size))
                         (let [ks-to-gc
                               (top n-to-gc
                                 (fn [k]
                                   (let [e ^TickedCacheEntry (get snapshot k)]
                                     (+ (.-tick-lru e) (.-tick-lfu e))))
                                 (keys snapshot))]

                           (cache_
                             (fn swap-fn [m]
                               (persistent!
                                 (reduce (fn [acc in] (dissoc! acc in))
                                   (transient (or m {})) ks-to-gc)))))))

                     #?(:clj
                        (do
                          (reset!     latch_ nil)
                          (.countDown latch)))

                     nil)))

               (let [fresh? (case a1 (:cache/fresh :mem/fresh) true false)
                     args   (if fresh? (next args) args)

                     _ #?(:clj (when-let [l (latch_)] (.await ^CountDownLatch l)) :default nil)
                     ^TickedCacheEntry e
                     (cache_ args
                       (fn swap-fn [?e]
                         (if (or (nil? ?e) fresh?
                                 (> (- instant (.-udt ^TickedCacheEntry ?e)) ttl-ms))
                           (TickedCacheEntry. (delay (apply f args)) instant tick 1)
                           (let [e ^TickedCacheEntry ?e]
                             (TickedCacheEntry. (.-delay e) (.-udt e)
                               tick (inc (.-tick-lfu e)))))))]

                 @(.-delay e)))))))

     ttl-ms ; De-raced, commands, ttl, gc
     (let [gc-now? gc-now?
           cache_  (latom nil) ; {<args> <SimpleCacheEntry>}
           latch_  (latom nil) ; Used to pause writes during gc
           ttl-ms  (long ttl-ms)
           gc-rate
           (let [gce (or gc-every 8e3)]
             (/ 1.0 (long gce)))]

       (fn cached [& args]
         (let [a1 (first args)]
           (case a1
             (:cache/del :mem/del)
             (let [argn (next  args)
                   a2   (first argn)]
               (if (case a2 (:cache/all :mem/all) true false)
                 (reset! cache_ nil)
                 (cache_ #(dissoc % argn)))
               nil)

             (let [instant (now-udt)]

               (when (gc-now? gc-rate)
                 (let [latch #?(:clj (CountDownLatch. 1) :default nil)]
                   (when (-cas!? latch_ nil latch)
                     (cache_
                       (fn swap-fn [m]
                         (persistent!
                           (reduce-kv
                             (fn [acc k ^SimpleCacheEntry e]
                               (if (> (- instant (.-udt e)) ttl-ms)
                                 (dissoc! acc k)
                                 acc))
                             (transient (or m {}))
                             m))))

                     #?(:clj
                        (do
                          (reset!     latch_ nil)
                          (.countDown latch)))

                     nil)))

               (let [fresh? (case a1 (:cache/fresh :mem/fresh) true false)
                     args   (if fresh? (next args) args)
                     _      #?(:clj (when-let [l (latch_)] (.await ^CountDownLatch l)) :default nil)
                     ^SimpleCacheEntry e
                     (cache_ args
                       (fn swap-fn [?e]
                         (if (or (nil? ?e) fresh?
                                 (> (- instant (.-udt ^SimpleCacheEntry ?e)) ttl-ms))
                           (SimpleCacheEntry. (delay (apply f args)) instant)
                           ?e)))]
                 @(.-delay e)))))))

     :else (cache f))))

(defn memoize
  "Alternative way to call `cache`, provided mostly for back compatibility.
  See `cache` docstring for details."
  ;; {:deprecated "Encore v3.36.0 (2022-11-18)"}
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

     {:added "Encore v3.36.0 (2022-11-18)"}
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

;;;; Rate limits

(deftype LimitSpec  [^long n ^long ms])
(deftype LimitEntry [^long n ^long udt0])
(deftype LimitHits  [m worst-lid ^long worst-ms])

(let [limit-spec (fn [n ms] (have? pos-int? n ms) (LimitSpec. n ms))]
  (defn- coerce-limit-spec [x]
    (cond
      (map?    x) (reduce-kv (fn [acc lid [n ms]] (assoc acc lid (limit-spec n ms))) {} x)
      (vector? x)
      (let [i (counter)]
        (reduce
          (fn [acc [n ms ?id]] ; ?id for back compatibility
            (assoc acc (or ?id (i)) (limit-spec n ms))) {} x))

      (unexpected-arg! x
        {:context  `rate-limiter
         :param    'rate-limiter-spec
         :expected '#{map vector}}))))

(comment (qb 1e6 (coerce-limit-spec [[10 1000] [20 2000]])))

(defn ^:no-doc rate-limiter*
  "Private, don't use.
  Like `rate-limiter` but returns [<state_> <rate-limiter>]."
  ([     spec] (rate-limiter* nil spec))
  ([opts spec]
   (if (empty? spec)
     [nil (constantly nil)]
     (let [latch_ (latom nil) ; Used to pause writes during gc
           reqs_  (latom nil) ; {<rid> {<lid> <LimitEntry>}}
           spec   (coerce-limit-spec spec) ; {<lid> <LimitSpec>}

           {:keys [gc-every] :or {gc-every 1.6e4}} opts ; Undocumented

           gc-now? gc-now?
           gc-rate (let [gce (long gc-every)] (/ 1.0 gce))

           f1
           (fn [rid peek?]
             (let [instant (now-udt)]

               (when (and (not peek?) (gc-now? gc-rate))
                 (let [latch #?(:clj (CountDownLatch. 1) :default nil)]
                   (when (-cas!? latch_ nil latch)
                     (reqs_
                       (fn swap-fn [reqs] ; {<rid> <entries>}
                         (persistent!
                           (reduce-kv
                             (fn [acc rid entries]
                               (let [new-entries
                                     (reduce-kv
                                       (fn [acc lid ^LimitEntry e]
                                         (if-let [^LimitSpec s (get spec lid)]
                                           (if (>= instant (+ (.-udt0 e) (.-ms s)))
                                             (dissoc acc lid)
                                             (do     acc))
                                           (dissoc acc lid)))
                                       entries ; {<lid <LimitEntry>}
                                       entries)]
                                 (if (empty? new-entries)
                                   (dissoc! acc rid)
                                   (assoc!  acc rid new-entries))))
                             (transient (or reqs {}))
                             (do            reqs)))))

                     #?(:clj
                        (do
                          (reset!     latch_ nil)
                          (.countDown latch)))

                     nil)))

               ;; Need to atomically check if all limits pass before
               ;; committing to any n increments:
               (loop []
                 (let [reqs        (reqs_)    ; {<lid> <entries>}
                       entries (get reqs rid) ; {<lid> <LimitEntry>}
                       ?hits                  ; ?LimitHits
                       (if (nil? entries)
                         nil
                         (reduce-kv
                           (fn [^LimitHits acc lid ^LimitEntry e]
                             (if-let [^LimitSpec s (get spec lid)]
                               (if (< (.-n e) (.-n s))
                                 acc
                                 (let [tdelta (- (+ (.-udt0 e) (.-ms s)) instant)]
                                   (if (<= tdelta 0)
                                     acc
                                     (cond
                                       (nil? acc) (LimitHits. {lid tdelta} lid tdelta)

                                       (> tdelta (.-worst-ms acc))
                                       (LimitHits. (assoc (.-m acc) lid tdelta) lid tdelta)

                                       :else
                                       (LimitHits. (assoc (.-m acc) lid tdelta)
                                         (.-worst-lid acc)
                                         (.-worst-ms  acc))))))
                               acc))
                           nil
                           entries))]

                   (if (or peek? ?hits)
                     ;; No action (peeking, or hit >= 1 spec)
                     (when-let [^LimitHits h ?hits]
                       [(.-worst-lid h) (.-worst-ms h) (.-m h)])

                     ;; Passed all limits, ready to commit increments:
                     (if-let [l (latch_)]
                       #?(:clj (do (.await ^CountDownLatch l) (recur)) :default nil)
                       (let [new-entries
                             (reduce-kv
                               (fn [acc lid ^LimitSpec s]
                                 (assoc acc lid
                                   (if-let [^LimitEntry e (get entries lid)]
                                     (let [udt0 (.-udt0 e)]
                                       (if (>= instant (+ udt0 (.-ms s)))
                                         (LimitEntry. 1 instant)
                                         (LimitEntry. (inc (.-n e)) udt0)))
                                     (LimitEntry. 1 instant))))
                               entries
                               spec)]

                         (if (-cas!? reqs_ reqs (assoc reqs rid new-entries))
                           nil
                           (recur)))))))))]

       [reqs_
        (fn rate-limiter
          ([          ] (f1 nil    false))
          ([    req-id] (f1 req-id false))
          ([cmd req-id]
           (case cmd
             (:rl/reset :limiter/reset)
             (do
               (if (case req-id (:rl/all :limiter/all) true false)
                 (reset! reqs_ nil)
                 (reqs_ #(dissoc % req-id)))
               nil)

             (:rl/peek :limiter/peek) (f1 req-id true)

             (unexpected-arg! cmd
               {:context  `rate-limiter
                :param    'rate-limiter-command
                :expected #{:rl/reset :rl/peek}
                :req-id   req-id}))))]))))

(defn rate-limiter
  "Takes a map spec of form {<limit-id> [<n-max-reqs> <msecs-window>]},
  and returns a basic stateful (fn rate-limiter [req-id] [command req-id]).

  Call fn with a single request id (e.g. username) by which to count/limit.
  Will return:
    - nil when all limits pass for that id, or
    - [<worst-limit-id> <worst-backoff-msecs> {<limit-id> <backoff-msecs>}]
      when any limits fail for that id.

  Or call fn with an additional command argument:
    `:rl/peek`  <req-id> - Check limits w/o incrementing count.
    `:rl/reset` <req-id> - Reset all limits for given req-id.

  Example:

    (defonce my-rate-limiter
      (rate-limiter
        {\"1  per sec\" [1   1000]
         \"10 per min\" [10 60000]}))

    (defn send-message! [username msg-content]
      (if-let [fail (my-rate-limiter username)]
        (throw (ex-info \"Sorry, rate limited!\" {:fail fail}))
        <send message>))"

  ([     spec] (let [[_ f] (rate-limiter* nil  spec)] f))
  ([opts spec] (let [[_ f] (rate-limiter* opts spec)] f)))

(comment
  (let [[s_ rl1] (rate-limiter* {:2s [1 2000] :5s [2 5000]})]
    (def s_  s_)
    (def rl1 rl1))

  (qb 1e6 (rl1)) ; 122.96
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
  (let [t1 (now-udt)
        ^long n-skip0  (n-skip_)
        ts             (ts_)
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
    (when (<              n-skip0 n-skip1)
      (if (-cas!? n-skip_ n-skip0 n-skip1)
        (when (> n-skip1 10000) ; Time to gc, amortised cost
          (gc-fn n-skip1))))

    n-window))

#?(:clj
   (deftype RollingCounter [^long msecs ts_ n-skip_ p_]
     clojure.lang.IFn
     (invoke [this]
       (when-let [p (p_)] @p) ; Block iff latched
       (let [t1 (now-udt)] (ts_ #(conj % t1)))
       this ; Return to allow optional deref
       )

     clojure.lang.IDeref
     (deref [_]
       (when-let [p (p_)] @p) ; Block iff latched
       (rc-deref msecs ts_ n-skip_
         (fn gc [n-skip1]
           (let [p (promise)]
             (if (-cas!? p_ nil p) ; Latch
               (do
                 (ts_ #(subvec % n-skip1))
                 (reset!  n-skip_ 0)
                 (reset!  p_ nil)
                 (deliver p  nil))))))))

   :cljs
   (deftype RollingCounter [^long msecs ts_ n-skip_]
     IFn
     (-invoke [this]
       (let [t1 (now-udt)] (ts_ #(conj % t1)))
       this ; Return to allow optional deref
       )

     IDeref
     (-deref [_]
       (rc-deref msecs ts_ n-skip_
         (fn gc [n-skip1]
           (ts_ #(subvec % n-skip1))
           (reset! n-skip_ 0))))))

(defn rolling-counter
  "Experimental. Returns a RollingCounter that you can:
    - Invoke to increment count in last `msecs` window and return RollingCounter.
    - Deref  to return    count in last `msecs` window."
  [msecs]
  (RollingCounter.
    (long (have pos-int? msecs))
    (latom [])
    (latom 0)
    #?(:clj (latom nil))))

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

  {:added "Encore v3.45.0 (2022-12-13)"}
  ([nmax                          ] (rolling-vector nmax nil))
  ([nmax {:keys [gc-every init-val]
          :or   {gc-every 16e3}}]

   (let [nmax     (long nmax)
         acc_     (latom (vec init-val))
         gc-every (when gc-every (long gc-every))
         ticker   (when gc-every (counter))
         latch_   (when gc-every (latom nil))]

     (fn rolling-vec-fn
       ([ ] (acc_))
       ([x]
        (when gc-every
          #?(:clj (when-let [l (latch_)] (.await ^CountDownLatch l)))

          (let [^long tick (ticker)]
            (when-let [gc-now? (== (rem tick ^long gc-every) 0)]
              #?(:cljs (acc_ (fn swap-fn [sv] (into [] sv)))
                 :clj
                 (let [latch (CountDownLatch. 1)]
                   (when (-cas!? latch_ nil latch)
                     (acc_ (fn swap-fn [sv] (into [] sv)))
                     (reset!     latch_ nil)
                     (.countDown latch)))))))

        (acc_
          (fn swap-fn [acc]
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

     {:added "Encore v3.45.0 (2022-12-13)"}
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

(def* ^:const newline  "Single system newline" {:added "Encore v3.68.0 (2023-09-25)"} #?(:cljs "\n" :clj (System/getProperty "line.separator")))
(def* ^:const newlines "Double system newline" {:added "Encore v3.68.0 (2023-09-25)"} (str newline newline))

(defn print1
  "Prints given argument as string, and flushes output stream."
  {:added "Encore v3.68.0 (2023-09-25)"}
  [x]
  #?(:cljs (print (str x))
     :clj
     (let [out *out*]
       (.append out (str x))
       (.flush  out)
       nil)))

(defn println-atomic
  "Like `core/println` but won't interleave content from different threads."
  {:added "Encore v3.68.0 (2023-09-25)"}
  [x]
  #?(:cljs (println x)
     :clj
     (let [sw  (java.io.StringWriter.)
           out *out*]
       (binding [*print-readably* nil]
         (print-method x sw))
       (.append out (str sw newline))
       (when *flush-on-newline*
         (.flush out))
       nil)))

(defn str-builder?
  #?(:cljs {:tag 'boolean})
  [x]
  #?(:clj  (instance?             StringBuilder x)
     :cljs (instance? goog.string.StringBuffer  x)))

(defn str-builder
  "Returns a new stateful string builder:
    - `java.lang.StringBuilder`  for Clj
    - `goog.string.StringBuffer` for Cljs

  See also `sb-append`."
  #?(:clj  (^StringBuilder [                ] (StringBuilder.)))
  #?(:cljs (               [                ] (goog.string.StringBuffer.)))
  #?(:clj  (^StringBuilder [^String init-str] (StringBuilder.            init-str)))
  #?(:cljs (               [^String init-str] (goog.string.StringBuffer. init-str))))

(defn sb-append
  "Appends given string to given string builder. See also `str-builder.`"
  #?(:clj  (^StringBuilder [^StringBuilder str-builder s] (.append str-builder s))
     :cljs (               [               str-builder s] (.append str-builder s)))
  ([str-builder s & more]
   (reduce (fn [acc in] (sb-append acc in))
     (sb-append str-builder s)
     more)))

(comment (str (sb-append (str-builder "a") "b" "c" \d)))

(defn str-rf
  "String builder reducing fn."
  ([      ] (str-builder))
  ([acc   ]            (if (str-builder? acc) acc (str-builder (str acc)))) ; cf
  ([acc in] (sb-append (if (str-builder? acc) acc (str-builder (str acc))) (str in))))

(comment
  (qb 1e3 ; [358.45 34.6]
         (reduce str    (range 512))
    (str (reduce str-rf (range 512)))))

(defn str-join
  "Faster, transducer-based generalization of `clojure.string/join` with `xform` support."
  {:tag #?(:clj 'String :cljs 'string)}
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
       (str (reduce str-rf coll))))))

(comment
  (qb 1e5
    (str/join "," ["a" "b" "c" "d"])
    (str-join "," ["a" "b" "c" "d"])
    (str-join ""  ["a" "b" "c" "d"])) ; [29.37 23.63 13.34]
  (str-join "," (comp (filter #{"a" "c"}) (map str/upper-case)) ["a" "b" "c"]))

(defn str-contains?
  #?(:cljs {:tag 'boolean})
  [s substr]
  #?(:clj  (.contains ^String s ^String substr)
     :cljs (not= -1 (.indexOf s substr))))

(defn str-starts-with?
  #?(:cljs {:tag 'boolean})
  [s substr]
  #?(:clj  (.startsWith ^String s ^String substr)
     :cljs (zero? (.indexOf s substr))))

(defn str-ends-with?
  #?(:cljs {:tag 'boolean})
  [s substr]
  #?(:clj (.endsWith ^String s ^String substr)
     :cljs
     (let [s-len      (.-length s)
           substr-len (.-length substr)]
       (when (>= s-len substr-len)
         (not= -1 (.indexOf s substr (- s-len substr-len)))))))

(defn str-?index
  "Returns (first/last) ?index of substring if it exists within given string."
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

(defn case-insensitive-str=
  "Returns true iff given strings are equal, ignoring case."
  ;; Implementation detail: compares normalized chars 1 by 1, so often faster
  ;; than naive comparison of normalized strings.
  {:tag #?(:cljs 'boolean :default nil)
   :added "Encore v3.25.0 (2022-10-13)"}
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
   (defn ^String norm-str
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

  Workaround for <http://dev.clojure.org/jira/browse/CLJS-794>,
                 <http://dev.clojure.org/jira/browse/CLJS-911>.

  Note that ClojureScript 1.7.145 introduced a partial fix for CLJS-911.
  A full fix could unfortunately not be introduced w/o breaking compatibility
  with the previously incorrect behaviour. CLJS-794 also remains unresolved."
  {:tag #?(:clj 'String :cljs 'string)}
  [s match replacement]
  #?(:clj (str/replace s match replacement)
     :cljs
     (cond
       (string? match) ; string -> string replacement
       (.replace s (js/RegExp. (gstr/regExpEscape match) "g") replacement)
       ;; (.hasOwnProperty match "source") ; No! Ref. <http://goo.gl/8hdqxb>

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
  (defn ^:no-doc nil->str [x] (if (nil? x) "nil" x)) ; (undefined? x) check no longer needed for modern Cljs

  (defn format*
    {:tag #?(:clj 'String :cljs 'string)}
    (#?(:clj  [      fmt args]
        :cljs [      fmt args]) (format* nil->str fmt args))
    (#?(:clj  [xform fmt args]
        :cljs [xform fmt args])
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
    {:tag #?(:clj 'String :cljs 'string)}
    [fmt & args] (format* fmt args)))

(defn str-join-once
  "Like `string/join` but skips nils and duplicate separators."
  {:tag #?(:clj 'String :cljs 'string)}
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

(defn path
  {:tag #?(:clj 'String :cljs 'string)}
  [& parts] (str-join-once "/" parts))

(comment (path "foo/" nil "/bar" "baz/" "/qux/"))

(defn norm-word-breaks
  "Converts all word breaks of any form and length (including line breaks of any
  form, tabs, spaces, etc.) to a single regular space."
  {:tag #?(:clj 'String :cljs 'string)}
  [s] (str/replace (str s) #"\s+" \space))

(defn count-words [s] (if (str/blank? s) 0 (count (str/split s #"\s+"))))
(comment (count-words "Hello this is a    test"))

(defn uuid
  "For Clj: returns a random `java.util.UUID`.
  For Cljs: returns a random UUID string.

  Uses strong randomness when possible.
  See also `uuid-str`, `nanoid`, `rand-id-fn`."
  {:added "Encore v3.75.0 (2024-01-29)"
   :inline #?(:default nil :clj (fn [] `(java.util.UUID/randomUUID)))}
  #?(:clj          (^java.util.UUID []  (java.util.UUID/randomUUID))
     :cljs
     ([]
      (if-let [f (oget js-?crypto "randomUUID")]
        (.call f js-?crypto)
        (let [^string quad-hex
              (fn []
                (let [unpadded-hex ^string (.toString (rand-int 65536) 16)]
                  (case (count   unpadded-hex)
                    1 (str "000" unpadded-hex)
                    2 (str "00"  unpadded-hex)
                    3 (str "0"   unpadded-hex)
                      (do        unpadded-hex))))

              ver-trip-hex ^string (.toString (bit-or 0x4000 (bit-and 0x0fff (rand-int 65536))) 16)
              res-trip-hex ^string (.toString (bit-or 0x8000 (bit-and 0x3fff (rand-int 65536))) 16)]

          (str (quad-hex) (quad-hex) "-" (quad-hex) "-" ver-trip-hex "-" res-trip-hex "-"
               (quad-hex) (quad-hex) (quad-hex)))))))

(defn uuid-str
  "Returns a random UUID string of given length (max 36).
  Uses strong randomness when possible. See also `uuid`, `nanoid`, `rand-id-fn`."
  {:tag #?(:clj 'String :cljs 'string)}
  ;; 128 bits of entropy with default length (36)
  ([max-len] (get-substr-by-len (uuid-str) 0 max-len))
  ([       ]
   #?(:clj (str (uuid))
      :cljs     (uuid))))

(comment (qb 1e6 (uuid-str 5)))

(defn into-str
  "Simple Hiccup-like string templating to complement Tempura."
  {:tag #?(:clj 'String :cljs 'string)}
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

(defn abbreviate-ns
  "Give any nameable type (string, keyword, symbol), returns the same
  type with at most `n-full` (default 1) unabbreviated namespace parts.

  Example:
    (abbreviate-ns 0  :foo.bar/baz)   => :f.b/baz
    (abbreviate-ns 1  'foo.bar/baz)   => 'f.bar/baz
    (abbreviate-ns 2 \"foo.bar/baz\") => \"foo.bar/baz\""

  {:added "Encore v3.68.0 (2023-09-25)"}
  ([       x] (abbreviate-ns 1 x))
  ([n-full x]
   (let [n-full (long (have nat-int? n-full))
         [p1 p2] (str/split (as-qname x) #"/")]
     (if-not p2
       x
       (let [name-part p2
             ns-parts  (str/split p1 #"\.")
             n-to-abbr (- (count ns-parts) n-full)
             sb
             (reduce-indexed
               (fn [sb ^long idx #?(:clj ^String in :cljs ^string in)]
                 (when-not (zero? idx) (sb-append sb "."))
                 (if (< idx n-to-abbr)
                   (sb-append sb (.substring in 0 1))
                   (sb-append sb             in)))
               (str-builder)
               ns-parts)]

         (sb-append sb "/")
         (sb-append sb name-part)

         (let [s (str sb)]
           (cond
             (keyword? x) (keyword s)
             (symbol?  x) (symbol  s)
             :else                 s)))))))

;;;; Signals (opt-in Telemere integration)

(def* ^:const have-telemere?
  "Is `taoensso.telemere` present (not necessarily loaded)?"
  {:added "Encore v3.68.0 (2023-09-25)"}
  (compile-if (io/resource "taoensso/telemere.cljc") true false))

#?(:clj
   (defmacro require-telemere-if-present
     "Experimental, subject to change without notice!
     Requires Telemere if it's present, otherwise noops.
     For Cljs: needs ClojureScript >= v1.9.293, and must be placed at top of file.
     Used in cooperation with `signal!`."
     {:added "Encore v3.68.0 (2023-09-25)"}
     [] (when have-telemere? `(require 'taoensso.telemere))))

(comment (require-telemere-if-present))

#?(:clj
   (defmacro signal!
     "Experimental, subject to change without notice!
     Expands to Telemere signal call if Telemere is present, otherwise expands
     to `fallback`.

     MUST be used with `require-telemere-if-present`:

       (ns my-ns (:require [taoensso.encore :as enc]))
       (encore/require-telemere-if-present) ; At top of file, just below `ns` form

       (encore/signal! {<signal-opts> :fallback (println \"Prints iff Telemere not present\")})

     For info on signal options, see:
       - Telemere documentation,                 Ref. <https://tinyurl.com/za3bmeub>
       - `taoensso.telemere/signal!` docstring,  Ref. <https://tinyurl.com/2r8ba98d>"

     {:added "Encore v3.68.0 (2023-09-25)"
      :tag #?(:cljs 'boolean :default nil)
      :arglists
      '([{:as opts
          :keys
          [fallback,
           #_defaults elidable? location instant uid middleware,
           sample-rate ns kind id level filter when rate-limit,
           ctx parent trace?, let data msg error run & user-opts]}])}

     [opts]
     (have? map? opts)
     (if have-telemere?
       (do
         (do    (require 'taoensso.telemere)) ; For macro expansion
         (keep-callsite `(taoensso.telemere/signal! ~(dissoc opts :fallback))))

       `(let  ~(get opts :let []) ; Currently undocumented
          (do ~(get opts :fallback))))))

(comment
  (macroexpand
    '(signal! {:level :warn :let [x :x] :msg ["Test" "message" x] :data {:a :A :x x}
               :fallback (println ["fallback" x])})))

;;;; Thread locals

#?(:clj
   (defmacro thread-local-proxy "Low-level, see `thread-local` instead."
     [& body] `(proxy [ThreadLocal] [] (initialValue [] (do ~@body)))))

#?(:clj
   (defn thread-local*
     "Low-level, see `thread-local` instead."
     {:added "Encore v3.48.0 (2023-01-25)"}
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
     {:added "Encore v3.48.0 (2023-01-25)"}
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
   (deftype ReseedingSRNG [^java.security.SecureRandom srng ^:volatile-mutable ^long call-count]
     clojure.lang.IFn
     (invoke [_]
       ;; Regularly supplement seed for extra security
       (if  (< call-count 1024)
         (set! call-count (unchecked-inc call-count))
         (do
           (set! call-count 0)
           (.setSeed srng (.generateSeed srng 8))))
       srng)))

#?(:clj
   (defn ^:no-doc reseeding-srng
     "Private, don't use. Returns a new stateful `ReseedingSRNG`."
     {:added "Encore v3.75.0 (2024-01-29)"}
     ^ReseedingSRNG []
     (compile-if      #(java.security.SecureRandom/getInstanceStrong) ; Java 8+, blocking
       (ReseedingSRNG. (java.security.SecureRandom/getInstanceStrong)      0)
       (ReseedingSRNG. (java.security.SecureRandom/getInstance "SHA1SRNG") 0))))

(comment (let [rsrng (reseeding-srng)] (qb 1e6 (secure-rng) (rsrng)))) ; [47.98 26.09]

#?(:clj (def ^:private rsrng* (thread-local-proxy (reseeding-srng))))
#?(:clj
   (do
     (defn secure-rng
       "Returns a an auto-reseeding thread-local `java.security.SecureRandom`.
       Favours security over performance. May block while waiting on entropy!"
       ^java.security.SecureRandom [] ((.get ^ThreadLocal rsrng*)))

     (defn secure-rng-mock!!!
       "Returns **INSECURE** `java.security.SecureRandom` mock instance backed by
       a seeded deterministic `java.util.Random`. Useful for testing, etc."
       {:added "Encore v3.53.0 (2023-03-22)"}
       ^java.security.SecureRandom [long-seed]
       (let [long-seed    (long              long-seed)
             insecure-rng (java.util.Random. long-seed)]

         (proxy [java.security.SecureRandom] []
           (getAlgorithm [] (str "INSECURE deterministic, seed=" long-seed))
           (nextBytes [^bytes ba] (.nextBytes insecure-rng ba)))))))

(defn secure-rand-bytes
  "Returns a random byte array of given size. Uses strong randomness when possible."
  #?(:clj (^bytes [size] (let [ba (byte-array size)] (.nextBytes (secure-rng) ba) ba))
     :cljs
     ([size]
      (let [ba (js/Uint8Array. size)]
        (if-let [crypto js-?crypto]
          (.getRandomValues crypto ba)
          (dotimes [i size] (aset ba i (Math/floor (* 256 (js/Math.random))))))
        ba))))

(comment
  (qb  1e6 (secure-rand-bytes 21)) ; 1021.07
  (do (seq (secure-rand-bytes 21))))

(defn rand-id-fn
  "Returns a (fn rand-id []) that returns random id strings.
  Uses strong randomness when possible.

  Options include:
    `:chars`         - ∈ #{<string> :nanoid :alphanumeric :no-look-alikes ...}
    `:len`           - Length of id strings to generate
    `:rand-bytes-fn` - Optional (fn [size]) to return random byte array of given size

  See also `uuid-str`, `nano-id`."
  {:added "Encore v3.75.0 (2024-01-29)"}
  [{:keys [chars ^long len rand-bytes-fn]
    :or
    {chars         :nanoid
     len           21
     rand-bytes-fn secure-rand-bytes}}]

  (let [chars
        (case chars
          :alphanumeric    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"   ; 62 chars
          :nanoid          "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_" ; 64 chars
          :nanoid-readable "346789ABCDEFGHJKLMNPQRTUVWXYabcdefghijkmnpqrtwxyz"                ; 49 chars, no look-alikes
          :numbers         "0123456789"                                                       ; 10 chars
          :alpha-lowercase "abcdefghijklmnopqrstuvwxyz"                                       ; 26 chars
          :alpha-uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                                       ; 26 chars
          :hex-lowercase   "0123456789abcdef"                                                 ; 16 chars
          :hex-uppercase   "0123456789ABCDEF"                                                 ; 16 chars
          (have string? chars))

        nchars       (count chars)
        max-char-idx (dec  nchars)
        chars #?(:clj (.toCharArray (str chars)) :cljs (object-array chars))

        ;; (bit-and <rand-byte> <mask>) uniformly maps <rand-byte> (256 vals) to
        ;; the valid subset of <rand-char-idx> (<256) *without* bias. Downside is that
        ;; invalid char-idxs need to be discarded, hence the stepping mechanism and
        ;; need for extra bytes when nchars∤256.
        mask ; Set all bits except the most significant, Ref. <https://bit.ly/3dtYv73>
        (bit-and -1
          (dec
            (bit-shift-left 2
              (int (Math/floor (/ (Math/log (dec nchars)) (Math/log 2)))))))

        ;; Calculate size of random byte arrays to fill
        exp-bytes (double (/ (* mask len) nchars))
        stepn     (int (max 2 (Math/ceil (* 0.2 exp-bytes)))) ; Used iff step1 insufficient
        step1 ; Size of initial random byte array
        (int
          (if (zero? (int (mod 256 nchars)))
            len ; n|256 => no discarding (steps) needed
            (Math/ceil (* 1.2 exp-bytes))))]

    (fn rand-id []
      (let [sb (str-builder)]
        (loop [idx 0, max-idx (dec step1), ^bytes rand-bytes (rand-bytes-fn step1)]

          (let [possible-ch-idx (bit-and (aget rand-bytes idx) mask)]
            (when (<= possible-ch-idx max-char-idx)
              (.append sb (aget chars possible-ch-idx))))

          (cond
            (== (.length sb) len) (str sb)
            (== idx      max-idx) (recur 0 (dec stepn) (rand-bytes-fn stepn))
            :else (recur (unchecked-inc idx) max-idx rand-bytes)))))))

(comment
  (let [f0 nanoid
        f1 (rand-id-fn {:len 21, :chars :nanoid})
        f2 (rand-id-fn {:len 22, :chars :no-look-alikes})]
    (qb 1e6 (uuid-str) (f0) (f1) (f2))) ; [234.65 346.14 399.66 544.64]

  ;; Bits of entropy
  (/ (Math/log (Math/pow 16 32)) (Math/log 2)) ;                uuid:  128
  (/ (Math/log (Math/pow 64 21)) (Math/log 2)) ;          nanoid(21):  126
  (/ (Math/log (Math/pow 49 23)) (Math/log 2)) ; nanoid-readable(23): ~129
  )

(let [chars
      (let [s "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_"]
        #?(:clj (.toCharArray s) :cljs (object-array s)))]

  (defn nanoid
    "Returns a random \"Nano ID\" of given length, Ref. <https://github.com/ai/nanoid>.
    Uses strong randomness when possible. See also `uuid-str`, `rand-id-fn`."
    {:tag #?(:clj 'String :cljs 'string)}

    ;; Slightly faster, variable-length version of (rand-id-fn {:chars :nanoid}).
    ;; 126 bits of entropy with default length (21).
    ([         ] (nanoid 21))
    ([^long len]
     (let [sb (str-builder)
           ba (secure-rand-bytes len)
           max-idx (dec len)]

       (loop [idx 0]
         ;; nchars|256 so (bit-and <rand-byte> 0x3f) yields uniform distribution
         ;; of chars without need for stepping
         (.append sb (aget chars (bit-and (aget ba idx) 0x3f)))
         (when (< idx max-idx) (recur (unchecked-inc idx))))

       (str sb)))))

(comment (qb 1e6 (nanoid) (uuid-str))) ; [341.65 225.23]

;;;; Hex strings

#?(:clj
   (defn ident-hex-str
     "Returns hex string of given Object's `identityHashCode` (e.g. \"0x5eeb49f2\")."
     {:added "Encore v3.56.0 (2023-03-29)"}
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
         {:added "Encore v3.53.0 (2023-03-22)"}
         ^String [^bytes ba]
         (str (reduce (fn [sb b] (byte->char-pair sb b)) (StringBuilder.) ba))))

     (let [char-pair->byte
           (fn [[c1 c2]]
             (let [d1 (Character/digit ^char c1 16)
                   d2 (Character/digit ^char c2 16)]
               (unchecked-byte (+ (bit-shift-left d1 4) d2))))]

       (defn hex-str->ba
         "Returns hex string for given byte[]."
         {:added "Encore v3.53.0 (2023-03-22)"}
         ^bytes [^String s]
         (if (even? (count s))
           (byte-array (into [] (comp (partition-all 2) (map char-pair->byte)) s))
           (throw
             (ex-info "Invalid hex string (length must be even)"
               {:given {:value s, :type (type s)}})))))

     ;; TODO Any way to auto select fastest implementation?
     ;; Ref. <https://stackoverflow.com/a/58118078/1982742>
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
(defn ms
  "Returns ~number of milliseconds in period defined by given args."
  {:arglists '([opts] [& {:as opts :keys [years months weeks days hours mins secs msecs ms]}])}
  (^long [{:keys [years months weeks days hours mins secs msecs ms]}]
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

  (^long [k1 v1             ] (ms {k1 v1}))
  (^long [k1 v1 k2 v2       ] (ms {k1 v1, k2 v2}))
  (      [k1 v1 k2 v2 & more] (ms (reduce-kvs assoc {k1 v1 k2 v2} more))))

(defn secs
  "Returns ~number of seconds in period defined by given args."
  {:arglists '([opts] [& {:as opts :keys [years months weeks days hours mins secs msecs ms]}])}
  (^long [opts              ] (ms->secs (ms opts)))
  (^long [k1 v1             ] (ms->secs (ms {k1 v1})))
  (^long [k1 v1 k2 v2       ] (ms->secs (ms {k1 v1, k2 v2})))
  (      [k1 v1 k2 v2 & more] (ms->secs (ms (reduce-kvs assoc {k1 v1 k2 v2} more)))))

(comment
  (ms   :years 88 :months 3 :days 33)
  (secs :years 88 :months 3 :days 33))

#?(:clj
   (defmacro msecs
     "Macro version of `ms`."
     {:arglists '([opts] [& {:as opts :keys [years months weeks days hours mins secs msecs ms]}])}
     ([k1 v1 & more] `(msecs ~(reduce-kvs assoc {k1 v1} (vec more))))
     ([{:keys [years months weeks days hours mins secs msecs ms] :as opts}]
      (have? [:ks<= #{:years :months :weeks :days :hours :mins :secs :msecs :ms}] opts)
      (taoensso.encore/ms opts))))

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
           (if (identical-kw? locale :jvm-default)
             nil ; (Locale/getDefault)
             locale)

           timezone
           (if (identical-kw? timezone :jvm-default)
             nil ; (TimeZone/getDefault)
             (if (identical-kw? timezone :utc)
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

;;;; Locals

#?(:clj
   (defmacro ^:no-doc get-locals
     "Private, don't use."
     []
     (let [ks (reduce
                (fn [acc in]
                  (if (str-starts-with? (name in) "__") ; Hide privates
                    (do   acc)
                    (conj acc (without-meta in))))
                [] (keys &env))]
       `(zipmap '~ks ~ks))))

(comment
  [(let [x :x] (get-locals)) ((fn [^long x] (get-locals)) 0)
   (let [x :x] (get-env))    ((fn [^long x] (get-env))    0)])

;;;; IO

#?(:clj
   (defn slurp-resource
     "Returns slurped named resource on classpath, or nil when resource not found."
     [rname]
     (when-let [r (io/resource rname)]
       (try
         (slurp (io/reader r))
         (catch Exception e
           (throw (ex-info "[encore/slurp-resource] Slurp failed" {:rname rname} e)))))))

#?(:clj
   (defn get-file-resource-?last-modified
     "Returns last-modified time for file backing given named resource, or nil
     if file doesn't exist."
     [rname]
     (when-let [file (try (->> rname io/resource io/file) (catch Exception _))]
       (.lastModified ^java.io.File file))))

#?(:clj
   (def file-resources-modified?
     "Returns true iff any files backing given named resources have changed since last call."
     (let [udts_ (atom {}) ; {<rnames> <udts>}
           swap! (fn [ks v] (swap-in! udts_ ks (fn [?v] (swapped v (when (not= v ?v) v)))))
           sorted-set (fmemoize (fn [x] (into (sorted-set) x)))]

       (fn [rnames & [?scope]]
         (let [rgroup (sorted-set rnames)]
           (swap! [?scope rgroup]
             (mapv get-file-resource-?last-modified rgroup)))))))

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

;;;;

#?(:clj
   (defn get-pom-version
     "Returns POM version string for given Maven dependency, or nil."
     [dep-sym]
     (let [path
           (clojure.core/format "META-INF/maven/%s/%s/pom.properties"
             (or (namespace dep-sym)
                 (name      dep-sym))
             (name dep-sym))]

       (when-let [props (io/resource path)]
         (with-open [stream (io/input-stream props)]
           (let [props (doto (java.util.Properties.) (.load stream))]
             (.getProperty props "version")))))))

(comment (get-pom-version 'com.taoensso/encore))

#?(:clj
   (let [cache_ (atom nil)] ; Impln detail
     (defn java-version
       "Returns Java's major version integer (8, 17, etc.)."
       {:added "Encore v3.75.0 (2024-01-29)"}
       (^long [              ] (or @cache_ (reset! cache_ (java-version (System/getProperty "java.version")))))
       (^long [version-string]
        (or
          (when-let [^String s version-string]
            (catching
              (Integer/parseInt
                (or ; Ref. <https://stackoverflow.com/a/2591122>
                  (when     (.startsWith s "1.")                  (.substring s 2 3))    ; "1.6.0_23", etc.
                  (let [idx (.indexOf    s ".")] (when (pos? idx) (.substring s 0 idx))) ; "9.0.1",    etc.
                  (let [idx (.indexOf    s "-")] (when (pos? idx) (.substring s 0 idx))) ; "16-ea",    etc.
                  (do                                                         s)))))
          (throw
            (ex-info "Failed to parse Java version string (unexpected form)"
              {:version-string version-string})))))))

#?(:clj
   (defn java-version>=
     "Returns true iff Java's major version integer is >= given integer:
       (if (java-version>= 21) <then> <else>)"
     {:added "Encore v3.75.0 (2024-01-29)"}
     [n] (>= (java-version) (long n))))

(comment (java-version>= 21))

;;;; Env config API

;; Notes:
;;   - Cljs must embed config in code (no runtime access to environmental config).
;;   - Embed config => macro, no runtime arg support, locally eval all symbols.
;;   - Locally eval symbols => no support for foreign unrequired namespaces.

#?(:clj
   (let [pattern-platform #"\<(.)?platform(.)?\>"
         pattern-opt      #"\<(.+?)\>"]

     (defn- prep-env-ids
       "Handles id prep for `get-env`:
         :a<XplatformY><optional> =>
           [\"a.bXcljYoptional\" \"a.bXcljY\" \"a.boptional\" \"a.b\"], etc."
       [target tf x]
       (when x
         (if (vector? x)
           (into [] (comp (map #(prep-env-ids target tf %)) cat (distinct)) x)
           (let [tf (or tf identity)
                 s  (as-qname
                      (if (const-form? x)
                        x
                        (throw
                          (ex-info "[encore/get-env] Ids must be const forms"
                            {:id x}))))

                 without-platform                 (str/replace s pattern-platform "")
                 with-platform       (when target (str/replace s pattern-platform (fn [[_ pre post]] (str pre (name target) post))))
                 without-opt (fn [s] (when s      (str/replace s pattern-opt      "")))
                 with-opt    (fn [s] (when s      (str/replace s pattern-opt      (fn [[_ cnt]] cnt))))]

             (into [] (comp (filter identity) (distinct) (map tf))
               [(with-opt    with-platform)
                (without-opt with-platform)
                (with-opt    without-platform)
                (without-opt without-platform)])))))))

(comment (prep-env-ids :clj vector [:a<XplatformY><optional> :b<XplatformY><optional> :a]))

#?(:clj
   (let [pname (fn [s] (-> s (str-replace #"/" ".")))
         ename (fn [s] (-> s (str-replace #"[./-]" "_") (str/upper-case)))
         parse-opts
         (fn        [opts-or-spec]
           (if (map? opts-or-spec)
             (do     opts-or-spec)
             {:spec  opts-or-spec}))]

     (defn ^:no-doc get-env*
       "Private, don't use.
       Function version of `get-env`:
         - Doesn't embed, or eval symbols.
         - Supports runtime args."
       {:added "Encore v3.75.0 (2024-01-29)"
        :arglists
        '([{:keys [as default return]} spec]
          [{:keys [as default return   spec]
            :or   {as     :str
                   return :value}}])}

       ([opts    spec] (get-env* (assoc opts :spec spec)))
       ([opts-or-spec]
        (let [opts (parse-opts opts-or-spec)
              {:keys [as default spec, return macro-env]
               :or   {as     :str
                      return :value}} opts

              ;;; Advanced opts, undocumented
              target (get opts :target (if (:ns macro-env) :cljs :clj))
              custom-res
              (when            (get opts ::allow-recur? true)
                (let [res-prop (get opts :res-prop)
                      env-prop (get opts :env-prop)]
                  (when (or res-prop env-prop)
                    (get-env*
                      {::allow-recur? false,
                       :macro-env macro-env, :target target,
                       :prop res-prop, :env env-prop, :res nil,
                       :return :value}))))

              props     (prep-env-ids target (fn [id] [:prop (pname id)])                (get opts :prop spec))
              envs      (prep-env-ids target (fn [id] [:env  (ename id)])                (get opts :env  spec))
              ress      (prep-env-ids target (fn [id] [:res  (pname id)]) (or custom-res (get opts :res  spec)))
              to-search [props envs ress] ; (vinterleave-all [[:p1 :p2] [:e1] [:r1 :r2 :r3]]) ; => [:p1 :e1 :r1 :p2 :r2 :r3]

              match ; ?[source str-val]
              (or
                (get opts :debug/match)
                (reduce-interleave-all
                  (fn rf [_ in]
                    (let [[kind n] in]
                      (case kind
                        :prop (when-let [v (System/getProperty n)] (reduced [in v]))
                        :env  (when-let [v (System/getenv      n)] (reduced [in v]))
                        :res  (when-let [v (slurp-resource     n)] (reduced [in v])))))
                  nil to-search))

              match-as ; ?[source as-val]
              (or
                (case as
                  :str match ; ?[source str-val]
                  :bool
                  (when-let [[source bool-str] match]
                    (let [parsed-bool
                          (case bool-str
                            ("true"  "1" "t" "T" "TRUE")  true
                            ("false" "0" "f" "F" "FALSE") false
                            (throw
                              (ex-info "[encore/get-env*] Error parsing as boolean"
                                {:bool-str bool-str
                                 :source   source
                                 :target   target
                                 :expected
                                 {true  #{"true"  "1" "t" "T" "TRUE"}
                                  false #{"false" "0" "f" "F" "FALSE"}}})))]
                      [source parsed-bool]))

                  :edn
                  (when-let [[source edn] match]
                    (let [x
                          (try
                            (read-edn (get opts :read-opts) edn)
                            (catch Throwable t
                              (throw
                                (ex-info "[encore/get-env*] Error reading as edn"
                                  {:edn edn :source source :target target} t))))]

                      ;; Allow `get-env` in ns1 to include lone sym from unrequired ns2.
                      ;; Useful when ns1 is a library, and ns2 is a user namespace.
                      ;;
                      ;; Undocumented, kept only for back-compatibility (Timbre, etc.).
                      ;; Better to avoid this pattern since Cljs can't support it
                      ;; even after CLJS-1346.
                      (when (and (symbol? x) (get opts :require-sym-ns? (= target :clj)))
                        (when-let [clj-ns (namespace x)]
                          (catching (require (symbol clj-ns)))))

                      [source x]))

                  (unexpected-arg! as
                    {:context  `get-env*
                     :param    'as
                     :expected #{:str :edn :bool}}))

                (when (contains? opts :default)
                  [:default default]))]

          (when (or match-as (= return :debug))
            (let [[source value] match-as]
              (case return
                :value                      value
                :legacy            {:config value, :source source} ; Back compatibility
                :map   (assoc-some {:value  value, :source source} :target target)
                :debug (assoc-some {:value  value, :source source} :target target, :search (vinterleave-all to-search))
                (unexpected-arg! return
                  {:context  `get-env*
                   :param    'return
                   :expected #{:value :map}})))))))

     (defmacro get-env
       "Cross-platform util for embedding flexible environmental config during
       macro expansion. Used by other Taoensso libraries.

       Given a const kw/string id or vector of desc-priority alternative ids,
       parse and return the first of the following that exists:
         - JVM         property value   for id (\"prop\")
         - Environment variable value   for id (\"env\")
         - Classpath   resource content for id (\"res\")

       Ids may include optional segment in `<>` tag (e.g. `<.edn>`).
       Ids may include `<.?platform.?>` tag for auto replacement, useful
       for supporting platform-specific config.

       Search order: desc by combined [alt-index platform(y/n) optional(y/n)].

       (get-env {:as :edn} [:my-app/alt1<.platform><.edn> :my-app/alt2])
       will parse and return the first of the following that exists:

         1. Alt1 +platform +optional (content type)
           1a. `my-app.alt1.clj.edn` JVM         property value
           1b. `MY_APP_ALT1_CLJ_EDN` environment variable value
           1c. `my-app.alt1.clj.edn` classpath   resource content

         2. Alt1 +platform -optional (content type)
           2a. `my-app.alt1.clj`     JVM         property value
           2b. `MY_APP_ALT1_CLJ`     environment variable value
           2c. `my-app.alt1.clj`     classpath   resource content

         3. Alt1 -platform +optional (content type)
           3a. `my-app.alt1.edn`     JVM         property value
           3b. `MY_APP_ALT1_EDN`     environment variable value
           3c. `my-app.alt1.edn`     classpath   resource content

         4. Alt1 -platform -optional (content type)
           4a. `my-app.alt1`         JVM         property value
           4b. `MY_APP_ALT1`         environment variable value
           4c. `my-app.alt1`         classpath   resource content

         5. Alt2
           5a. `my-app.alt2`         JVM         property value
           5b. `MY_APP_ALT2`         environment variable value
           5c. `my-app.alt2`         classpath   resource content

       Options:
         `:as`      - Parse found value as given type ∈ #{:str :bool :edn} (default :str).
         `:default` - Fallback to return if no value found during search (default nil).
         `:return`  - Return type ∈ #{:value :map :debug} (default :value).
                      TIP: Use `:debug` to inspect/verify search behaviour!

       Result must be something that can be safely embedded in code during
       macro-expansion. Symbols in edn will be evaluated during expansion."
       {:added "Encore v3.75.0 (2024-01-29)"
        :arglists
        '([{:keys [as default return]} spec]
          [{:keys [as default return   spec]
            :or   {as     :str
                   return :value}}])}

       ([            ] `(get-locals)) ; Back compatibility
       ([opts    spec] (have? const-form? opts    spec) (get-env* (assoc             opts          :macro-env &env :spec spec)))
       ([opts-or-spec] (have? const-form? opts-or-spec) (get-env* (assoc (parse-opts opts-or-spec) :macro-env &env))))))

(comment
  (def myvar "myvar")
  (do           (get-env {:as :edn, :return :debug} [:a.b/c<.platform><.edn>]))
  (macroexpand '(get-env {:as :edn, :return :debug, :default "my-default"}                                     ::nx))
  (macroexpand '(get-env {:as :edn, :return :debug, :debug/match [:debug/source "{:x taoensso.encore/myvar}"]} ::nx))
  (macroexpand '(get-env {:as :edn, :return :debug, :debug/match [:debug/source "{:x taoensso.encore/nx}"]}    ::nx))
  (defn foo [x] (get-env {:spec [:const x]})))

;;;; Async

#?(:clj
   (defmacro ^:no-doc threaded "Private, don't use."
     [& body] `(doto (Thread. (fn [] ~@body)) (.setDaemon true) (.start))))

(comment (threaded (println "Runs on daemon thread")))

#?(:clj
   (defn virtual-executor
     "Experimental, subject to change without notice!
     Returns new virtual `java.util.concurrent.ThreadPerTaskExecutor` when
     possible (JVM 21+), otherwise returns nil."
     {:added "Encore v3.72.0 (2023-10-24)"}
     []
     (compile-if (Thread/ofVirtual)
       (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)
       nil)))

#?(:clj
   (let [ap    (fn []  (.availableProcessors (Runtime/getRuntime)))
         perc  (fn [n] (max 1 (long (Math/floor (* (/ (double (ap)) 100.0) (double n))))))
         ratio (fn [r] (max 1 (long (Math/floor (*    (double (ap)) (double r))))))]

     (defn- get-num-threads ^long [n-threads]
       (if (vector? n-threads)
         (let [[kind n] n-threads]
           (case kind
             :num   (long (have pos-int? n))
             :perc  (long (perc          n))
             :ratio (long (ratio         n))
             (unexpected-arg! kind
               {:context  `get-num-threads
                :param        'num-threads
                :expected '#{<pos-int> [:perc <percent>] [:ratio <ratio>]}})))
         (long (have pos-int? n-threads))))))

(comment
  (get-num-threads [:perc   90])
  (get-num-threads [:ratio 0.9]))

#?(:clj
   (defn pool-executor
     "Experimental, subject to change without notice!
     Returns new `java.util.concurrent.ThreadPoolExecutor` with given opts."
     {:added "Encore v3.72.0 (2023-10-24)"}
     ^java.util.concurrent.ThreadPoolExecutor
     [{:keys [n-threads n-min-threads n-max-threads thread-name-prefix
              daemon-threads? keep-alive-msecs queue-type queue-size]
       :or
       {n-threads          (+ 2 (get-num-threads [:ratio 1.0]))
        thread-name-prefix "com.taoensso/encore-pool-"
        daemon-threads?    true
        keep-alive-msecs   60000
        queue-type         :linked}}]

     (let [n-min-threads    (int (or n-min-threads n-threads))
           n-max-threads    (int (or n-max-threads n-threads))
           keep-alive-msecs (int keep-alive-msecs)
           queue
           (case queue-type
             :array (java.util.concurrent.ArrayBlockingQueue. (int queue-size))
             :linked
             (if queue-size
               (java.util.concurrent.LinkedBlockingQueue. (int queue-size))
               (java.util.concurrent.LinkedBlockingQueue.))
             (unexpected-arg! queue-type
               {:expected #{:array :linked}
                :context `thread-pool}))

           factory
           (let [idx* (java.util.concurrent.atomic.AtomicInteger. 0)]
             (reify java.util.concurrent.ThreadFactory
               (newThread [_ runnable]
                 (let [idx (.incrementAndGet idx*)
                       t   (Thread. runnable (str thread-name-prefix idx))]
                   (when daemon-threads?
                     (.setDaemon t daemon-threads?))
                   t))))]

       (java.util.concurrent.ThreadPoolExecutor.
         (int n-min-threads)
         (int n-max-threads)
         (int keep-alive-msecs)
         java.util.concurrent.TimeUnit/MILLISECONDS
         ^java.util.concurrent.BlockingQueue queue
         ^java.util.concurrent.ThreadFactory factory))))

(comment (pool-executor {}))

#?(:clj
   (def* ^:no-doc ^:private default-executor_
     "Default `java.util.concurrent.ExecutorService`."
     {:added "Encore v3.72.0 (2023-10-24)"}
     (delay (or (virtual-executor) (pool-executor {})))))

#?(:clj
   (defn ^:no-doc binding-conveyor-fn
     "Private, don't use."
     {:added "Encore v3.72.0 (2023-10-24)"}
     [f]
     (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
       (fn
         ([            ] (clojure.lang.Var/resetThreadBindingFrame frame)       (f))
         ([x           ] (clojure.lang.Var/resetThreadBindingFrame frame)       (f x))
         ([x y         ] (clojure.lang.Var/resetThreadBindingFrame frame)       (f x y))
         ([x y z       ] (clojure.lang.Var/resetThreadBindingFrame frame)       (f x y z) )
         ([x y z & args] (clojure.lang.Var/resetThreadBindingFrame frame) (apply f x y z args))))))

#?(:clj
   (defn future-call*
     "Experimental, subject to change without notice!
     Like `future-call` but supports use of given custom
     `java.util.concurrent.ExecutorService`.

     Will default to using JVM 21+ virtual threads when possible,
     otherwise an unbounded fixed daemon thread pool.

     See also `future`, `virtual-executor`, `pool-executor`."
     {:added "Encore v3.72.0 (2023-10-24)"}
     ([                 f] (future-call* @default-executor_ f))
     ([executor-service f]
      (let [f   (binding-conveyor-fn f)
            fut (.submit ^java.util.concurrent.ExecutorService executor-service
                  ^Callable f)]

        (reify
          clojure.lang.IPending (isRealized [_] (.isDone fut))
          clojure.lang.IDeref   (deref      [_] (.get    fut))
          clojure.lang.IBlockingDeref
          (deref [_ timeout-msecs timeout-val]
            (try
              (.get fut timeout-msecs java.util.concurrent.TimeUnit/MILLISECONDS)
              (catch java.util.concurrent.TimeoutException _ timeout-val)))

          java.util.concurrent.Future
          (isDone      [_             ] (.isDone      fut))
          (isCancelled [_             ] (.isCancelled fut))
          (get         [_             ] (.get         fut))
          (get         [_ timeout unit] (.get         fut timeout unit))
          (cancel      [_ interrupt?  ] (.cancel      fut interrupt?)))))))

#?(:clj
   (defmacro future*
     "Experimental, subject to change without notice!
     Like `future` but supports use of given custom
     `java.util.concurrent.ExecutorService`.

     Will default to using JVM 21+ virtual threads when possible,
     otherwise an unbounded fixed daemon thread pool.

     See also `future-call`, `virtual-executor`, `pool-executor`."
     {:added "Encore v3.72.0 (2023-10-24)"}
     ([                 form] `(future-call*                   (^{:once true} fn* [] ~form)))
     ([executor-service form] `(future-call* ~executor-service (^{:once true} fn* [] ~form)))))

(comment @(future* (do (println "running") (Thread/sleep 2000) :done)))

#?(:clj
   (defn future-pool
     "Returns a simple semaphore-limited wrapper of Clojure's standard `future`:
       (fn future-pool-fn
         ([f] [timeout-msecs timeout-val f] [] [timeout-msecs timeout-val]))

       Arities of returned function:
         [f]  - Blocks to acquire a   future,  then executes (f) on that future.
         [ ]  - Blocks to acquire ALL futures, then immediately releases them.
                Useful for blocking till all outstanding work completes.

         [timeout-msecs timeout-val f] - Variant of [f] with timeout
         [timeout-msecs timeout-val  ] - Variant of [ ] with timeout

     See also `future*` for fully custom pools, etc."
     [n-threads]
     (let [n    (get-num-threads n-threads) ; Undocumented special vec support
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

         ([^long timeout-msecs timeout-val]
          (if (.tryAcquire s n timeout-msecs msecs)
            (do  (.release s n) true)
            timeout-val))

         ([^long timeout-msecs timeout-val f]
          (if (.tryAcquire s timeout-msecs msecs)
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
     "Given a nullary fn `f` that is non-idempotent and free of side-effects,
     returns a wrapped version of `f` that asynchronously maintains a cache
     of up to `n-capacity` pre-computed return values of (f).

     Useful when `f` is expensive & may be called in a spikey fashion,
     e.g. ideal for cryptographic key generators."

     {:added "Encore v3.49.0 (2023-02-01)"}
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

(comment
  (let [fp (future-pool 6)] ; Shared future pool
    (def f1 (pre-cache 3 fp (fn [] (Thread/sleep 1000) :f1)))
    (def f2 (pre-cache 4 fp (fn [] (Thread/sleep 500)  :f2)))))

#?(:clj
   (defn runner
     "Experimental, subject to change without notice!!
     Returns a new stateful (fn runner ([]) ([f])) such that:
      (runner f) => Runner should execute given nullary fn according to runner opts.
                    Returns:
                      nil   if runner has stopped accepting new execution requests.
                      true  if fn was accepted for execution *without* back pressure.
                      false if runner's back-pressure mechanism was engaged.

      (runner)   => Runner should stop accepting new execution requests.
                    Returns true iff runner's status changed with this call.

    Compared to agents:
      - Runners have (configurable) back pressure.
      - Runners may be non-linear when n-threads > 1.
      - Runners take nullary fns rather than unary fns of state.
      - Runners have no validators or watches.
      - Runners auto shutdown their threads on JVM shutdown.

    These properties make them useful as configurable async workers, etc.

    Options include:
      `mode`        - Mode of operation, ∈ #{:sync :blocking :dropping :sliding}.
      `buffer-size` - Size of buffer before back-pressure mechanism is engaged.
      `n-threads`   - Number of threads for asynchronously executing fns.
                      NB execution order may be non-sequential when n > 1."

     {:added "Encore v3.68.0 (2023-09-25)"}
     [{:keys [mode buffer-size n-threads thread-name daemon-threads?] :as opts
       :or
       {mode :dropping
        buffer-size 1024
        n-threads   1}}]

     (case mode
       :sync
       (let [stopped?_ (volatile! false)]
         (fn sync-runner
           ([ ] (when-not @stopped?_ (vreset! stopped?_ true)))
           ([f] (if       @stopped?_ nil (do (catching (f)) true)))))

       (:blocking :dropping :sliding)
       (let [stopped?_ (volatile! false)
             abq (java.util.concurrent.ArrayBlockingQueue.
                   (as-pos-int buffer-size) false)
             init_
             (delay
               (when-not daemon-threads?
                 (.addShutdownHook (Runtime/getRuntime)
                   (Thread. (fn stop-runner [] (vreset! stopped?_ true)))))

               (dotimes [n (as-pos-int n-threads)]
                 (let [wfn
                       (fn worker-fn []
                         (loop []
                           (if-let [f (.poll abq 2000 java.util.concurrent.TimeUnit/MILLISECONDS)]
                             ;; Recur unconditionally to drain abq even when stopped
                             (do (catching (f))   (recur))
                             (when-not @stopped?_ (recur)))))

                       thread-name (str-join-once "-" [(or thread-name `runner) "loop" (inc n) "of" n-threads])
                       thread (Thread. wfn thread-name)]

                   (when daemon-threads?
                     (.setDaemon thread true))

                   ;; (println "Starting thread:" thread-name)
                   (.start thread))))

             run-fn
             (case mode
               :blocking (fn [f] (or (.offer abq f) (do (.put abq f) false)))
               :dropping (fn [f] (or (.offer abq f)                  false))
               :sliding
               (fn [f]
                 (or
                   (.offer abq f) ; Common case
                   (loop []
                     (.poll abq) ; Drop
                     (if (.offer abq f)
                       false ; Indicate that drop/s occurred
                       (recur))))))]

         (if-let [msecs (get opts :debug/init-after)]
           (do
             (future (Thread/sleep (int msecs)) @init_)
             (fn async-runner
               ([ ] (when-not @stopped?_ (vreset! stopped?_ true)))
               ([f] (if       @stopped?_ nil (run-fn f)))))

           (fn async-runner
             ([ ] (when-not @stopped?_ (vreset! stopped?_ true)))
             ([f] (if       @stopped?_ nil (do @init_ (run-fn f)))))))

       (unexpected-arg! mode
         {:context  `runner
          :expected #{:sync :blocking :dropping :sliding}}))))

(comment
  (let [r1 (runner {:mode :sync})
        r2 (runner {:mode :blocking})]
    (qb 1e6 ; [40.68 158.46]
      (r1 (fn []))
      (r2 (fn [])))))

;;;; Hostname

#?(:clj
   (let [cache_ (latom nil) ; ?[promise udt]
         cache-update-pending?_ (latom false)]

     (defn ^String get-hostname
       "Returns local hostname string, or `fallback` (default nil).
       Can be slow, prefer 3-arity caching variant when possible."
       {:added "Encore v3.82.0 (2024-02-23) (arities: 1, 2, 3)"}
       ([        ] (get-hostname nil))
       ([fallback]
        (try
          (.getHostName (java.net.InetAddress/getLocalHost))
          (catch Throwable _ fallback)))

       ([timeout-msecs timeout-val]
        (let [p (promise)]
          (future* (p (get-hostname timeout-val)))
          (deref    p timeout-msecs timeout-val)))

       ;; Note custom cache semantics, unlike standard ttl cache.
       ;; When cache is stale, continue to deliver pre-existing (stale) cache
       ;; until a new (fresh) value becomes available.
       ([^long cache-msecs timeout-msecs timeout-val]
        (loop [force-use-cache? false]

          (if (or force-use-cache? (cache-update-pending?_))
            (let [[p] (cache_)] (deref p timeout-msecs timeout-val))
            (let [t1 (System/currentTimeMillis)]
              (if-let [[p ^long t0] (cache_)]
                (if (< (- t1 t0) cache-msecs) ; Have fresh cache
                  (deref p timeout-msecs timeout-val)
                  (do
                    ;; Ensure exactly 1 async thread is updating cache
                    (when (compare-and-set! cache-update-pending?_ false true)
                      (threaded
                        (if-let [new-hostname (get-hostname nil)] ; Take as long as needed
                          (reset! cache_ [((promise) new-hostname) t1]) ; Update p and t
                          (reset! cache_ [p                        t1]) ; Update only  t
                          )
                        (reset! cache-update-pending?_ false)))
                    (recur true)))

                (let [p (promise)]
                  (when (compare-and-set! cache_ nil [p t1]) ; First call
                    ;; Init cache with pending init value
                    (threaded (p (get-hostname timeout-val))))
                  (recur true))))))))))

(comment
  (defn-cached get-hostname-ttl {:ttl-ms (msecs :mins 1)} [] (get-hostname 5000 "UnknownHost"))
  (qb 1e5 ; [4.58 466.53 7.51 9.32]
    (get-hostname                      "UnknownHost") ; Blocking
    (get-hostname                 5000 "UnknownHost") ; With timeout
    (get-hostname (msecs :mins 1) 5000 "UnknownHost") ; With timeout + cache
    (get-hostname-ttl)))

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
   (defn get-win-loc
     "Returns current window location as
     {:keys [href protocol hostname host pathname search hash]}."
     []
     (when-let [loc (oget js-?window "location")]
       {;; Ref. <http://bl.ocks.org/abernier/3070589>
        :href     (.-href     loc) ; "http://www.example.org:80/foo/bar?q=baz#bang"
        :protocol (.-protocol loc) ; "http:" ; Note the :
        :hostname (.-hostname loc) ; "example.org"
        :host     (.-host     loc) ; "example.org:80"
        :pathname (.-pathname loc) ; "/foo/bar"
        :search   (.-search   loc) ; "?q=baz"
        :hash     (.-hash     loc) ; "#bang"
        })))

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
   (defn ajax-call
     "Queues a lightweight Ajax call with Google Closure's `goog.net.XhrIo` and
     returns nil, or the resulting `goog.net.XhrIo` instance if one was
     immediately available from the XHR pool:

       (ajax-call
         \"http://localhost:8080/my-post-route\" ; Endpoint URL

         {:method     :post ; ∈ #{:get :post :put}
          :resp-type  :text ; ∈ #{:auto :edn :json :xml :text}

          :params     {:username \"Rich Hickey\" :type \"Awesome\"} ; Request params
          :headers    {\"Content-Type\" \"text/plain\"} ; Request headers

          :timeout-ms 7000
          :with-credentials? false ; Enable if using CORS (requires xhr v2+)

          :xhr-pool       my-xhr-pool ; Optional `goog.net.XhrIoPool` instance or delay
          :xhr-cb-fn      (fn [xhr])  ; Optional fn to call with `XhrIo` from pool when available
          :xhr-timeout-ms 2500        ; Optional max msecs to wait on pool for `XhrIo`
         }

         (fn ajax-callback-fn [resp-map]
           (let [{:keys [success? ?status ?error ?content ?content-type]} resp-map]
             ;; ?status ; ∈ #{nil 200 404 ...}, non-nil iff server responded
             ;; ?error  ; ∈ #{nil <http-error-status-code> <exception> :timeout
                              :abort :http-error :exception :xhr-pool-depleted}
             (js/alert (str \"Ajax response: \" resp-map)))))"

     {:added "Encore v3.74.0 (2023-11-06)"}
     [url
      {:keys [method params headers timeout-ms resp-type with-credentials?
              xhr-pool xhr-cb-fn xhr-timeout-ms] :as opts
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
               (let [timeout-ms (or (get opts :timeout) timeout-ms) ; Deprecated opt
                     xhr-method
                     (case method
                       :get  "GET"
                       :post "POST"
                       :put  "PUT"
                       (unexpected-arg! method
                         {:context  `ajax-call
                          :param    'method
                          :expected #{:get :post :put}}))

                     [xhr-url xhr-?data] (coerce-xhr-params url method params)

                     xhr-headers
                     (let [headers (map-keys #(str/lower-case (name %)) headers)
                           headers (assoc-some headers "x-requested-with"
                                     (get headers "x-requested-with" "XMLHTTPRequest"))]
                       ;; `x-www-form-urlencoded`/`multipart/form-data` content-type
                       ;; will be added by Closure if a custom content-type isn't provided
                       (clj->js headers))

                     ?progress-listener
                     (when-let [pf (get opts :progress-fn)]
                       (.setProgressEventsEnabled xhr true)
                       (gevents/listen xhr goog.net.EventType/PROGRESS
                         (fn [ev]
                           (let [length-computable? (.-lengthComputable ev)
                                 loaded (.-loaded ev)
                                 total  (.-total  ev)
                                 ?ratio (when (and length-computable? (not= total 0))
                                          (/ loaded total))]
                             (pf
                               {:length-computable? length-computable?
                                :?ratio ?ratio
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
                                           :text           (.getResponseText xhr)
                                           (unexpected-arg! resp-type
                                             {:context  `ajax-call
                                              :param    'resp-type
                                              :expected #{:auto :edn :json :xml :text}}))

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

                 (.send xhr xhr-url xhr-method xhr-?data xhr-headers)

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
         (let [done?_ (latom false)]

           (js/setTimeout
             (fn xhr-timeout []
               (when (-cas!? done?_ false true)
                 (callback-fn {:?error :xhr-pool-timeout})))
             xhr-timeout-ms)

           (.getObject xhr-pool
             (fn xhr-cb [xhr]
               ;; We've acquired xhr after some time
               (if (-cas!? done?_ false true)
                 (with-xhr                xhr)
                 (.releaseObject xhr-pool xhr))))
           nil)))))

;;;; Ring

#?(:clj
   (defn session-swap
     "Util to help correctly update Ring sessions (something easy to get wrong!).

     Given a Ring request (rreq) and Ring response (rresp), returns a new
     Ring response with the response session updated to be (f <old-session>)
     or (apply f <old-session> args)."
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
   (defn ^:no-doc -ring-merge-headers
     "Experimental!"
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

(defn url-encode
  "Based on <https://goo.gl/fBqy6e>."
  #?(:clj  [s & [encoding]] :cljs [s])
  (when s
    #?(:clj  (-> (str s)
               (java.net.URLEncoder/encode (str (or encoding "UTF-8")))
               (str/replace "*" "%2A") ; Cautious, <https://stackoverflow.com/a/25149577/1982742>
               (str/replace "+" "%20") ; Cautious, <https://stackoverflow.com/a/40292770/1982742>
               )
       :cljs (-> (str s)
               (js/encodeURIComponent s)
               (str/replace "*" "%2A")))))

(defn url-decode
  "Stolen from <http://goo.gl/99NSR1>."
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

;;;; Stubs (experimental)
;; Could do with a refactor

(do
  #?(:cljs
     (defn ^:no-doc -new-stubfn_ [name]
       (volatile!
         (fn [& args]
           (throw (ex-info (str "[encore/stubfn] Attempted to call uninitialized stub fn (" name ")")
                    {:stub name :args args}))))))

  (defn ^:no-doc -assert-unstub-val [x]
    (let [pred #?(:clj symbol? :cljs fn?)]
      (if (pred x)
        x
        (throw (ex-info "[encore/stubfn] Unexpected unstub type (expected symbol)"
                 {:unstub {:value x :type (type x)}})))))

  #?(:clj
     (defmacro ^:no-doc -intern-stub [ns stub-sym stub-var src]
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
     "Experimental!!
     Declares a stub var that can be initialized from any namespace with
     `unstub-<stub-name>`.

     Decouples a var's declaration (location) and its initialization (value).
     Handy for defining vars in a shared ns from elsewhere (e.g. a private
     or cyclic ns)."
     [sym]
     (let [   stub-sym sym
            unstub-sym (with-meta (symbol (str  "unstub-" (name stub-sym))) {:doc "Call to initialze stub"})
           -unstub-sym (with-meta (symbol (str "-unstub-" (name stub-sym))) {:no-doc true})]

       (if (:ns &env)
         ;; No declare/intern support
         `(let [~'stubfn_ (-new-stubfn_ ~(name stub-sym))]
            (defn ~-unstub-sym [~'f] (vreset! ~'stubfn_ (-assert-unstub-val ~'f)))
            (defn  ~unstub-sym [~'f] (~-unstub-sym ~'f))
            (defn    ~stub-sym [~'& ~'args] (apply @~'stubfn_ ~'args)))

         `(let [stub-var# (declare ~(with-meta stub-sym {:redef true}))]
            (defmacro ~unstub-sym
              [~'x] ; ~'sym for clj, ~'f for cljs
              (if (:ns ~'&env)
                 ;; In Cljs, a macro+fn can have the same name. Preference will be
                 ;; given to the macro in contexts where both are applicable.
                 ;; So there's 3 cases to consider:
                 ;;   1. clj   stub: def var, clj macro
                 ;;   2. cljs  stub: def volatile, 2 fns
                 ;;   3. clj/s stub: def volatile, 2 fns, var, and clj/s macro
                `(~'~(symbol (str *ns*) (str (name -unstub-sym))) ~~'x)
                `(-intern-stub ~'~(symbol (str *ns*)) ~'~stub-sym
                   ~stub-var# ~~'x))))))))

(comment
  (defn- -foo ^long [y] (* y y))
  (macroexpand-all '(defstub foo))
  (defstub     foo)
  (unstub-foo -foo)
  (qb 1e6 (-foo 5) (foo 5)) ; [68.49 71.88]
  (meta (first (:arglists (meta #'foo))))

  (do
    #?(:cljs (def cljs-thing "cljs-thing")
       :clj  (def clj-thing  "clj-thing"))

    #?(:clj (defmacro cljs-macro [] (if (:ns &env) `cljs-thing `clj-thing)))

    #?(:clj  (cljs-macro)
       :cljs (enc-macros/cljs-macro))

    #?(:cljs (enc-macros/defstub stub-test)
       :clj             (defstub stub-test))

    #?(:cljs (enc-macros/unstub-stub-test identity)
       :clj             (unstub-stub-test identity))))

;;;; Name filter

(let [always (fn always [_in] true)
      never  (fn never  [_in] false)

      name? (fn [x] (or (string? x) (named? x)))
      qname (fn [x] (if (named?  x) (let [n (name x)] (if-let [ns (namespace x)] (str ns "/" n) n)) x))
      qname!
      (fn [x]
        (cond
          (string? x) x
          (nil?    x) ""
          (or
            (qname x)
            (unexpected-arg! x
              {:context  `name-filter
               :param    'filter-input-arg
               :expected '#{string keyword symbol nil}}))))

      wild-str->?re-pattern
      (fn [s]
        (when (str-contains? s "*")
          (re-pattern
            (-> (str "^" s "$")
              (str/replace "." "\\.")
              (str/replace "*" "(.*)")))))

      compile->match-fn
      (fn compile->match-fn
        [spec cache?]
        (cond
          (#{:any "*"}     spec) always
          (#{:none #{} []} spec) never
          (re-pattern?     spec) (fn match? [in] (re-find spec (qname! in)))
          (name?           spec)
          (cond
            :let    [spec       (qname spec)]
            :if-let [re-pattern (wild-str->?re-pattern spec)] (recur re-pattern cache?)
            :else   (fn match? [in] (= spec (qname! in))))

          (or (vector? spec) (set? spec))
          (cond
            ;; (empty? spec)   never
            ((set spec) "*")   always
            (= (count spec) 1) (recur (first spec) cache?)
            :else
            (let [[fixed-strs re-patterns]
                  (reduce
                    (fn [[fixed-strs re-patterns] spec]
                      (let [spec (qname spec)]
                        (if-let [re-pattern (if (re-pattern? spec) spec (wild-str->?re-pattern spec))]
                          [      fixed-strs       (conj re-patterns re-pattern)]
                          [(conj fixed-strs spec)       re-patterns            ])))
                    [#{} []]
                    spec)

                  fx-match (not-empty fixed-strs) ; #{"foo" "bar"}, etc.
                  re-match
                  (when-let [re-patterns (not-empty re-patterns)] ; ["foo.*", "bar.*"], etc.
                    (let [f (fn match? [in-str] (rsome #(re-find % in-str) re-patterns))]
                      (if cache? (fmemoize f) f)))]

              (cond!
                (and fx-match re-match)
                (fn match? [in]
                  (let [in-str (qname! in)]
                    (or
                      (fx-match in-str)
                      (re-match in-str))))

                fx-match (fn [in] (fx-match (qname! in)))
                re-match (fn [in] (re-match (qname! in))))))

          :else
          (unexpected-arg! spec
            {:context  `name-filter
             :param    'filter-spec
             :expected '#{string keyword set regex {:allow <spec>, :deny <spec>}}})))]

  (defn name-filter
    "Given filter `spec`, returns a compiled (fn conform? [name]) that takes
    any nameable type (string, keyword, symbol).

    Spec may be:
      - A regex pattern. Will conform on match.
      - A str/kw/sym, in which \"*\"s act as wildcards. Will conform on match.

      - A vector or set of regex patterns or strs/kws/syms.
        Will conform on ANY match.
        If you need literal \"*\"s, use #\"\\*\" regex instead.

      - {:allow <spec> :deny <spec>} with specs as above.
        Will conform iff `allow` spec matches AND `deny` spec does NOT.

    Resulting conform fn is useful as allowlist and/or denylist.
    Example inputs: namespace strings, class names, ids, etc.

    Spec examples:
      #{}, \"*\", \"foo.bar\", \"foo.bar.*\", #{\"foo\" \"bar.*\"},
      {:allow #{\"foo\" \"bar.*\"} :deny #{\"foo.*.bar.*\"}},
      #\"(foo1|foo2)\\.bar\"."

    {:added "Encore v3.67.0 (2023-09-08)"}
    [spec]
    (if-not (map? spec)
      (recur {:allow spec :deny nil})
      (let [cache?         (get spec :cache?)
            allow-spec (or (get spec :allow) (get spec :whitelist))
            deny-spec  (or (get spec :deny)  (get spec :blacklist))

            allow (when-let [as allow-spec] (compile->match-fn as cache?))
            deny  (when-let [ds deny-spec]  (compile->match-fn ds cache?))]

        (cond
          (= deny  always) never
          (= allow never)  never

          (and allow deny)
          (fn conform? [in]
            (if ^boolean (allow in)
              (if ^boolean (deny in)
                false
                true)
              false))

          allow (if (= allow always) always (fn conform? [in] (if ^boolean (allow in) true  false)))
          deny  (if (= deny  never)  always (fn conform? [in] (if ^boolean (deny  in) false true)))
          :else
          (throw
            (ex-info "[encore/name-filter] `allow-spec` and `deny-spec` cannot both be nil"
              {:allow-spec allow-spec :deny-spec deny-spec})))))))

(comment (let [nf (name-filter #{"foo.*" "bar"})] (qb 1e6 (nf "foo")))) ; 118.25

;;;; Namespaces

(declare str-starts-with?)

#?(:clj
   (defn interns-overview
     "Returns {:keys [api public private impl test no-doc]}, with each key mapped
     to an alphabetical list of the relevant vars in given namespace.

     \"impl\" vars are public vars with names that begin with \"-\" or \"_\",
     a naming convention commonly used to indicate vars intended to be treated
     as private implementation details even when public."
     ([  ] (interns-overview *ns*))
     ([ns]
      (map-vals (comp vec sort)
        (reduce-kv
          (fn [{:keys [public private impl test no-doc] :as m} k v]
            (let [mt (meta v)]
              (cond
                (:test    mt) (update m :test    conj k)
                (:private mt) (update m :private conj k)

                :if-let [impl?
                         (let [sw? (partial str-starts-with? (name (:name mt)))]
                           (and
                             (not (sw? "->"))
                             (some sw? ["-" "_" "*-" "*_"])))]
                (update m :impl conj k)

                :let [m (update m :public conj k)]

                (:no-doc mt) (update m :no-doc conj k)
                :else        (update m :api    conj k))))
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
      #?(:cljs (js/setTimeout f msecs)
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
        #?(:clj (java.util.Timer. "encore/timer" true))))))

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
     (tf-poll       [_]                     @result__)
     (tf-done?      [_] (not (identical-kw? @result__ :timeout/pending)))
     (tf-pending?   [_]      (identical-kw? @result__ :timeout/pending))
     (tf-cancelled? [_]      (identical-kw? @result__ :timeout/cancelled))
     (tf-cancel!    [_] (-cas!? result__ :timeout/pending :timeout/cancelled))

     IPending (-realized?  [t] (tf-done? t))
     IDeref   (-deref      [t] (tf-poll  t))))

#?(:clj
   (deftype TimeoutFuture
     [f result__ ^long udt ^CountDownLatch latch]
     ITimeoutFuture
     (tf-state      [_] {:fn f :udt udt})
     (tf-poll       [_]                     (result__))
     (tf-done?      [_] (not (identical-kw? (result__) :timeout/pending)))
     (tf-pending?   [_]      (identical-kw? (result__) :timeout/pending))
     (tf-cancelled? [_]      (identical-kw? (result__) :timeout/cancelled))
     (tf-cancel!    [_]
       (if (-cas!? result__ :timeout/pending :timeout/cancelled)
         (do (.countDown latch) true)
         false))

     clojure.lang.IPending (isRealized  [t] (tf-done? t))
     clojure.lang.IDeref   (deref       [_] (.await latch) (result__))
     clojure.lang.IBlockingDeref
     (deref [_ timeout-msecs timeout-val]
       (if (.await latch timeout-msecs java.util.concurrent.TimeUnit/MILLISECONDS)
         (result__)
         timeout-val))

     java.util.concurrent.Future
     (isCancelled [t]   (tf-cancelled? t))
     (isDone      [t]   (tf-done?      t))
     (cancel      [t _] (tf-cancel!    t))))

(defn timeout-future? #?(:cljs {:tag 'boolean}) [x] (instance? TimeoutFuture x))

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
         udt   (+ (now-udt) msecs) ; Approx instant to run
         result__ (latom :timeout/pending)
         #?(:clj latch) #?(:clj (CountDownLatch. 1))
         cas-f
         (fn []
           (let [result_ (delay (f))]
             (when (-cas!? result__ :timeout/pending result_)
               @result_
               #?(:clj (.countDown latch)))))]

     (let [impl (force impl_)] (-schedule-timeout impl msecs cas-f))
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
;; {:deprecated "v<X.Y.Z> (<YYYY-MM-DD>)" :doc "Prefer `<replacement>`."}
;; Note `:deprecation-nowarn` meta used as workaround for long-standing
;; CLJS-2000, Ref. <https://clojure.atlassian.net/browse/CLJS-2000>.

#?(:clj
   (defmacro deprecated
     "Elides body when `taoensso.elide-deprecated` JVM property or
     `TAOENSSO_ELIDE_DEPRECATED` environment variable is ∈ #{\"true\" \"TRUE\"}."
     [& body]
     (let [elide? (get-env {:as :bool} :taoensso.elide-deprecated<.platform>)]
       (when-not elide? `(do ~@body)))))

(deprecated
  (defn ^:no-doc -swap-val!
    "Prefer `latom`."
    {:deprecated "Encore v3.67.0 (2023-09-08)"}
    [atom_ k f]
    (loop []
      (let [m0 @atom_
            v1 (f (get m0 k))
            m1 (assoc  m0 k v1)]
        (if (-cas!? atom_ m0 m1)
          v1
          (recur)))))

  #?(:cljs (def ^:no-doc ^:deprecated regular-num?        finite-num?))
  #?(:cljs (def ^:no-doc ^:deprecated get-window-location get-win-loc))
  #?(:clj  (def ^:no-doc ^:deprecated srng                secure-rng))
  (def ^:no-doc ^:deprecated backport-run!   run!)
  (def ^:no-doc ^:deprecated fq-name         as-qname)
  (def ^:no-doc ^:deprecated qname           as-qname)
  (def ^:no-doc ^:deprecated merge-deep-with nested-merge-with)
  (def ^:no-doc ^:deprecated merge-deep      nested-merge)
  (def ^:no-doc ^:deprecated parse-bool      as-?bool)
  (def ^:no-doc ^:deprecated parse-int       as-?int)
  (def ^:no-doc ^:deprecated parse-float     as-?float)
  (def ^:no-doc ^:deprecated swapped*        swapped)
  (def ^:no-doc ^:deprecated memoize-a0_     memoize)
  (def ^:no-doc ^:deprecated memoize-a1_     memoize)
  (def ^:no-doc ^:deprecated a0-memoize_     memoize)
  (def ^:no-doc ^:deprecated a1-memoize_     memoize)
  (def ^:no-doc ^:deprecated memoize-1       memoize-last)
  (def ^:no-doc ^:deprecated memoize1        memoize-last)
  (def ^:no-doc ^:deprecated memoize*        memoize)
  (def ^:no-doc ^:deprecated memoize_        memoize)
  (def ^:no-doc ^:deprecated nnil?           some?)
  (def ^:no-doc ^:deprecated nneg-num?       nat-num?)
  (def ^:no-doc ^:deprecated nneg-int?       nat-int?)
  (def ^:no-doc ^:deprecated nneg-float?     nat-float?)
  (def ^:no-doc ^:deprecated uint?           nat-int?)
  (def ^:no-doc ^:deprecated pint?           pos-int?)
  (def ^:no-doc ^:deprecated nnil=           some=)
  (def ^:no-doc ^:deprecated as-?uint        as-?nat-int)
  (def ^:no-doc ^:deprecated as-?pint        as-?pos-int)
  (def ^:no-doc ^:deprecated as-?ufloat      as-?nat-float)
  (def ^:no-doc ^:deprecated as-?pfloat      as-?pos-float)
  (def ^:no-doc ^:deprecated as-uint         as-nat-int)
  (def ^:no-doc ^:deprecated as-pint         as-pos-int)
  (def ^:no-doc ^:deprecated as-ufloat       as-nat-float)
  (def ^:no-doc ^:deprecated as-pfloat       as-pos-float)
  (def ^:no-doc ^:deprecated run!*           run!)
  (def ^:no-doc ^:deprecated ?subvec<idx     (comp not-empty get-subvec))
  (def ^:no-doc ^:deprecated ?subvec<len     (comp not-empty get-subvector))
  (def ^:no-doc ^:deprecated nano-time       now-nano)
  (def ^:no-doc ^:deprecated -swap-cache!    -swap-val!)
  (def ^:no-doc ^:deprecated -unswapped      swapped-vec)
  (def ^:no-doc ^:deprecated -vswapped       swapped-vec)
  (def ^:no-doc ^:deprecated -swap-k!        -swap-val!)
  (def ^:no-doc ^:deprecated update-in*      update-in)
  (def ^:no-doc ^:deprecated idx-fn          counter)
  (def ^:no-doc ^:deprecated vec*            ensure-vec)
  (def ^:no-doc ^:deprecated set*            ensure-set)
  (def ^:no-doc ^:deprecated have-transducers? true)

  (def ^:no-doc ^:deprecated     pval?    pnum?)
  (def ^:no-doc ^:deprecated as-?pval as-?pnum)
  (def ^:no-doc ^:deprecated  as-pval  as-pnum)

  (defn ^:no-doc get-substr
    {:deprecated "Encore v3.26.0 (2022-10-14)"
     :doc "Prefer `get-substr-by-idx`."}
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

  (defn ^:no-doc get-substring
    {:deprecated "Encore v3.26.0 (2022-10-14)"
     :doc "Prefer `get-substr-by-len`."}
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

  (def ^:no-doc ^:deprecated ?substr<idx (comp as-?nempty-str get-substr))
  (def ^:no-doc ^:deprecated ?substr<len (comp as-?nempty-str get-substring))  

  ;; Used by old versions of Timbre, Tufte
  (let [nolist? #(contains? #{nil [] #{}} %)]
    (defn ^:no-doc ^:deprecated compile-ns-filter
      ([ns-pattern         ] ^:deprecation-nowarn (compile-ns-filter ns-pattern nil))
      ([whitelist blacklist]
       (if (and (nolist? whitelist) (nolist? blacklist))
         (fn [_] true) ; Unfortunate API choice
         (name-filter {:allow whitelist :deny blacklist})))))

  #?(:clj (defn ^:no-doc ^:deprecated set-body      [rresp body]    (ring-set-body      body    rresp)))
  #?(:clj (defn ^:no-doc ^:deprecated set-status    [rresp code]    (ring-set-status    code    rresp)))
  #?(:clj (defn ^:no-doc ^:deprecated merge-headers [rresp headers] (ring-merge-headers headers rresp)))
  #?(:clj (def  ^:no-doc ^:deprecated redirect-resp ring-redirect-resp))

  #?(:clj
     (do
       (defmacro ^:no-doc ^:deprecated if-lets       [& args]  `(taoensso.encore/if-let        ~@args))
       (defmacro ^:no-doc ^:deprecated when-lets     [& args]  `(taoensso.encore/when-let      ~@args))
       (defmacro ^:no-doc ^:deprecated if-not*       [& args]  `(taoensso.encore/if-not        ~@args))
       (defmacro ^:no-doc ^:deprecated cond*         [& args]  `(taoensso.encore/cond          ~@args))
       (defmacro ^:no-doc ^:deprecated defonce*      [& args]  `(taoensso.encore/defonce       ~@args))
       (defmacro ^:no-doc ^:deprecated have-in       [a1 & an] `(taoensso.encore/have  ~a1 :in ~@an))
       (defmacro ^:no-doc ^:deprecated have-in!      [a1 & an] `(taoensso.encore/have! ~a1 :in ~@an))
       (defmacro ^:no-doc ^:deprecated cond-throw    [& args]  `(taoensso.encore/cond!         ~@args))
       (defmacro ^:no-doc ^:deprecated catch-errors* [& args]  `(taoensso.encore/catching      ~@args))
       (defmacro ^:no-doc ^:deprecated use-fixtures* [& args]  `(taoensso.encore/use-fixtures  ~@args))
       (defmacro ^:no-doc ^:deprecated nano-time*    [& args]  `(taoensso.encore/now-nano*     ~@args))
       (defmacro ^:no-doc ^:deprecated qbench        [& args]  `(taoensso.encore/quick-bench   ~@args))
       (defmacro ^:no-doc ^:deprecated catch-errors  [& body]  `(catching [(do ~@body) nil] e# [nil e#]))

       (defmacro ^:no-doc ^:deprecated -vol!       [val]           `(volatile!     ~val))
       (defmacro ^:no-doc ^:deprecated -vol-reset! [vol_ val]      `(vreset! ~vol_ ~val))
       (defmacro ^:no-doc ^:deprecated -vol-swap!  [vol_ f & args] `(vswap!  ~vol_ ~f ~@args))

       (defmacro ^:no-doc thrown
         {:deprecated "Encore v3.31.0 (2022-10-27)"
          :doc "Prefer `throws`."}
         [& args] `(throws ~@args))))

  ;;; Prefer `str-join` when possible (needs Clojure 1.7+)
  #?(:cljs (defn ^:no-doc ^:deprecated undefined->nil [x] (if (undefined? x) nil x)))
  (defn ^:no-doc ^:deprecated spaced-str-with-nils [xs] (str/join " " (mapv nil->str xs)))
  (defn ^:no-doc ^:deprecated spaced-str [xs] (str/join " " #?(:clj xs :cljs (mapv undefined->nil xs))))

  ;; Arg order changed for easier partials, etc.:
  (defn ^:no-doc ^:deprecated round [n & [type nplaces]] (round* (or type :round) nplaces n))
  (defn ^:no-doc ^:deprecated approx=
    ([x y      ] (approx==       x y))
    ([x y signf] (approx== signf x y)))

  ;; & coll changed to coll:
  (defn ^:no-doc ^:deprecated join-once [sep & coll] (str-join-once sep coll))

  #?(:clj
     (do ;; Used by Carmine <= v2.7.0
       (defmacro ^:no-doc ^:deprecated repeatedly* [n & body] `(repeatedly-into* [] ~n ~@body))
       (defmacro ^:no-doc ^:deprecated repeatedly-into* ; Used by Nippy < v2.10
         [coll n & body] `(repeatedly-into ~coll ~n (fn [] ~@body)))))
  
  (defn ^:no-doc ^:deprecated nnil-set [x] (disj (ensure-set x) nil))

  ;;; Arg order changed for easier partials
  (defn ^:no-doc ^:deprecated keys=      [m ks] (ks=      ks m))
  (defn ^:no-doc ^:deprecated keys<=     [m ks] (ks<=     ks m))
  (defn ^:no-doc ^:deprecated keys>=     [m ks] (ks>=     ks m))
  (defn ^:no-doc ^:deprecated keys=nnil? [m ks] (ks-nnil? ks m))

  ;; Used by Sente <= v1.4.0-alpha2
  (def ^:no-doc ^:deprecated logging-level (atom :debug)) ; Just ignoring this now

  #?(:cljs ; Used by Sente <= v1.1.0
     (defn ^:no-doc ^:deprecated set-exp-backoff-timeout! [nullary-f & [nattempt]]
       (js/setTimeout nullary-f (exp-backoff (or nattempt 0)))))

  #?(:cljs
     (do ; Level-based Cljs logging (prefer Timbre v4+)
       (defonce ^:no-doc ^:deprecated ^:dynamic *log-level* :debug)
       (def ^:private log?
         (let [->n {:trace 1 :debug 2 :info 3 :warn 4 :error 5 :fatal 6 :report 7}]
           (fn [level] (>= (->n level) (->n *log-level*)))))

       (defn ^:no-doc ^:deprecated tracef  [fmt & xs] (when (log? :trace)  (apply logf fmt xs)))
       (defn ^:no-doc ^:deprecated debugf  [fmt & xs] (when (log? :debug)  (apply logf fmt xs)))
       (defn ^:no-doc ^:deprecated infof   [fmt & xs] (when (log? :info)   (apply logf fmt xs)))
       (defn ^:no-doc ^:deprecated warnf   [fmt & xs] (when (log? :warn)   (apply logf (str "WARN: "  fmt) xs)))
       (defn ^:no-doc ^:deprecated errorf  [fmt & xs] (when (log? :error)  (apply logf (str "ERROR: " fmt) xs)))
       (defn ^:no-doc ^:deprecated fatalf  [fmt & xs] (when (log? :fatal)  (apply logf (str "FATAL: " fmt) xs)))
       (defn ^:no-doc ^:deprecated reportf [fmt & xs] (when (log? :report) (apply logf fmt xs)))))

  (defn ^:no-doc ^:deprecated greatest [coll & [?comparator]]
    (let [comparator (or ?comparator rcompare)]
      (reduce #(if (pos? (comparator %1 %2)) %2 %1) coll)))

  (defn ^:no-doc ^:deprecated least [coll & [?comparator]]
    (let [comparator (or ?comparator rcompare)]
      (reduce #(if (neg? (comparator %1 %2)) %2 %1) coll)))

  (defn ^:no-doc ^:deprecated clj1098 "Ref. <http://goo.gl/0GzRuz>" [x] (or x {}))

  (defn ^:no-doc ^:deprecated distinct-by
    "Prefer `xdistinct`."
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

  (defn ^:no-doc ^:deprecated distinctv
    "Prefer `xdistinct`."
    ([      coll] ^:deprecation-nowarn (distinctv identity coll))
    ([keyfn coll]
     (let [tr (reduce (fn [[v seen] in]
                        (let [in* (keyfn in)]
                          (if-not (contains? seen in*)
                            [(conj! v in) (conj seen in*)]
                            [v seen])))
                [(transient []) #{}]
                coll)]
       (persistent! (nth tr 0)))))

  (defn ^:no-doc ^:deprecated map-kvs [kf vf m]
    "Prefer `reduce-kv`."
    (if-not m {}
      (let [vf (cond (nil? vf) (fn [_ v] v) :else vf)
            kf (cond (nil? kf) (fn [k _] k)
                 (identical-kw? kf :keywordize) (fn [k _] (keyword k))
                 :else kf)]
        (persistent!
          (reduce-kv (fn [m k v] (assoc! m (kf k v) (vf k v)))
            (transient {}) m)))))

  (defn ^:no-doc ^:deprecated as-map [kvs & [kf vf]]
    "Prefer `reduce-kvs`."
    (if (empty? kvs) {}
        (let [vf (cond (nil? vf) (fn [_ v] v) :else vf)
              kf (cond (nil? kf) (fn [k _] k)
                   (identical-kw? kf :keywordize) (fn [k _] (keyword k))
                   :else kf)]
          (persistent!
            (reduce-kvs
              (fn [m k v] (assoc! m (kf k v) (vf k v))) (transient {}) kvs)))))

  (defn ^:no-doc ^:deprecated keywordize-map [m] (map-keys keyword m))
  (defn ^:no-doc ^:deprecated removev [pred coll] (filterv (complement pred) coll))
  (defn ^:no-doc ^:deprecated nvec? [n x] (and (vector? x) (= (count x) n)))

  (defn ^:no-doc ^:deprecated memoized [cache f & args]
    (if-not cache ; {<args> <delay-val>}
      (apply f args)
      (deref ^:deprecation-nowarn
        (-swap-val! cache args (fn [?dv] (if ?dv ?dv (delay (apply f args))))))))

  (defn- translate-signed-idx [^long signed-idx ^long max-idx]
    (if (>= signed-idx 0)
      (min      signed-idx max-idx)
      (max 0 (+ signed-idx max-idx))))

  (comment (translate-signed-idx -3 5))

  (defn ^:no-doc ^:deprecated sub-indexes
    [x start-idx & {:keys [^long max-len ^long end-idx]}]
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

  (defn ^:no-doc ^:deprecated substr
    "Prefer `get-substr-by-idx` or `get-substr-by-len`"
    [s start-idx & [?max-len]]
    (let [[start-idx* end-idx*] ^:deprecation-nowarn (sub-indexes s start-idx :max-len ?max-len)]
      #?(:clj  (.substring ^String s start-idx* end-idx*)
         :cljs (.substring         s start-idx* end-idx*))))

  (comment (substr "hello" -1 1))

  (defn ^:no-doc ^:deprecated subvec*
    "Prefer `get-subvec` or `get-subvector`"
    [v start-idx & [?max-len]]
    (let [[start-idx* end-idx*] ^:deprecation-nowarn (sub-indexes v start-idx :max-len ?max-len)]
      (subvec v start-idx* end-idx*)))

  (def  ^:no-doc ^:deprecated sentinel (new-object))
  (defn ^:no-doc ^:deprecated sentinel?     [x] (identical? x sentinel))
  (defn ^:no-doc ^:deprecated nil->sentinel [x] (if (nil? x) sentinel x))
  (defn ^:no-doc ^:deprecated sentinel->nil [x] (if ^:deprecation-nowarn (sentinel? x) nil x))

  (defn ^:no-doc ^:deprecated    singleton? [coll] (if (counted? coll) (= (count coll) 1) (not (next coll))))
  (defn ^:no-doc ^:deprecated ->?singleton  [coll] (when ^:deprecation-nowarn (singleton? coll) (let [[c1] coll] c1)))
  (defn ^:no-doc ^:deprecated ->vec [x] (cond (vector? x) x (sequential? x) (vec x) :else [x]))

  (defn ^:no-doc ^:deprecated fzipmap [ks vs]
    (loop [m  (transient {})
           ks (seq ks)
           vs (seq vs)]
      (if-not (and ks vs)
        (persistent! m)
        (recur (assoc! m (first ks) (first vs))
          (next ks)
          (next vs)))))

  (defn ^:no-doc ^:deprecated filter-kvs [pred m] (if (nil? m) {} (reduce-kv (fn [m k v] (if (pred k v)         m    (dissoc m k))) m m)))
  (defn ^:no-doc ^:deprecated remove-kvs [pred m] (if (nil? m) {} (reduce-kv (fn [m k v] (if (pred k v) (dissoc m k)         m))    m m)))

  (defn ^:no-doc ^:deprecated revery     [pred coll] (reduce    (fn [acc in]  (if (pred in)  coll (reduced nil))) coll coll))
  (defn ^:no-doc ^:deprecated revery-kv  [pred coll] (reduce-kv (fn [acc k v] (if (pred k v) coll (reduced nil))) coll coll))

  (def ^:no-doc ^:deprecated every revery)

  (defn ^:no-doc ^:deprecated replace-in [m & ops]
    (reduce
      (fn [m ?op]
        (if-not ?op
          m ; Support conditional ops
          (let [[type ks valf] ?op
                f (if (identical-kw? type :reset) (fn [_] valf) valf)]
            (update-in m ks nil f))))
      m
      ops))

  (let [return (fn [m0 v0 m1 v1] [v0 v1])]
    (defn ^:no-doc ^:deprecated swap-in!*
      "Prefer `swap-in!` with `swapped` return value."
      ([atom_              f] (-swap-k0! return atom_              f))
      ([atom_ ks           f] (-swap-kn! return atom_ ks nil       f))
      ([atom_ ks not-found f] (-swap-kn! return atom_ ks not-found f)))

    (defn ^:no-doc ^:deprecated swap-val!*
      "Prefer `swap-val!` with `swapped` return value."
      ([atom_ k           f] (-swap-k1! return atom_ k nil       f))
      ([atom_ k not-found f] (-swap-k1! return atom_ k not-found f))))

  (def ^:no-doc ^:deprecated dswap! swap-in!*)
  (def ^:no-doc ^:deprecated swap!* swap-in!*)

  #?(:clj (defalias ^:no-doc ^:deprecated taoensso.truss/get-dynamic-assertion-data))
  #?(:clj (defalias ^:no-doc ^:deprecated taoensso.truss/with-dynamic-assertion-data))

  (defalias ^:no-doc compile-str-filter name-filter
    {:deprecated "Encore v3.67.0 (2023-09-08)"
     :doc "Renamed to `name-filter`."})

  (defn ^:no-doc ^:deprecated kw-identical?
    "Prefer `identical-kw?` macro."
    #?(:cljs {:tag 'boolean})
    [x y] (identical-kw? x y))

  #?(:clj
     (defmacro ^:no-doc -if-cas!
       "Prefer `-cas!?`."
       {:deprecated "Encore v3.67.0 (2023-09-08)"}
       [atom_ old-val new-val then & [else]]
       `(if (-cas!? ~atom_ ~old-val ~new-val) ~then ~else)))

  (def* ^:const ^:no-doc system-newline
    "Prefer `newline`."
    {:deprecated "Encore v3.68.0 (2023-09-25)"}
    newline)

  (defn ^:no-doc -unexpected-arg!
    "Prefer `unexpected-arg!`"
    {:added      "Encore v3.66.0 (2023-08-23)"
     :deprecated "Encore v3.68.0 (2023-09-25)"}
    ([arg        ] (unexpected-arg! arg nil))
    ([arg details] (unexpected-arg! arg details)))

  (def* ^:no-doc -matching-error
    "Prefer `matching-error`."
    {:deprecated "Encore v3.70.0 (2023-10-17)"}
    matching-error)

  (def* ^:no-doc limiter* "Prefer `rate-limiter*`." {:deprecated "Encore v3.73.0 (2023-10-30)"} rate-limiter*)
  (def* ^:no-doc limiter  "Prefer `rate-limiter`."  {:deprecated "Encore v3.73.0 (2023-10-30)"} rate-limiter)

  #?(:cljs (def* ^:no-doc ajax-lite "Prefer `ajax-call`." {:deprecated "Encore v3.74.0 (2023-11-06)"} ajax-call))

  #?(:clj
     (do
       (defmacro ^:no-doc ^:deprecated do-nil   [& body] `(do ~@body nil))
       (defmacro ^:no-doc ^:deprecated do-false [& body] `(do ~@body false))
       (defmacro ^:no-doc ^:deprecated do-true  [& body] `(do ~@body true)))))

(deprecated
  #?(:clj
     (do
       (defn- get-config-opts [opts]
         (let [edn? (= (get opts :as) :edn)
               spec    (get opts :prop)
               spec (if edn? (keyword (str (as-qname spec) "<.edn>")) spec)]
           (dissoc (assoc opts :return :legacy :spec spec) :prop)))

       (defmacro ^:no-doc get-config
         {:deprecated "Encore v3.75.0 (2024-01-29)" :doc "Prefer `get-env`."}
         [opts] `(get-env ~(get-config-opts opts)))

       (defmacro ^:no-doc get-sys-val*
         {:deprecated "Encore v3.75.0 (2024-01-29)" :doc "Prefer `get-env`."}
         ([spec        ] ^:deprecation-nowarn `(get-sys-val* ~spec ~spec ~spec))
         ([spec env    ] ^:deprecation-nowarn `(get-sys-val* ~spec ~env  ~spec))
         ([spec env res]
          (if (str-starts-with? (str *ns*) "taoensso.nippy")
            `(get-env* {:as :str :spec ~spec :env ~env :res ~res}) ; Back compatibility (don't embed)
            `(get-env  {:as :str :spec ~spec :env ~env :res ~res}))))

       (defmacro ^:no-doc get-sys-bool*
         {:deprecated "Encore v3.75.0 (2024-01-29)" :doc "Prefer `get-env`."}
         ([default spec env res] `(get-env {:as :bool :default ~default :spec ~spec :env ~env :res ~res}))
         ([default spec env    ] `(get-env {:as :bool :default ~default :spec ~spec :env ~env          }))
         ([default spec        ] `(get-env {:as :bool :default ~default :spec ~spec                    })))

       (defmacro ^:no-doc read-sys-val*
         {:deprecated "Encore v3.75.0 (2024-01-29)" :doc "Prefer `get-env`."}
         ([spec env res] `(get-env {:as :edn :spec ~spec :env ~env :res ~res}))
         ([spec env    ] `(get-env {:as :edn :spec ~spec :env ~env          }))
         ([spec        ] `(get-env {:as :edn :spec ~spec                    })))

       (defn ^:no-doc get-sys-val
         {:deprecated "Encore v3.66.0 (2023-08-23)" :doc "Prefer `get-env`."}
         ([spec env] (get-env* {:as :str :spec spec :env env}))
         ([spec    ] (get-env* {:as :str :spec spec})))

       (defn ^:no-doc read-sys-val
         {:deprecated "Encore v3.66.0 (2023-08-23)" :doc "Prefer `get-env`."}
         ([spec env] (get-env* {:as :edn :spec spec :env env}))
         ([spec    ] (get-env* {:as :edn :spec spec})))

       (defn ^:no-doc get-sys-bool
         {:deprecated "Encore v3.66.0 (2023-08-23)" :doc "Prefer `get-env`."}
         ([default spec env] (get-env* {:as :bool :default default :spec spec :env env}))
         ([default spec    ] (get-env* {:as :bool :default default :spec spec})))

       (defn ^:no-doc load-edn-config
         {:deprecated "Encore v3.66.0 (2023-08-23)" :doc "Prefer `get-env`."}
         [opts]
         (let [{:keys [error-data validator default]} opts
               have-default? (contains? opts :default)]
           (try
             (when (and validator have-default?) (have? validator default))
             (when-let [{:keys [config] :as m} (get-env* (get-config-opts opts))]
               (let [config
                     (if (and (map? config) (map? default))
                       (nested-merge default config)
                       (do                   config))]
                 (when validator (have? validator config))
                 (assoc m :config config)))

             (catch Throwable t
               (throw
                 (ex-info "[encore/load-edn-config] Error loading edn config"
                   (assoc error-data :opts opts) t)))))))))

(do ; Not currently eliding
  #?(:cljs (def ^:no-doc ^:deprecated js-?win js-?window)))
