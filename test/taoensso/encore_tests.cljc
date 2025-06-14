(ns taoensso.encore-tests
  (:require
   #?(:clj  [clojure.core :as core]
      :cljs [cljs.core    :as core])

   [clojure.test                     :as test :refer [deftest testing is]]
   ;; [clojure.test.check            :as tc]
   ;; [clojure.test.check.generators :as tc-gens]
   ;; [clojure.test.check.properties :as tc-props]
   [clojure.string  :as str]
   [taoensso.truss  :as truss]
   [taoensso.encore :as enc]
   #?(:clj [taoensso.encore.bytes     :as bytes])
   [taoensso.encore.stats             :as stats]
   [taoensso.encore.signals           :as sigs]
   [taoensso.encore-tests.required-ns :as rns])

  #?(:cljs
     (:require-macros
      [taoensso.encore-tests :refer
       [test-macro-alias test-if-cljs test-get-source resolve-sym]])))

(comment
  (remove-ns      'taoensso.encore-tests)
  (test/run-tests 'taoensso.encore-tests))

;;;;

;; (deftest pass (is (= 1 1)))
;; (deftest fail (is (= 1 0)))

(def  ex1 (truss/ex-info "Ex1" {}))
(defn ex1!   [   ] (throw ex1))
(defn throw! [arg] (throw (truss/ex-info "TestEx" {:arg {:value arg :type (type arg)}})))

;;;; Core

(do
  (defn var-fn [n] (* (long n) (long n)))
  #?(:clj  (def ^{:doc "doc:var-clj"}  var-clj  "val:var-clj"))
  #?(:cljs (def ^{:doc "doc:var-cljs"} var-cljs "val:var-cljs"))
  (do      (def ^{:doc "doc:var-cljc"} var-cljc
             #?(:clj  "val:var-cljc/clj"
                :cljs "val:var-cljc/cljs"))))

(do
  (defn- test-fn "doc a" [x] x)
  (enc/defalias                        test-fn-alias-1 test-fn)
  (enc/defalias        ^{:doc "doc b"} test-fn-alias-2 test-fn)
  (enc/defalias        ^{:doc "doc b"} test-fn-alias-3 test-fn {:doc "doc c"})
  (enc/defaliases {:src test-fn :alias test-fn-alias-4 :attrs  {:doc "doc d"}})
  (enc/defaliases {:src test-fn :alias test-fn-alias-5          :doc "doc e"})

  #?(:clj  (defmacro ^:private test-macro [x] `~x))
  #?(:clj  (enc/defalias test-macro-alias test-macro))
  #?(:cljs (enc/defalias var-cljs-alias var-cljs)))

(deftest _defalias
  [(is (= (test-fn-alias-1  :x) :x))
   (is (= (test-macro-alias :x) :x))

   (is (= (:doc (meta #'test-fn-alias-1)) "doc a"))
   (is (= (:doc (meta #'test-fn-alias-2)) "doc b"))
   (is (= (:doc (meta #'test-fn-alias-3)) "doc c"))
   (is (= (:doc (meta #'test-fn-alias-4)) "doc d"))
   (is (= (:doc (meta #'test-fn-alias-5)) "doc e"))

   #?(:cljs (is (= var-cljs-alias                 "val:var-cljs")))
   #?(:cljs (is (= (:doc (meta #'var-cljs))       "doc:var-cljs")))
   #?(:cljs (is (= (:doc (meta #'var-cljs-alias)) "doc:var-cljs")))])

(defprotocol       IMyProtocol (my-protocol-fn [_]))
(deftype MyType [] IMyProtocol (my-protocol-fn [_]))

(deftest _satisfies?
  [(is (true?  (enc/satisfies? IMyProtocol (MyType.))))
   (is (false? (enc/satisfies? IMyProtocol "String")))])

;;;; Cond

(def ^:dynamic *dynamic-var* nil)

(deftest _cond
  [(is (= nil       (enc/cond)))
   (is (= "default" (enc/cond         "default")))
   (is (= "default" (enc/cond :always "default")))
   (is (= nil       (enc/cond false "false")))
   (is (= "default" (enc/cond :let    [a "a"] "default")))
   (is (= "a"       (enc/cond :let    [a "a"] a)))
   (is (= "a"       (enc/cond :if-let [a "a"] a "default")))
   (is (= "default" (enc/cond :when true, :when      [a "a"],   "default")))
   (is (= nil       (enc/cond :when true, :when      [a nil],   "default")))
   (is (= "default" (enc/cond :when true, :when-not  [a false], "default")))
   (is (= "default" (enc/cond :when true, :when-some [a false], "default")))

   (is (= "truthy"  (enc/cond :return-when "truthy", "default")))
   (is (= false     (enc/cond :return-some false,    "default")))
   (is (= "default" (enc/cond :return-some nil,      "default")))

   (is (= "b"       (enc/cond false "a", (not false) "b", "default")))

   (is (= "dv2"     (enc/cond :binding       [*dynamic-var* "dv1"],  :binding       [*dynamic-var* "dv2"],  :else *dynamic-var*)))
   (is (= "dv2"     (enc/cond :wrap (binding [*dynamic-var* "dv1"]), :wrap (binding [*dynamic-var* "dv2"]), :else *dynamic-var*)))

   (is (=
         {:r "r3", :a "a1", :b "b2", :dv "dv2"}
         (let [r_ (atom nil)]
           (enc/cond
             :let         [a "a1", b "b1"]
             :do          (reset! r_ "r1")
             :when        @r_
             :binding     [*dynamic-var* "dv1"]
             :when-let    [c *dynamic-var*]
             :wrap        (binding [*dynamic-var* "dv2"] (reset! r_ "r2"))
             :wrap        (do  (reset! r_ "r3"))
             :wrap        (let [b "b2"])
             :let         [c {:r @r_, :a a, :b b, :dv *dynamic-var*}]
             :do          (reset! r_ nil)
             :return-some @r_
             (not true)   "branch 1"
             :if-let      [d "d1", e false] "branch 2"
             :if-not      [d "d1", e false] c
             "default"))))])

;;;; Submap/s

(deftest _submap?
  [(is      (enc/submap? nil nil))
   (is      (enc/submap? nil {}))
   (is      (enc/submap? {}  {}))
   (is      (enc/submap? {:a {:b :B1 :c :C1}} {:a {:b :B1}}))
   (is      (enc/submap? {:a {:b :B1       }} {:a {:c :submap/nx}}))
   (is (not (enc/submap? {:a {:b :B1 :c nil}} {:a {:c :submap/nx}})))
   (is      (enc/submap? {:a {:b :B1}}        {:a {:b :submap/ex}}))
   (is (not (enc/submap? {:a {:b :B1}}        {:a {:c :submap/ex}})))
   (is      (enc/submap? {:a {:b :B1}}        {:a {:b :submap/some}}))
   (is (not (enc/submap? {:a {:b nil}}        {:a {:b :submap/some}})))
   (is      (enc/submap? {:a 1 :b 2}          {:a odd?
                                               :b even?}))
   (is (not (enc/submap? {:a 1 :b 2}          {:a neg?
                                               :b even?})))])

(deftest _submaps?
  [(is      (enc/submaps? nil  nil))
   (is      (enc/submaps? nil  [  ]))
   (is      (enc/submaps? [  ] nil))
   (is      (enc/submaps? [{}] [{}]))
   (is      (enc/submaps? [{}] [  ]))
   (is (not (enc/submaps? [  ] [{}])))

   (is      (enc/submaps? [{:a :A :b :B} {:a :A}] [{:a :A}]))
   (is (not (enc/submaps? [{:a :A :b :B}        ] [{:a :A} {:a :A}])))
   (is      (enc/submaps? [{} {:a {:b :B1}}] []))
   (is      (enc/submaps? [{} {:a {:b :B1}}] [{} {:a {:b :submap/ex}}]))
   (is (not (enc/submaps? [{} {:a {:b :B1}}] [{} {:a {:b :submap/nx}}])))])

;;;; Loading

#?(:clj  (enc/load-inline "taoensso/encore_tests/loadme.clj")
   :cljs (enc/load-inline "taoensso/encore_tests/loadme.cljs"))

(deftest _load-inline
  [(is (= (get-loaded) #?(:clj :clj, :cljs :cljs)))])

;;;; Reductions

(deftest _reduce-zip
  [(is (= (enc/reduce-zip assoc {}  [:a :b :c]     [1 2 3])   {:a 1, :b 2, :c 3}) "Vec,  normal")
   (is (= (enc/reduce-zip assoc {} '(:a :b :c)    '(1 2 3))   {:a 1, :b 2, :c 3}) "List, normal")
   (is (= (enc/reduce-zip assoc {}  [:a :b :c :a]  [1 2 3 4]) {:a 4, :b 2, :c 3}) "Vec,  replacing")
   (is (= (enc/reduce-zip assoc {} '(:a :b :c :a) '(1 2 3 4)) {:a 4, :b 2, :c 3}) "List, replacing")
   (is (= (enc/reduce-zip assoc {}  [:a :b :c]     [1 2])     {:a 1, :b 2})       "Vec,  uneven")
   (is (= (enc/reduce-zip assoc {} '(:a :b :c)    '(1 2))     {:a 1, :b 2})       "List, uneven")
   (is (= (enc/reduce-zip assoc {}  []             [1])       {})                 "Vec,  empty")
   (is (= (enc/reduce-zip assoc {} '()            '(1))       {})                 "List, empty")
   (is (= (enc/reduce-zip (fn [acc k v] (reduced ::reduced!)) {} [:a :b :c]  [1 2 3]) ::reduced!))
   (is (= (enc/reduce-zip (fn [acc k v] (reduced ::reduced!)) {} [:a :b :c] '(1 2 3)) ::reduced!))

   (is (= (enc/reduce-zip conj [] [:x1    ] [:y1 :y2])     [:x1 :y1]))
   (is (= (enc/reduce-zip conj [] [:x1 :x2] [:y1    ])     [:x1 :y1]))
   (is (= (enc/reduce-zip conj [] [:x1    ] [:y1 :y2] nil) [:x1 :y1 nil :y2]))
   (is (= (enc/reduce-zip conj [] [:x1 :x2] [:y1    ] nil) [:x1 :y1 :x2 nil]))
   (is (= (enc/reduce-zip conj [] [:x1    ] [:y1 :y2] :nx) [:x1 :y1 :nx :y2]))
   (is (= (enc/reduce-zip conj [] [:x1 :x2] [:y1    ] :nx) [:x1 :y1 :x2 :nx]))

   (is (= (enc/reduce-zip conj [] '(:x1    ) '(:y1 :y2))     [:x1 :y1]))
   (is (= (enc/reduce-zip conj [] '(:x1 :x2) '(:y1    ))     [:x1 :y1]))
   (is (= (enc/reduce-zip conj [] '(:x1    ) '(:y1 :y2) nil) [:x1 :y1 nil :y2]))
   (is (= (enc/reduce-zip conj [] '(:x1 :x2) '(:y1    ) nil) [:x1 :y1 :x2 nil]))
   (is (= (enc/reduce-zip conj [] '(:x1    ) '(:y1 :y2) :nx) [:x1 :y1 :nx :y2]))
   (is (= (enc/reduce-zip conj [] '(:x1 :x2) '(:y1    ) :nx) [:x1 :y1 :x2 :nx]))])

(deftest _reduce-multi
  [(is (= (enc/reduce-multi + 0 []) 0))

   (is (= (enc/reduce-multi + 0 (range 1 11))      55))
   (is (= (enc/reduce-multi * 1 (range 1 11)) 3628800))
   (is (= (enc/reduce-multi * 2 (range 1 11)) 7257600))

   (is (= (enc/reduce-multi + 0 * 1     []) [0 1]))
   (is (= (enc/reduce-multi + 0 * 1     []) [0 1]))
   (is (= (enc/reduce-multi + 0 * 1 * 2 []) [0 1 2]))

   (is (= (enc/reduce-multi + 0 * 1     (range 1 11)) [55 3628800]))
   (is (= (enc/reduce-multi + 0 * 1 * 2 (range 1 11)) [55 3628800 7257600]))

   (is (=
         (enc/reduce-multi
           (fn [^long acc ^long in] (let [new (+ acc in)] (if (>= new 100) (reduced [:reduced new in]) new))) 0
           (fn [^long acc ^long in] (let [new (* acc in)] (if (>= new 100) (reduced [:reduced new in]) new))) 1
           (range 1 100))

         [[:reduced 105 14] [:reduced 120 5]]))])

(deftest _reduce-interleave-all
  [(is (= (enc/reduce-interleave-all conj "init" [])              "init"))
   (is (= (enc/reduce-interleave-all conj []     [[:a1 :a2 :a3]]) [:a1 :a2 :a3]))
   (is (= (enc/reduce-interleave-all conj []     [[:a1 :a2 :a3]
                                                  [:b1 :b2]
                                                  [:c1 :c2 :c3 :c4]])
         [:a1 :b1 :c1 :a2 :b2 :c2 :a3 :c3 :c4]))

   (is (= (enc/reduce-interleave-all
            (fn [acc in] (when (= in :b2) (reduced ::reduced!)))
            [] [[:a1 :a2] [:b1 :b2 :b3]])
         ::reduced!))])

(deftest _postwalk
  (let [pwf #(if (int? %) (inc ^long %) %)
        pw  (fn [preserve-seqs? x] (enc/postwalk preserve-seqs? x pwf))]

    [(is (= (pw nil  1)  2))
     (is (= (pw nil 's) 's))

     (is (= (pw nil    [1 2 3]) [2 3 4]))
     (is (= (pw nil   '(1 2 3)) [2 3 4]))
     (is (= (pw :seqs '(1 2 3)) '(2 3 4)))

     (is (= (pw nil   {:a [1 2 3 #{1 2 3 {:a '(1 2 3)}}] 1 "1" 2 nil 3 {1 "1" 2 "2" 3 "3"}}) {:a [2 3 4 #{4 3 2 {:a  [2 3 4]}}], 2 "1", 3 nil, 4 {2 "1", 3 "2", 4 "3"}}))
     (is (= (pw :seqs {:a [1 2 3 #{1 2 3 {:a '(1 2 3)}}] 1 "1" 2 nil 3 {1 "1" 2 "2" 3 "3"}}) {:a [2 3 4 #{4 3 2 {:a '(2 3 4)}}], 2 "1", 3 nil, 4 {2 "1", 3 "2", 4 "3"}}))]))

;;;; Sub fns

(deftest _subvec
  [(is (nil? (enc/subvec nil nil)))
   (is (=
         (vec (for [start (range -1 +3)
                    end   (range -1 +3)]
                [start end (enc/subvec [:a :b] :by-idx start end)]))

         [[-1 -1 nil]
          [-1  0 nil]
          [-1  1 [:a]]
          [-1  2 [:a :b]]
          [ 0 -1 nil]
          [ 0  0 nil]
          [ 0  1 [:a]]
          [ 0  2 [:a :b]]
          [ 1 -1 nil]
          [ 1  0 nil]
          [ 1  1 nil]
          [ 1  2 [:b]]
          [ 2 -1 nil]
          [ 2  0 nil]
          [ 2  1 nil]
          [ 2  2 nil]]))

   (is (nil? (enc/substr nil nil)))
   (is (=
         (vec (for [start (range -3 +4)
                    end   (range -3 +4)]
                [start end (enc/subvec [:a :b] :by-len start end)]))

         [[-3 -3 nil]
          [-3 -2 nil]
          [-3 -1 nil]
          [-3  0 nil]
          [-3  1 [:a]]
          [-3  2 [:a :b]]
          [-3  3 [:a :b]]
          [-2 -3 nil]
          [-2 -2 nil]
          [-2 -1 nil]
          [-2  0 nil]
          [-2  1 [:a]]
          [-2  2 [:a :b]]
          [-2  3 [:a :b]]
          [-1 -3 nil]
          [-1 -2 nil]
          [-1 -1 nil]
          [-1  0 nil]
          [-1  1 [:b]]
          [-1  2 [:b]]
          [-1  3 [:b]]
          [ 0 -3 nil]
          [ 0 -2 nil]
          [ 0 -1 nil]
          [ 0  0 nil]
          [ 0  1 [:a]]
          [ 0  2 [:a :b]]
          [ 0  3 [:a :b]]
          [ 1 -3 nil]
          [ 1 -2 nil]
          [ 1 -1 nil]
          [ 1  0 nil]
          [ 1  1 [:b]]
          [ 1  2 [:b]]
          [ 1  3 [:b]]
          [ 2 -3 nil]
          [ 2 -2 nil]
          [ 2 -1 nil]
          [ 2  0 nil]
          [ 2  1 nil]
          [ 2  2 nil]
          [ 2  3 nil]
          [ 3 -3 nil]
          [ 3 -2 nil]
          [ 3 -1 nil]
          [ 3  0 nil]
          [ 3  1 nil]
          [ 3  2 nil]
          [ 3  3 nil]]))])

;;;; Collections

(deftest _map-entry
  [(is (= (enc/map-entry :k :v) (enc/map-entry :k :v)))
   (is (= (enc/map-entry :k :v) (find         {:k :v} :k)))])

(deftest _get1
  [(is (= (enc/get1 {:a :A}  :b       ::nx) ::nx))
   (is (= (enc/get1 {:a nil} :a       ::nx) nil))
   (is (= (enc/get1 {:a nil} :b :a    ::nx) nil))
   (is (= (enc/get1 {:a nil} :c :b :a ::nx) nil))])

(deftest _get*
  [(is (let [c (enc/counter)] (and (= (enc/get* {:a  :A}                         (do (c) :a) (do (c) ::nx))   :A) (= @c 1))) "truthy v1")
   (is (let [c (enc/counter)] (and (= (enc/get* {:a  :A}                         (do (c) :b) (do (c) ::nx)) ::nx) (= @c 2))) "fallback")
   (is (let [c (enc/counter)] (and (= (enc/get* {:a nil}                         (do (c) :a) (do (c) ::nx))  nil) (= @c 1))) "falsey v1")
   (is (let [c (enc/counter)] (and (= (enc/get* {:a nil}                         (do (c) :b) (do (c) ::nx)) ::nx) (= @c 2))) "fallback")
   (is (let [c (enc/counter)] (and (= (enc/get* {:a nil}             (do (c) :b) (do (c) :a) (do (c) ::nx))  nil) (= @c 2))) "falsey v2")
   (is (let [c (enc/counter)] (and (= (enc/get* {:a nil}             (do (c) :b) (do (c) :d) (do (c) ::nx)) ::nx) (= @c 3))) "fallback")
   (is (let [c (enc/counter)] (and (= (enc/get* {:a nil} (do (c) :c) (do (c) :b) (do (c) :a) (do (c) ::nx))  nil) (= @c 3))) "falsey k3")
   (is (let [c (enc/counter)] (and (= (enc/get* {:a nil} (do (c) :c) (do (c) :b) (do (c) :d) (do (c) ::nx)) ::nx) (= @c 4))) "fallback")])

(deftest _get-in*
  (let [m {:a {:b {:c {:d :D}}}}]
    [(is (= (get-in m [])                  (enc/get-in* m [])))
     (is (= (get-in m [:a])                (enc/get-in* m [:a])))
     (is (= (get-in m [:a :b :c :d])       (enc/get-in* m [:a :b :c :d])))
     ;;
     (is (= (get-in m [])                  (enc/get-in* m [])))
     (is (= (get-in m [:nx]           :nf) (enc/get-in* m [:nx]           :nf)))
     (is (= (get-in m [:a :b  :c :nx] :nf) (enc/get-in* m [:a :b  :c :nx] :nf)))
     (is (= (get-in m [:a :nx :c :d]  :nf) (enc/get-in* m [:a :nx :c :d]  :nf)))
     (is
       (= (let [c (enc/counter)] (enc/get-in* m [:a :b :c :nx] (c)) 0))
       "Avoids unnecessary evaluation of `not-found`.")]))

(deftest _select-nested-keys
  [(is (= (enc/select-nested-keys nil    nil)) {})
   (is (= (enc/select-nested-keys {:a 1} nil)  {}))
   (is (= (enc/select-nested-keys {    } [:a]) {}))

   (is (= (enc/select-nested-keys {:a 1 :b 1 :c 1} [:a :c]) {:a 1 :c 1}))
   (is (= (enc/select-nested-keys {:a 1 :b 1 :c {:ca 2 :cb 2 :cc {:cca 3 :ccb 3} :cd 2} :d 1}
            [:a :b {:c [:ca :cb {:cc [:cca]} :ce]
                    :d [:da :db]} {:e []}])

         {:a 1, :b 1, :c {:ca 2, :cb 2, :cc {:cca 3}}, :d 1}))])

(deftest _vinterleave-all
  [(is (= (enc/vinterleave-all [  ]) []))
   (is (= (enc/vinterleave-all [[]]) []))
   (is (= (enc/vinterleave-all [[:a1 :a2]]) [:a1 :a2]))
   (is (= (enc/vinterleave-all [[:a1 :a2 :a3]
                                [:b1 :b2]
                                [:c1 :c2 :c3 :c4]])
         [:a1 :b1 :c1 :a2 :b2 :c2 :a3 :c3 :c4]))

   (testing "Optimized 2-arity case"
     [(is (= (enc/vinterleave-all [       ] [       ]) [           ]))
      (is (= (enc/vinterleave-all [:a1    ] [       ]) [:a1        ]))
      (is (= (enc/vinterleave-all [       ] [:b1    ]) [:b1        ]))
      (is (= (enc/vinterleave-all [:a1    ] [:b1 :b2]) [:a1 :b1 :b2]))
      (is (= (enc/vinterleave-all [:a1 :a2] [:b1    ]) [:a1 :b1 :a2]))])])

(deftest _update-in
  [(is (= (enc/update-in {:a :A :b :B} [   ]        (fn [_] :x))                :x))
   (is (= (enc/update-in {:a :A :b :B} [:a ]        (fn [_] :x))            {:a :x, :b :B}))
   (is (= (enc/update-in {:a :A :b :B} [:a ] :nf    (fn [_] :x))            {:a :x, :b :B}))
   (is (= (enc/update-in {:a :A :b :B} [:nx] :nf    (fn [x]  x))            {:a :A, :b :B, :nx :nf}))

   (is (= (enc/update-in {           } [:a :b ]     (fn [_] :x))            {:a {:b :x}}))
   (is (= (enc/update-in {           } [:a :b ]     (fn [x] :swap/abort))   {}))
   (is (= (enc/update-in {           } [:a :b ] :nf (fn [x] x))             {:a {:b :nf}}))

   (is (= (enc/update-in {           } [:a :b ] :nf (fn [x] :swap/abort))   {}))
   (is (= (enc/update-in {           } [:a :b ] :nf (fn [x] :swap/abort))   {}))

   (is (= (enc/update-in {           } [:a :b ]     (fn [m] (dissoc m :k))) {:a {:b nil}}))
   (is (= (enc/update-in {:a {:b :B} } [:a :b ]     (fn [_] :swap/dissoc))  {:a {}}))
   (is (= (enc/update-in {:a {:b :B} } [:a :nx]     (fn [_] :swap/dissoc))  {:a {:b :B}}))

   (is (truss/throws? #?(:clj ClassCastException) (enc/update-in {:a :A} [:a :b] (fn [_] :x)))
     "Non-associative val")])

(deftest _contains-in?
  [(is (= (enc/contains-in? {:a {:b {:c :C}}} [:a :b :c])  true))
   (is (= (enc/contains-in? {:a {:b {:c :C}}} [:a :b :nx]) false))
   (is (= (enc/contains-in? {:a {:b {:c :C}}} [:a :b] :c)  true))
   (is (= (enc/contains-in? {:a {:b {:c :C}}} [:a :b] :nx) false))])

(deftest _dissoc-in
  [(is (= (enc/dissoc-in {:a {:b {:c :C :d :D}}} [:a :b :c])    {:a {:b {:d :D}}}))
   (is (= (enc/dissoc-in {:a {:b {:c :C :d :D}}} [:a :b :nx])   {:a {:b {:c :C :d :D}}}))
   (is (= (enc/dissoc-in {:a {:b {:c :C :d :D}}} [:a :b] :c)    {:a {:b {:d :D}}}))
   (is (= (enc/dissoc-in {:a {:b {:c :C :d :D}}} [:a :b] :c :d) {:a {:b {}}}))
   (is (= (enc/dissoc-in {                     } [:a :b :c])    {}))
   (is (= (enc/dissoc-in {:a :A                } [:a :b :c])    {:a :A}))
   (is (= (enc/dissoc-in {                     } [:a :b] :c :d) {}))
   (is (= (enc/dissoc-in {:a :A                } [:a :b] :c :d) {:a :A}))
   (is (truss/throws? #?(:clj ClassCastException) (enc/dissoc-in {:a :A} [:a :b])))])

(deftest _merging
  [(testing "Basics"
     [(is (= (enc/merge)                     nil))
      (is (= (enc/merge nil         nil)     nil))
      (is (= (enc/merge {}          nil)      {}))
      (is (= (enc/merge nil          {})      {}))
      (is (= (enc/merge {:a :A}     nil) {:a :A}))
      (is (= (enc/merge nil     {:a :A}) {:a :A}))

      (is (= (enc/merge {:a 1} {:b 2} {:a 3} {:c 4}) {:a 3, :b 2, :c 4}))])

   (testing "nx (preserve existing)"
     [(is (= (enc/merge-nx {:a 1} {:a 2 :b 2}) {:a 1, :b 2}))
      (is (= (enc/merge-nx {}     {:a 2 :b 2}) {:a 2, :b 2}))
      (is (= (enc/merge-nx {} {:a 2} {:a 3} {:b 4} {:a 5} {:c 6}) {:a 2, :b 4, :c 6}))])

   (testing "Transient support (currently undocumented)"
     [(is (= (enc/merge (transient {})) {}))
      (is (= (enc/merge nil (transient {})) nil))
      (is (= (enc/merge (transient {:a 1}) {:a 2} (transient {:a 3})) {:a 3}))])

   (testing "Preserve left-most metadata"
     [(is (= (meta (enc/merge ^:m1 {}                )) {:m1 true}))
      (is (= (meta (enc/merge ^:m1 {} ^:m2 {}        )) {:m1 true}))
      (is (= (meta (enc/merge nil     ^:m2 {}        )) nil))
      (is (= (meta (enc/merge nil     ^:m2 {} ^:m3 {})) nil))])

   (testing "Cardinality optimizations"
     [(is (= (enc/merge {:a 1 :b 1 :_1 nil} {:b 2 :c 2           }) {:a 1 :b 2 :c 2 :_1 nil})         "|m1| > |m2|")
      (is (= (enc/merge {:a 1 :b 1        } {:b 2 :c 2 :_1 nil   }) {:a 1 :b 2 :c 2 :_1 nil})         "|m1| < |m2|")
      (is (= (enc/merge {:a 1 :c 1        } {:b 2                }) {:a 1 :b 2 :c 1})                 "|m1| > |m1|")
      (is (= (enc/merge {:a 1 :c 1        } {:b 2 :_1 nil :_2 nil}) {:a 1 :b 2 :c 1 :_1 nil :_2 nil}) "|m1| < |m2|")])

   (testing "Special vals"
     [(is (= (enc/merge {:a :A1 :b :B1} {:a :A1 :b :merge/dissoc})      {:a :A1}))
      (is (= (enc/merge {:a :A1}        {:b :B2 :merge/replace? true})  {:b :B2}))
      (is (= (enc/merge {:a :A1}        {:b :B2 :merge/replace? false}) {:a :A1 :b :B2}))])

   (testing "Nesting"
     [(is (= (enc/nested-merge) nil))
      (is (= (enc/nested-merge
               {:a1 :A1 :b1 :B1  :c1 {:a2 :A2 :b2 {:a3 :A3 :b3 :B3  :d1 :D1 :e1 :E1}}}
               {        :b1 :B1* :c1 {        :b2 {        :b3 :B3* :d1 nil :e1 :swap/dissoc}}}
               nil)
            {:a1 :A1, :b1 :B1* :c1 {:a2 :A2 :b2 {:a3 :A3, :b3 :B3* :d1 nil}}}))])])

(deftest _key-sets
  [(is (true?  (enc/ks=      #{}         {})))
   (is (true?  (enc/ks<=     #{}         {})))
   (is (true?  (enc/ks>=     #{}         {})))
   (is (true?  (enc/ks=      #{:a :b}    {:a :A :b :B})))
   (is (false? (enc/ks=      #{:a :b}    {:a :A :b :B :c :C})))
   (is (true?  (enc/ks<=     #{:a :b}    {:a :A :b :B})))
   (is (false? (enc/ks<=     #{:a :b}    {:a :A :b :B :c :C})))
   (is (true?  (enc/ks>=     #{:a :b}    {:a :A :b :B :c :C})))
   (is (false? (enc/ks>=     #{:a :b :c} {:a :A :b :B})))
   (is (true?  (enc/ks-nnil? #{:a :b}    {:a :A :b :B :c nil})))
   (is (false? (enc/ks-nnil? #{:a :b :c} {:a :A :b :B :c nil})))])

;;;; Strings

#?(:clj
   (deftest _utf8-byte-strings
     (let [s enc/a-utf8-str]
       (is (= (-> enc/a-utf8-str enc/str->utf8-ba enc/utf8-ba->str) s)))))

#?(:clj
   (deftest _hex-strings
     [(is (= (enc/ba->hex-str (byte-array  0))    ""))
      (is (= (enc/ba->hex-str (byte-array [0]))   "00"))
      (is (= (enc/ba->hex-str (byte-array [0 1])) "0001"))
      (let [v (vec (range -128 128))]
        (is (= (-> v byte-array enc/ba->hex-str enc/hex-str->ba vec) v)))]))

(deftest _abbreviate-ns
 [(is (= (enc/abbreviate-ns 0 :foo.bar.baz)  :foo.bar.baz))
  (is (= (enc/abbreviate-ns 0 'foo.bar.baz)  'foo.bar.baz))
  (is (= (enc/abbreviate-ns 0 "foo.bar.baz") "foo.bar.baz"))

  (is (= (enc/abbreviate-ns 0 :foo.bar/baz) :f.b/baz))
  (is (= (enc/abbreviate-ns 1 :foo.bar/baz) :f.bar/baz))
  (is (= (enc/abbreviate-ns 2 :foo.bar/baz) :foo.bar/baz))

  (is (= (enc/abbreviate-ns 0 'foo.bar/baz) 'f.b/baz))
  (is (= (enc/abbreviate-ns 1 'foo.bar/baz) 'f.bar/baz))
  (is (= (enc/abbreviate-ns 2 'foo.bar/baz) 'foo.bar/baz))

  (is (= (enc/abbreviate-ns 0 "foo.bar/baz") "f.b/baz"))
  (is (= (enc/abbreviate-ns 1 "foo.bar/baz") "f.bar/baz"))
  (is (= (enc/abbreviate-ns 2 "foo.bar/baz") "foo.bar/baz"))])

;;;; Forms

#?(:clj
   (deftest _forms
     [(is (false? (enc/list-form?  'foo)))
      (is (false? (enc/list-form?  '[foo])))
      (is (true?  (enc/list-form?  '(foo bar))))
      (is (true?  (enc/list-form?  ())))
      (is (true?  (enc/list-form?  '(1 2 3))))
      (is (false? (enc/const-form? 'foo)))
      (is (true?  (enc/const-form? :foo)))]))

;;;; LightAtoms

(deftest _latoms
  (let [la_ (enc/latom 0)]
    [(is (= @la_ (la_) 0))
     (is (= (compare-and-set! la_ 0 1) true))  (is (= @la_ (la_) 1))
     (is (= (compare-and-set! la_ 0 2) false)) (is (= @la_ (la_) 1))
     (is (= (reset! la_ 2)   2))               (is (= @la_ (la_) 2))
     (is (= (swap!  la_ inc) 3))               (is (= @la_ (la_) 3))

     (is (= (reset! la_ {:a 0, :b 0}) {:a 0, :b 0}))
     (is (= (la_ :a inc) 1)) (is (= @la_ (la_) {:a 1, :b 0}))

     (is (= (reset-vals! la_ 4)   [{:a 1, :b 0} 4]))
     (is (= (swap-vals!  la_ inc) [4 5]))]))

;;;; Cache API

(do
  (def ^:private cache-idx_ (atom 0))
  (enc/defn-cached ^:private cached-fn
    {:ttl-ms 1000 :size 2 :gc-every 100}
    "Example cached function"
    ([   ] (swap! cache-idx_ inc))
    ([_  ] (swap! cache-idx_ inc))
    ([_ _] (swap! cache-idx_ inc))))

(deftest _defn-cached
  [(testing "Basics"
     [(is (= (reset! cache-idx_ 0) 0))
      (is (= (cached-fn :cache/del :cache/all) nil))
      (is (= (cached-fn)     1))
      (is (= (cached-fn)     1))
      (is (= (cached-fn "a") 2))
      (is (= (cached-fn "a") 2))
      (is (= (cached-fn "b") 3))
      (is (= (cached-fn "b") 3))
      (is (= (cached-fn "a" "b")) 4)
      (is (= (cached-fn "a" "b")) 4)
      (is (= (cached-fn "a") 2))

      (is (= (cached-fn :cache/fresh)     5))
      (is (= (cached-fn :cache/fresh)     6))
      (is (= (cached-fn)                  6))
      (is (= (cached-fn "a")              2))
      (is (= (cached-fn :cache/fresh "a") 7))

      (is (= (cached-fn "b")            3))
      (is (= @cache-idx_                7))
      (is (= (cached-fn :cache/del "b") nil))
      (is (= @cache-idx_                7))
      (is (= (cached-fn "b")            8))

      (is (= (cached-fn "a"                    7)))
      (is (= (cached-fn :cache/del :cache/all) nil))
      (is (= (cached-fn "a"                    8)))])

   #?(:clj
      (testing "TTL"
        [(is (= (reset! cache-idx_ 0)             0))
         (is (= (cached-fn :cache/del :cache/all) nil))

         (is (= (cached-fn "foo") 1))
         (is (= (cached-fn "foo") 1))

         (do (Thread/sleep 1200) :sleep>ttl-ms)
         (is (= (cached-fn "foo") 2))
         (is (= (cached-fn "foo") 2))]))

   (testing "Max size"
     [(is (= (reset! cache-idx_ 0)             0))
      (is (= (cached-fn :cache/del :cache/all) nil))

      (is (= (cached-fn "infrequent-old")    1))
      (is (= (cached-fn "infrequent-recent") 2))
      (is (= (cached-fn "frequent")          3))

      (do (dotimes [_ 100] (cached-fn "frequent")) :call>gc-every)

      (is (= (cached-fn "frequent")          3) "Cache retained (freq & recent)")
      (is (= (cached-fn "infrequent-recent") 2) "Cache retained (recent)")
      (is (= (cached-fn "infrequent-old")    4) "Cache dropped")])])

(deftest _memoize-last
  [(let [c     (enc/counter)
         f     (enc/memoize-last (fn [& args] (c) (vec args)))
         test1 (fn [args] (dotimes [_ 100] (apply f args)) [@c (apply f args)])]

     [(is (= (test1 [:x1            ]) [1 [:x1            ]]))
      (is (= (test1 [               ]) [2 [               ]]))
      (is (= (test1 [:x1            ]) [3 [:x1            ]]))
      (is (= (test1 [:x1 :x2        ]) [4 [:x1 :x2        ]]))
      (is (= (test1 [:x1 :x2 :x3    ]) [5 [:x1 :x2 :x3    ]]))
      (is (= (test1 [:x1 :x2 :x3    ]) [5 [:x1 :x2 :x3    ]]))
      (is (= (test1 [:x1 :x2 :x3 :x4]) [6 [:x1 :x2 :x3 :x4]]))
      (is (= (test1 [:x1 :x2 :x3 :x4]) [6 [:x1 :x2 :x3 :x4]]))
      (is (= (test1 [:x1            ]) [7 [:x1            ]]))
      (is (= (test1 [               ]) [8 [               ]]))])])

;;;; Swap API

(deftest _swap-in
  [(testing "k0"
     [(let [a (atom :a)]
        [(is (= :x1 (enc/swap-in! a (fn [_] :x1))))
         (is (= :x1 @a))

         (is (= :y1 (enc/swap-in! a (fn [_] (enc/swapped :x1 :y1)))))
         (is (= :x1 @a))

         (is (= :x1 (enc/swap-in! a (fn [v] (enc/swapped :x2 v)))))
         (is (= :x2 @a))

         (is (= :x2 (enc/swap-in! a (fn [v] :swap/abort))))
         (is (= :x2 @a))

         (is (= :x3 (enc/swap-in! a nil (fn [_] :x3))) "nil ks")
         (is (= :x3 @a))

         (is (= :x4 (enc/swap-in! a []  (fn [_] :x4))) "empty ks")
         (is (= :x4 @a))])])

   (testing "k1"
     [(let [a (atom {:a :A :b :B})]
        [(is (= (enc/swap-in! a [:a] (fn [_] :x1)) :x1))
         (is (= @a {:a :x1 :b :B}))

         (is (= (enc/swap-in! a [:a] (fn [v] (enc/swapped :x2 v))) :x1))
         (is (= @a {:a :x2 :b :B}))

         (is (= (enc/swap-in! a [:a] (fn [_] :swap/abort)) :x2))
         (is (= @a {:a :x2 :b :B}) "No change")

         (is (= (enc/swap-in! a [:nx] (fn [_] :swap/abort)) nil))
         (is (= @a {:a :x2 :b :B}) "No change")

         (is (= (enc/swap-in! a [:a] (fn [v] (enc/swapped :swap/abort v))) :x2))
         (is (= @a {:a :x2 :b :B}) "No change")

         (is (= (enc/swap-in! a [:nx] (fn [v] (enc/swapped :swap/abort v))) nil))
         (is (= @a {:a :x2 :b :B}) "No change")

         (is (= (enc/swap-in! a [:nx] (fn [_] :swap/dissoc)) :swap/dissoc))
         (is (= @a {:a :x2 :b :B}) "No change")

         (is (= (enc/swap-in! a [:a] (fn [_] :swap/dissoc)) :swap/dissoc))
         (is (= @a {:b :B}))

         (is (truss/throws? #?(:clj ClassCastException) (enc/swap-in! a [:b :c] (fn [_] :x)))
           "Non-associative val")

         (is (= (enc/swap-in! a [:b] (fn [v] (enc/swapped :swap/dissoc v))) :B))
         (is (= @a {}))])])

   (testing "kn"
     [(let [a (atom {:a {:b1 :B1 :b2 :B2}})]
        [(is (= (enc/swap-in! a [:a :b1] (fn [_] :x1)) :x1))
         (is (= @a {:a {:b1 :x1 :b2 :B2}}))

         (is (= (enc/swap-in! a [:a :b2] (fn [v] (enc/swapped :x2 v))) :B2))
         (is (= @a {:a {:b1 :x1 :b2 :x2}}))

         (is (= (enc/swap-in! a [:a :b2] (fn [_] :swap/abort)) :x2))
         (is (= @a {:a {:b1 :x1 :b2 :x2}}) "No change")

         (is (= (enc/swap-in! a [:nx] (fn [_] :swap/abort)) nil))
         (is (= @a {:a {:b1 :x1 :b2 :x2}}) "No change")

         (is (= (enc/swap-in! a [:a :b1] (fn [v] (enc/swapped :swap/abort v))) :x1))
         (is (= @a {:a {:b1 :x1 :b2 :x2}}) "No change")

         (is (= (enc/swap-in! a [:a :nx] (fn [v] (enc/swapped :swap/abort v))) nil))
         (is (= @a {:a {:b1 :x1 :b2 :x2}}) "No change")

         (is (= (enc/swap-in! a [:a :nx] (fn [_] :swap/dissoc)) :swap/dissoc))
         (is (= @a {:a {:b1 :x1 :b2 :x2}}) "No change")

         (is (= (enc/swap-in! a [:a :b1] (fn [_] :swap/dissoc)) :swap/dissoc))
         (is (= @a {:a {:b2 :x2}}))

         (is (truss/throws? #?(:clj ClassCastException) (enc/swap-in! a [:a :b2 :c] (fn [_] :x)))
           "Non-associative val")

         (is (= (enc/swap-in! a [:a :b2] (fn [v] (enc/swapped :swap/dissoc v))) :x2))
         (is (= @a {:a {}}))])])])

;;;; Strings

(deftest _str-builder
  [(is (zero? (enc/sb-length (enc/str-builder))))])

(deftest _sb-appender
  [(is (= (str (let [s+ (enc/sb-appender)] (s+ "x1a" "x1b" nil "x1c") (s+ nil "x2a" "x2c") (enc/sb-append (s+) "\n"))) "x1ax1bx1c x2ax2c\n"))
   (is (= (str (let [s+ (enc/sb-appender)] (s+ "x1a" "x1b" nil "x1c") (s+ nil nil nil)     (enc/sb-append (s+) "\n"))) "x1ax1bx1c\n"))])

;;;; Math

(deftest _round
  [(is (= (enc/round 1.0) 1) "New/old API (1 arg)")
   (is (= (enc/round 1.5) 2) "New/old API (1 arg)")

   (is (= (enc/round :floor 1.5) 1) "New API (2 args)")
   (is (= (enc/round :ceil  1.5) 2) "New API (2 args)")

   (is (= (enc/round 1.5 :floor) 1) "Old API (2 args)")
   (is (= (enc/round 1.5 :ceil)  2) "Old API (2 args)")

   (is (= (enc/round :floor 1 1.55) 1.5) "New API (3 args)")
   (is (= (enc/round :ceil  1 1.55) 1.6) "New API (3 args)")

   (is (= (enc/round 1.55 :floor 1) 1.5) "Old API (3 args)")
   (is (= (enc/round 1.55 :ceil  1) 1.6) "Old API (3 args)")])

;;;; Callsites, etc.

(defmacro test-if-cljs [caller]
  `(enc/if-cljs
     {:platform :cljs, :caller ~caller}
     {:platform :clj,  :caller ~caller}))

(deftest _if-cljs
  [#?(:clj  (is (= (test-if-cljs :clj)  {:platform :clj,  :caller :clj})))
   #?(:cljs (is (= (test-if-cljs :cljs) {:platform :cljs, :caller :cljs})))])

#?(:clj
   (defmacro test-get-source [caller]
     #_(spit "debug.txt" (str [(boolean (:ns &env)) (:file (meta &form))] "\n") :append true)
     `{:caller    ~caller
       :platform  ~(if (:ns &env) :cljs :clj)
       :form      '~&form
       :*file*    ~(str *file*)
       :env       ~(-> &env       (select-keys [:line :column]))
       :form-meta ~(-> &form meta (select-keys [:line :column :file]))
       :source    ~(enc/get-source &form &env)}))

(comment (test-get-source :repl))

(deftest _get-source
  [#?(:clj
      (let [m (test-get-source :clj)]
        [(is (enc/submap? m {:platform :clj, :caller :clj, :source {:file :submap/some}}))
         (is (= (get-in   m [:source :file]) (get-in m [:*file*])))]))

   #?(:cljs
      (let [m (test-get-source :cljs)]
        [(is (enc/submap?  m {:platform :cljs, :caller :cljs, :source {:file :submap/some}}))
         (is (not= (get-in m [:source :file]) (get-in m [:*file*])))]))])

;;;; Vars, etc.

#?(:clj (defmacro resolve-sym [sym] (keyword (enc/resolve-sym &env sym :may-require-ns))))

(deftest _resolve
  [(is (= (resolve-sym __nx)     nil))
   (is (= (resolve-sym __nx/foo) nil))
   (is (= (resolve-sym str)      #?(:clj :clojure.core/str, :cljs :cljs.core/str)))
   (is (= (let [x "x"] (resolve-sym x)) nil))

   (is (= (resolve-sym                                     var-cljc) :taoensso.encore-tests/var-cljc))
   (is (= (resolve-sym taoensso.encore-tests.unrequired-ns/var-cljc) :taoensso.encore-tests.unrequired-ns/var-cljc))])

(deftest _update-var-root!
  (let [init var-cljc]
    [(is (= (enc/update-var-root! var-cljc (fn [s] (str s ".2"))) (str init ".2")))
     (is (=                       var-cljc                        (str init ".2")))
     (is (= (enc/update-var-root! var-cljc (fn [_] init))              init))
     (is (=                       var-cljc                             init))]))

;;;; Stubs

(do
  (do     (defn-         astub-f1-impl "Doc" ^long [^long y] (* y y #?(:cljs -1    :clj 1))))
  (do     (defn-         astub-fn-impl "Doc"       [&  args] (into [#?(:cljs :cljs :clj :clj)] args)))
  #?(:clj (defmacro      astub-m1-impl "Doc"       [       ] (if (:ns &env)  :cljs :clj)))
  #?(:clj (def ^:private astub-d1-impl "Doc" :my-val))

  (do     (rns/unstub-astub-f1 astub-f1-impl))
  (do     (rns/unstub-astub-fn astub-fn-impl))
  #?(:clj (rns/unstub-astub-m1 astub-m1-impl))
  #?(:clj (rns/unstub-astub-d1 astub-d1-impl)))

(comment
  (clojure.walk/macroexpand-all '(defstub astub-f1))
  (enc/qb 1e6 (astub-f1-impl 5) (astub-f1 5)) ; [39.64 40.71]
  (astub-m1))

(deftest _stubs
  [(is (= (rns/astub-f1 5)        #?(:clj 25              :cljs -25)))
   (is (= (rns/astub-fn :x :y :z) #?(:clj [:clj :x :y :z] :cljs [:cljs :x :y :z])))
   (is (= (rns/astub-m1)          #?(:clj  :clj           :cljs  :cljs)))

   #?(:clj (is (= rns/astub-d1 :my-val)))
   #?(:clj (is (enc/submap? (meta #'rns/astub-d1) {:doc "Doc", :private :submap/nx})))
   #?(:clj (is (enc/submap? (meta #'rns/astub-f1) {:doc "Doc", :private :submap/nx, :arglists '([y])})))
   #?(:clj (is (= (meta (first (:arglists (meta #'rns/astub-f1)))) {:tag 'long})))])

;;;; Env config API

#?(:clj
   (deftest _prep-env-ids
     [(is (= (#'enc/prep-env-ids nil nil    nil)             nil))
      (is (= (#'enc/prep-env-ids nil nil     :a)             ["a"]))
      (is (= (#'enc/prep-env-ids nil nil    [:a])            ["a"]))
      (is (= (#'enc/prep-env-ids nil nil    [:a :a])         ["a"]))
      (is (= (#'enc/prep-env-ids nil nil    [:a :b])         ["a" "b"]))
      (is (= (#'enc/prep-env-ids nil vector [:a :b])         [["a"] ["b"]]))
      (is (= (#'enc/prep-env-ids :p  vector [:a :b])         [["a"] ["b"]]))
      (is (= (#'enc/prep-env-ids :p  vector [:a :b<o>])      [["a"] ["bo"] ["b"]]))
      (is (= (#'enc/prep-env-ids :p  vector [:a :<o1>b<o2>]) [["a"] ["o1bo2"] ["b"]]))
      (is (= (#'enc/prep-env-ids :p  vector [:a<.platform> :c :b<.platform-><o> :c])
            [["a.p"] ["a"] ["c"] ["b.p-o"] ["b.p-"] ["bo"] ["b"]]))]))

(deftest _get-env
  [(is (= (enc/get-env {             }) nil))
   (is (= (enc/get-env {:default  nil}) nil))
   (is (= (enc/get-env {:default :foo}) :foo))

   (is (= (enc/get-env {:return :map             }) nil))
   (is (= (enc/get-env {:return :map :default nil}) {:value nil, :source :default, :platform #?(:clj :clj :cljs :cljs)}))

   (is (enc/submap? (enc/get-env {:return :explain} [:a<.platform> :b])
         {:search
          #?(:clj  [[:prop "a.clj"]  [:env "A_CLJ"]  [:res "a.clj"]  [:prop "a"] [:env "A"] [:res "a"] [:prop "b"] [:env "B"] [:res "b"]]
             :cljs [[:prop "a.cljs"] [:env "A_CLJS"] [:res "a.cljs"] [:prop "a"] [:env "A"] [:res "a"] [:prop "b"] [:env "B"] [:res "b"]])})
     "Basic search, with dynamic platform")

   (is (enc/submap? (enc/get-env {:as :edn :platform :p :return :explain} [:a<.platform><.edn> :b :<platform.>c<.edn>])
         {:search
          [[:prop "a.p.edn"] [:env "A_P_EDN"] [:res "a.p.edn"]
           [:prop "a.p"]     [:env "A_P"]     [:res "a.p"]
           [:prop "a.edn"]   [:env "A_EDN"]   [:res "a.edn"]
           [:prop "a"]       [:env "A"]       [:res "a"]
           [:prop "b"]       [:env "B"]       [:res "b"]
           [:prop "p.c.edn"] [:env "P_C_EDN"] [:res "p.c.edn"]
           [:prop "p.c"]     [:env "P_C"]     [:res "p.c"]
           [:prop "c.edn"]   [:env "C_EDN"]   [:res "c.edn"]
           [:prop "c"]       [:env "C"]       [:res "c"]]})
     "Extensive search, with fixed platform")

   (is (= (enc/submap? (enc/get-env {:return :explain} [#?(:clj :a.clj :cljs :a.cljs) :a.default])
            {:search
             #?(:clj  [[:prop "a.clj"]  [:env "A_CLJ"]  [:res "a.clj"]  [:prop "a.default"] [:env "A_DEFAULT"] [:res "a.default"]])
             #?(:cljs [[:prop "a.cljs"] [:env "A_CLJS"] [:res "a.cljs"] [:prop "a.default"] [:env "A_DEFAULT"] [:res "a.default"]])}))
     "Can also use platform-specific ids")

   #_(do   (is (= ((enc/get-env {:as :edn, :debug/match [:debug/source "(fn [x] (* (long x) (long x)))"]}) 5) 25) "Will eval inline fn"))
   (do     (is (= ((enc/get-env {:as :edn, :debug/match [:debug/source "taoensso.encore-tests/var-fn"]})   5) 25) "Will eval symbol"))
   (do     (is (=  (enc/get-env {:as :edn, :debug/match [:debug/source "taoensso.encore-tests/var-cljc"]}) #?(:clj "val:var-cljc/clj", :cljs "val:var-cljc/cljs"))))
   #?(:clj (is (=  (enc/get-env {:as :edn, :debug/match [:debug/source "taoensso.encore-tests.unrequired-ns/var-cljc"]}) "foreign.val:var-cljc/clj") "Auto require"))

   (testing "Needs :jvm-opts"
     [(is (enc/submap? (enc/get-env {:return :explain} :taoensso.encore-tests.config.str)
            {:value "foo"
             :search
             [[:prop "taoensso.encore-tests.config.str"]
              [:env  "TAOENSSO_ENCORE_TESTS_CONFIG_STR"]
              [:res  "taoensso.encore-tests.config.str"]]}))

      (is (=
            (enc/get-env #?(:clj  [:taoensso.encore-tests.config.clj.str  :taoensso.encore-tests.config.str]
                            :cljs [:taoensso.encore-tests.config.cljs.str :taoensso.encore-tests.config.str]))
            #?(:clj "foo/clj" :cljs "foo/cljs")))

      (is (= (enc/get-env {}          [::nx :taoensso.encore-tests.config.str])  "foo"))
      (is (= (enc/get-env {:as :bool} [::nx :taoensso.encore-tests.config.bool]) true))
      (is (= (enc/get-env {:as :edn}  [::nx :taoensso.encore-tests.config.edn])  {:kw :my-kw :str "foo" :int 5 :vec [:x]}))])])

(comment
  (def foo {:fn (fn [x] (* x x))})
  (enc/get-env {} nil)
  (enc/get-env {:as :edn :debug/match [:debug/source "taoensso.encore/foo"]} nil)
  (enc/get-env {:as :edn :return :explain} [:p1]))

(deftest _binding*
  (is (= :foo (enc/binding* [*dynamic-var* :foo] *dynamic-var*))))

;;;; Misc

(deftest _counters
  (let [c (enc/counter)]
    [(is (= @c        0))
     (is (= (c)       0))
     (is (= @c        1))
     (is (= (c 5)     1))
     (is (= @c        6))
     (is (= (c :+= 2) 8))
     (is (= (c :=+ 2) 8))
     (is (= @c        10))]))

#?(:clj
   (deftest _java-version
     [(is (= 2  (enc/java-version "1.2")))
      (is (= 6  (enc/java-version "1.6.0_23")))
      (is (= 8  (enc/java-version "1.8.0_302")))
      (is (= 9  (enc/java-version "9.0.1")))
      (is (= 11 (enc/java-version "11.0.12")))
      (is (= 16 (enc/java-version "16-ea")))
      (is (= 17 (enc/java-version "17")))
      (is (= 21 (enc/java-version "21")))]))

#?(:clj
   (deftest _refreshing-caching
     (let [v_ (volatile! nil)
           rc (#'enc/refreshing-cache (fn [_] (Thread/sleep 1000) "done"))
           t  (enc/threaded :daemon (vreset! v_ (rc 0 100 "timeout")))]
       (.interrupt t)
       (.join      t)
       (is (= @v_ "timeout")
         "`get-hostname`, `get-host-ip` will return `timeout-val` when their calling thread is
         interrupted while blocking on initializing a cold cache."))))

#?(:clj
   (deftest _secure-rng-mock
     [(is (=
            (let [msrng (enc/secure-rng-mock!!! 5)] [(.nextLong msrng) (.nextDouble msrng)])
            (let [msrng (enc/secure-rng-mock!!! 5)] [(.nextLong msrng) (.nextDouble msrng)])))

      (is (not=
            (let [msrng (enc/secure-rng-mock!!! 5)] [(.nextLong msrng) (.nextDouble msrng)])
            (let [msrng (enc/secure-rng-mock!!! 2)] [(.nextLong msrng) (.nextDouble msrng)])))]))

(deftest _rolling-sequentials
  [(do     (is (= (let [rv (enc/rolling-vector 3)] (dotimes [idx 1e4] (rv idx))      (rv))  [9997 9998 9999])))
   #?(:clj (is (= (let [rl (enc/rolling-list   3)] (dotimes [idx 1e4] (rl idx)) (vec (rl))) [9997 9998 9999])))])

#?(:clj
   (deftest _precache
     (let [c   (enc/counter)
           pcf (enc/pre-cache 3 1 (fn inc-counter [] (c)))]

       [(do (Thread/sleep 1000) "Sleep for cache to initialize (async)")
        (is (= @c 3)  "f was run to initialize cache")
        (is (= (pcf) 0))
        (do (Thread/sleep 1000) "Sleep for cache to replenish (async)")
        (is (= @c 4) "f was run to replenish cache")
        (is (= (pcf) 1))
        (is (= (pcf) 2))
        (is (= (pcf) 3))])))

(deftest _predicates
  [(is      (enc/can-meta? []))
   (is (not (enc/can-meta? "foo")))])

(deftest _instants
  [(is (true?  (enc/inst? (enc/now-inst))))
   (is (false? (enc/inst? (enc/now-udt))))
   (is (true?  (enc/inst? (-> (enc/now-inst) enc/inst->udt enc/udt->inst))))

   (testing "Conversions"
     (let [ref-str  "2024-06-09T21:15:20.170Z"
           ref-udt  1717967720170 #_(.toEpochMilli (java.time.Instant/parse ref-str))
           ref-inst #?(:clj (java.time.Instant/ofEpochMilli ref-udt) :cljs (js/Date. ref-udt))
           ref-dt   #?(:clj (java.util.Date. ref-udt) :cljs nil)]

       [(is (= (enc/as-udt (str ref-udt)) ref-udt))
        (is (= (enc/format-inst ref-inst) ref-str))
        (vec
          (for [src [ref-str ref-udt ref-inst #?(:clj ref-dt)]
                [ref xf]
                [        [ref-udt  enc/as-udt]
                         [ref-inst enc/as-inst]
                 #?(:clj [ref-dt   enc/as-dt])]]

            (is (= (xf src) ref) (str "Instant conversion: " src " -> "ref))))

        #?(:clj
           (let [ff (enc/format-inst-fn {:formatter java.time.format.DateTimeFormatter/ISO_LOCAL_DATE_TIME})]
             (is (= (ff ref-inst) "2024-06-09T21:15:20.17") "Depends on auto zone")))]))])

;;;; Futures

#?(:clj
   (deftest _futures
     (vec
      (for [ex [(enc/virtual-executor) (enc/pool-executor {})] :when ex]
        [(is (= "foo"     (deref (enc/future* ex (do                     "foo")))))
         (is (= "foo"     (deref (enc/future* ex (do (Thread/sleep 1000) "foo")))))
         (is (= "foo"     (deref (enc/future* ex (do (Thread/sleep 1000) "foo")) 4000 "timeout")))
         (is (= "timeout" (deref (enc/future* ex (do (Thread/sleep 1000) "foo")) 100  "timeout")))
         (is (= false (realized? (enc/future* ex (do (Thread/sleep 1000) "foo")))))
         (let [f (enc/future* ex (do (Thread/sleep 1000) "foo"))]
           (is (= false (realized? (do    f))))
           (is (= true  (realized? (do @f f)))))]))))

;;;; LightAtom

(deftest _light-atom
  [(is (= @(enc/latom :foo)  :foo))
   (is (= ((enc/latom :foo)) :foo))
   (is (= (let [la (enc/latom     0)]  [(reset! la 1)   @la (la)])  [1     1      1]))
   (is (= (let [la (enc/latom     0)]  [(swap!  la inc) @la (la)])  [1     1      1]))
   (is (= (let [la (enc/latom     0) ] [(la        inc) @la (la)])  [1     1      1]))
   (is (= (let [la (enc/latom {:a 0})] [(la :a     inc) @la (la)])  [1 {:a 1} {:a 1}]))
   (is (true?  (compare-and-set! (enc/latom 0) 0 1)))
   (is (false? (compare-and-set! (enc/latom 1) 0 1)))])

;;;; Swap API

(deftest _swap-api
  [(testing "Reset variants"
     [(testing "0, n keys"
        [(is (= (let [a (atom             :old)] [(enc/reset-in! a               :new) @a]) [:old :new]))
         (is (= (let [a (atom              nil)] [(enc/reset-in! a [:k1 :k2]     :v2b) @a]) [nil  {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom              nil)] [(enc/reset-in! a [:k1 :k2] :nf :v2b) @a]) [:nf  {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/reset-in! a [:k1 :k2]     :v2b) @a]) [:v2a {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/reset-in! a [:k1 :k3]     :v3a) @a]) [nil  {:k1 {:k2 :v2a, :k3 :v3a}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/reset-in! a [:k1 :k3] :nf :v3a) @a]) [:nf  {:k1 {:k2 :v2a, :k3 :v3a}}]))])

      (testing "1 key"
        [(is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/reset-val! a :k1     :v1b) @a]) [:v1a {:k1 :v1b, :k2 :v2a}]))
         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/reset-val! a :k2     :v2b) @a]) [:v2a {:k1 :v1a, :k2 :v2b}]))
         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/reset-val! a :k2 :nf :v2b) @a]) [:v2a {:k1 :v1a, :k2 :v2b}]))])

      (testing "!?"
        [(is (= (let [a (atom              nil)] [(enc/reset-in!? a           :new) @a]) [true  :new]))
         (is (= (let [a (atom              nil)] [(enc/reset-in!? a            nil) @a]) [false nil]))
         (is (= (let [a (atom              nil)] [(enc/reset-in!? a [:k1 :k2] :v2b) @a]) [true {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/reset-in!? a [:k1 :k2] :v2b) @a]) [true {:k1 {:k2 :v2b}}]))])])

   (testing "Swap variants"
     [(testing "0 keys"
        [(is (= (let [a (atom :old)] [(enc/swap-in! a (fn [old]              :new                )) @a]) [:new :new]))
         (is (= (let [a (atom :old)] [(enc/swap-in! a (fn [old] (enc/swapped :new            old))) @a]) [:old :new]))
         (is (= (let [a (atom :old)] [(enc/swap-in! a (fn [old] (enc/swapped :new      :swap/old))) @a]) [:old :new]))
         (is (= (let [a (atom :old)] [(enc/swap-in! a (fn [old] (enc/swapped :new :swap/changed?))) @a]) [true :new]))
         (is (= (let [a (atom :old)] [(enc/swap-in! a (fn [old] (enc/swapped :swap/abort     old))) @a]) [:old :old]))
         (is (= (let [a (atom :old)] [(enc/swap-in! a (fn [old]              :swap/abort         )) @a]) [:old :old]))])

      (testing "n keys"
        [(is (= (let [a (atom nil)] [(enc/swap-in! a [:k1 :k2]     (fn [old]              :v2b     )) @a]) [:v2b {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom nil)] [(enc/swap-in! a [:k1 :k2]     (fn [old] (enc/swapped :v2b old))) @a]) [nil  {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom nil)] [(enc/swap-in! a [:k1 :k2] :nf (fn [old] (enc/swapped :v2b old))) @a]) [:nf  {:k1 {:k2 :v2b}}]))

         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/swap-in! a [:k1 :k2] (fn [old]              :v2b             )) @a]) [:v2b {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/swap-in! a [:k1 :k2] (fn [old] (enc/swapped :v2b         old))) @a]) [:v2a {:k1 {:k2 :v2b}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/swap-in! a [:k1 :k2] (fn [old] (enc/swapped :swap/abort  old))) @a]) [:v2a {:k1 {:k2 :v2a}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/swap-in! a [:k1 :k2] (fn [old] (enc/swapped :swap/dissoc old))) @a]) [:v2a {:k1 {}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/swap-in! a [:k1 :k2] (fn [old]              :swap/abort      )) @a]) [:v2a {:k1 {:k2 :v2a}}]))
         (is (= (let [a (atom {:k1 {:k2 :v2a}})] [(enc/swap-in! a [:k1 :k2] (fn [old]              :swap/dissoc     )) @a]) [:swap/dissoc {:k1 {}}]))])

      (testing "n keys"
        [(is (= (let [a (atom nil)] [(enc/swap-val! a :k2     (fn [old]              :v2b     )) @a]) [:v2b {:k2 :v2b}]))
         (is (= (let [a (atom nil)] [(enc/swap-val! a :k2     (fn [old] (enc/swapped :v2b old))) @a]) [nil {:k2 :v2b}]))
         (is (= (let [a (atom nil)] [(enc/swap-val! a :k2 :nf (fn [old] (enc/swapped :v2b old))) @a]) [:nf {:k2 :v2b}]))

         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/swap-val! a :k2 (fn [old]              :v2b             )) @a]) [:v2b {:k1 :v1a, :k2 :v2b}]))
         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/swap-val! a :k2 (fn [old] (enc/swapped :v2b         old))) @a]) [:v2a {:k1 :v1a, :k2 :v2b}]))
         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/swap-val! a :k2 (fn [old] (enc/swapped :swap/abort  old))) @a]) [:v2a {:k1 :v1a, :k2 :v2a}]))
         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/swap-val! a :k2 (fn [old] (enc/swapped :swap/dissoc old))) @a]) [:v2a {:k1 :v1a}]))
         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/swap-val! a :k2 (fn [old]              :swap/abort      )) @a]) [:v2a {:k1 :v1a, :k2 :v2a}]))
         (is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/swap-val! a :k2 (fn [old]              :swap/dissoc     )) @a]) [:swap/dissoc {:k1 :v1a}]))])

      (testing "Pull"
        [(is (= (let [a (atom {:k1 :v1a, :k2 :v2a})] [(enc/pull-val! a :k2    ) @a]) [:v2a {:k1 :v1a}]))
         (is (= (let [a (atom {:k1 :v1a          })] [(enc/pull-val! a :k2    ) @a]) [nil  {:k1 :v1a}]))
         (is (= (let [a (atom {:k1 :v1a          })] [(enc/pull-val! a :k2 :nf) @a]) [:nf  {:k1 :v1a}]))])])])

;;;; Name filter

(deftest _name-filter
  [(is (truss/throws? (enc/name-filter  nil)))

   (is (true? ((enc/name-filter :any) "foo")))

   (let [nf (enc/name-filter "a.b.*")]
     [(is (true?  (nf "a.b.c")))
      (is (false? (nf "a.b")))
      (is (false? (nf "a.bX")))])

   (let [nf (enc/name-filter "a.b(.*)")]
     [(is (true?  (nf "a.b.c")))
      (is (true?  (nf "a.b")))
      (is (false? (nf "a.bX")))])

   (is (true?  ((enc/name-filter #{:foo*})  "foo")))
   (is (true?  ((enc/name-filter #{"foo*"}) 'foo)))
   (is (false? ((enc/name-filter #{"foo*"}) 'bar)))

   (is (true?  ((enc/name-filter ["foo" "bar"]) "bar")))
   (is (true?  ((enc/name-filter ["foo" "bar"]) :bar)))
   (is (false? ((enc/name-filter ["foo" "bar"]) :baz)))

   (is (true?  ((enc/name-filter ["foo" "b*"])  "bar")))
   (is (true?  ((enc/name-filter ["foo" "b*"])  :bar)))
   (is (false? ((enc/name-filter ["foo" "b*"])  :qux)))

   (is (false? ((enc/name-filter {:allow :any :disallow :any}) "foo")))
   (is (true?  ((enc/name-filter {:allow :any :disallow  #{}}) "foo")))

   (is (true? ((enc/name-filter {:allow "foo*"}) "foo")))
   (is (true? ((enc/name-filter {:allow "foo*"}) :foobar)))

   (is (false? ((enc/name-filter {:allow "foo*" :disallow "foobar"}) :foobar)))
   (is (true?  ((enc/name-filter {:allow "foo*" :disallow "foobar"}) :foobaz)))])

;;;;

(deftest _rate-limiters
  (let [c1  (enc/counter)
        c2  (enc/counter)
        rl1 (enc/rate-limiter {"1/100" [1 100]})
        rl2 (enc/rate-limiter-once-per    100)]

    (dotimes [_ 4]
      (dotimes [_ 10]
        (when-not (rl1) (c1))
        (when-not (rl2) (c2)))
      (enc/hot-sleep 250))

    [(is (= @c1 4))
     (is (= @c2 4))]))

;;;; Runner

#?(:clj
   (deftest _runner
     [(let [a (atom nil)
            r (enc/runner {:mode :sync})]

        [(is (= (r (fn [] (Thread/sleep 1000) (reset! a :done))) true))
         (is (= @a   :done))
         (is (= @(r) :drained))
         (is (=  (r) nil))
         (is (= @@r  :drained))])

      (let [a (atom [])
            r (enc/runner {:mode :dropping, :buffer-size 3, :debug/init-after 100})]

        [(is (= (vec (for [n (range 6)] (r (fn [] (Thread/sleep 100) (swap! a conj n))))) [true true true false false false]))
         (is (= @a   []))
         (is (= @(r) :drained))
         (is (=  (r) nil))
         (is (= @@r  :drained))
         (is (= @a   [0 1 2]))])

      (let [a (atom [])
            r (enc/runner {:mode :sliding, :buffer-size 3, :debug/init-after 100})]

        [(is (= (vec (for [n (range 6)] (r (fn [] (Thread/sleep 100) (swap! a conj n))))) [true true true false false false]))
         (is (= @a   []))
         (is (= @(r) :drained))
         (is (=  (r) nil))
         (is (= @@r  :drained))
         (is (= @a   [3 4 5]))])

      (let [a (atom [])
            r (enc/runner {:mode :blocking, :buffer-size 3, :debug/init-after 100})]

        [(is (= (vec (for [n (range 6)] (r (fn [] (Thread/sleep 100) (swap! a conj n))))) [true true true false false false]))
         (is (= @a   [0 1]) "Blocked 3x to get 2x executed and 1x executing")
         (is (= @(r) :drained))
         (is (=  (r) nil))
         (is (= @@r  :drained))
         (is (= @a   [0 1 2 3 4 5]))])

      (let [a (atom nil)
            r (enc/runner {:mode :blocking})]

        (binding [*dynamic-var* "bound"] (r (fn [] (Thread/sleep 100) (reset! a *dynamic-var*))))
        [(is (= @(r) :drained))
         (is (=  (r) nil))
         (is (= @a  "bound") "Runner binding conveyance")])

      (let [r (enc/runner {:mode :dropping, :buffer-size 4})] (dotimes [_ 4] (r (fn [] (Thread/sleep 250)))) (is (= (deref (r) 250  ::timeout) ::timeout)))
      (let [r (enc/runner {:mode :dropping, :buffer-size 4})] (dotimes [_ 4] (r (fn [] (Thread/sleep 250)))) (is (= (deref (r) 2500 ::timeout) :drained)))

      (testing "High-volume, cross-thread runner calls"
        ;; May block up to 200 msecs per @(r)
        (every? true?
          (let [n  2e4 ; Laps per run
                fp (enc/future-pool [:ratio 1.0])]
            (flatten
              (for [_ (range 16)]
                [(let [c (enc/counter), r (enc/runner {:mode :sync                    })] (is (== (do (dotimes [_ n] (fp (fn [] (r (fn [] (c)))))) (fp) @(r) @c) n)))
                 (let [c (enc/counter), r (enc/runner {:mode :blocking, :buffer-size 1})] (is (== (do (dotimes [_ n] (fp (fn [] (r (fn [] (c)))))) (fp) @(r) @c) n)))
                 (let [c (enc/counter), r (enc/runner {:mode :dropping, :buffer-size n})] (is (== (do (dotimes [_ n] (fp (fn [] (r (fn [] (c)))))) (fp) @(r) @c) n)))
                 (let [c (enc/counter), r (enc/runner {:mode :sliding,  :buffer-size n})] (is (== (do (dotimes [_ n] (fp (fn [] (r (fn [] (c)))))) (fp) @(r) @c) n)))])))))]))

;;;; Bytes

#?(:clj
   (deftest _bytes
     [(testing "?ba="
        [(is (true?  (bytes/?ba= nil              nil)))
         (is (true?  (bytes/?ba= nil              (byte-array 0))))
         (is (false? (bytes/?ba= nil              (byte-array [1]))))
         ;;
         (is (true?  (bytes/?ba= (byte-array 0)   nil)))
         (is (true?  (bytes/?ba= (byte-array 0)   (byte-array 0))))
         (is (false? (bytes/?ba= (byte-array 0)   (byte-array [1]))))
         ;;
         (is (false? (bytes/?ba= (byte-array [1]) nil)))
         (is (false? (bytes/?ba= (byte-array [1]) (byte-array 0))))
         (is (true?  (bytes/?ba= (byte-array [1]) (byte-array [1]))))
         (is (false? (bytes/?ba= (byte-array [1]) (byte-array [2]))))])

      (testing "ba-lengths"
        [(is (=  (vec (bytes/ba->len    5 (bytes/as-ba [1 2 3]))) [1 2 3 0 0]))
         (is (-> (vec (bytes/ba->sublen 5 (bytes/as-ba [1 2 3]))) truss/throws?))])

      (testing "ba-join" (is (= (vec (bytes/ba-join nil (bytes/as-ba [0]) (bytes/as-ba [1 2]) nil (bytes/as-ba [3 4 5]) nil nil (bytes/as-ba [6]))) [0 1 2 3 4 5 6])))
      (testing "ba-parts"
        (let [ba (bytes/ba-join (bytes/as-ba [1 2]) (bytes/as-ba [3]) (bytes/as-ba [5 6]) (bytes/as-ba [7 8 9]))]
          (is (= (mapv vec (bytes/ba-parts ba 0 2 1 1 0 0 0 1)) [[1 2] [3] [5] [] [] [] [6] [7 8 9]]))))

      (testing "nempty-bas"
        [(false? (bytes/nempty-ba? nil))
         (false? (bytes/nempty-ba? ""))
         (false? (bytes/nempty-ba? (byte-array [])))
         (true?  (bytes/nempty-ba? (byte-array [1])))

         (nil? (bytes/nempty-ba nil))
         (nil? (bytes/nempty-ba ""))
         (nil? (bytes/nempty-ba (byte-array [])))
         (let [ba (byte-array [1])]
           (identical? ba (bytes/nempty-ba ba)))])

      (testing "unsigned-ints"
        [(let [n     Byte/MAX_VALUE] (is (= (bytes/from-ubyte  (bytes/to-ubyte    n)) n)))
         (let [n    Short/MAX_VALUE] (is (= (bytes/from-ushort (bytes/to-ushort   n)) n)))
         (let [n  bytes/range-ubyte] (is (= (bytes/to-ubyte    (bytes/from-ubyte  n)) n)))
         (let [n bytes/range-ushort] (is (= (bytes/to-ushort   (bytes/from-ushort n)) n)))

         (enc/reduce-n (fn [acc unum] (if (= unum (-> unum bytes/from-ubyte  bytes/to-ubyte))  acc (reduced false))) true 0 bytes/range-ubyte)
         (enc/reduce-n (fn [acc unum] (if (= unum (-> unum bytes/from-ushort bytes/to-ushort)) acc (reduced false))) true 0 bytes/range-ushort)
         (enc/reduce-n (fn [acc unum] (if (= unum (-> unum bytes/from-uint   bytes/to-uint))   acc (reduced false))) true 0 1e6)])

      (testing "strings"
        [(let [s enc/a-utf8-str] (is (= (-> s bytes/str->utf8-ba   bytes/utf8-ba->str)   s)))
         (let [s enc/a-utf8-str] (is (= (-> s bytes/?str->?utf8-ba bytes/?utf8-ba->?str) s)))
         (let [s enc/a-utf8-str] (is (=
                                       (vec (bytes/as-ba  s))
                                       (vec (bytes/as-?ba s))
                                       [72 105 32 -32 -78 -84 -32 -78 -66 32 -32 -78 -121 -32 -78 -78 -32 -77
                                        -115 -32 -78 -78 -32 -78 -65 32 -32 -78 -72 -32 -78 -126 -32 -78 -83
                                        -32 -78 -75 -32 -78 -65 -32 -78 -72 32 49 48])))])

      (testing "chars" (let [s enc/a-utf8-str] (is (= (String. (bytes/as-ca s)) s))))
      (testing "parse-buffer-len" (is (= (bytes/parse-buffer-len [1 (bytes/as-ba 3)]) 4)))

      (testing "with-io"
        [(is (=      (bytes/with-in  [din] (bytes/with-out [dos] 1 (.writeByte dos 67)) (.readByte din)) 67))
         (is (= (vec (bytes/with-out [dos] [0 (bytes/as-ba 6)] (.writeByte dos 1))) [1]))])

      (testing "dynamic-uints"
        [(let [ba (bytes/with-out [dos] 16 (bytes/write-dynamic-uint dos   0))]
           [(is (= (bytes/with-in [din] ba (bytes/read-dynamic-uint  din)) 0))
            (is (= (count ba) 1) "1+0=1 bytes")])

         (let [ba (bytes/with-out [dos] 16 (bytes/write-dynamic-uint dos   bytes/range-ubyte))]
           [(is (= (bytes/with-in [din] ba (bytes/read-dynamic-uint  din)) bytes/range-ubyte))
            (is (= (count ba) 2) "1+1=2 bytes")])

         (enc/reduce-n
           (fn [acc unum]
             (let [ba      (bytes/with-out [dos] 16 (bytes/write-dynamic-uint dos unum))]
               (if (= unum (bytes/with-in  [din] ba (bytes/read-dynamic-uint  din)))
                 acc
                 (reduced false))))
           true
           0 1e6)])

      (testing "unsigned-io"
        [(is (= (bytes/with-in [din] (bytes/with-out [dos] 2 (bytes/write-ubyte  dos  bytes/range-ubyte)) (bytes/read-ubyte  din))  bytes/range-ubyte))
         (is (= (bytes/with-in [din] (bytes/with-out [dos] 2 (bytes/write-ushort dos bytes/range-ushort)) (bytes/read-ushort din)) bytes/range-ushort))])

      (testing "dynamic-bas"
        [(let [dba (bytes/with-out [dos] 1 (dotimes [_ 3] (bytes/write-dynamic-ba dos nil)))]
           (bytes/with-in [din] dba
             (let [x1      (bytes/read-dynamic-?ba  din)
                   x2      (bytes/read-dynamic-ba   din)
                   [x3 n3] (bytes/read-dynamic-?ba* din)]

               [(is (=         x1 nil))
                (is (bytes/ba= x2 (bytes/as-ba 0)))
                (is (=         n3 1))
                (is (=         x3 nil))])))

         (let [dba (bytes/with-out [dos] 1 (dotimes [_ 3] (bytes/write-dynamic-ba dos (bytes/as-ba 0))))]
           (bytes/with-in [din] dba
             (let [x1      (bytes/read-dynamic-?ba  din)
                   x2      (bytes/read-dynamic-ba   din)
                   [x3 n3] (bytes/read-dynamic-?ba* din)]

               [(is (=         x1 nil))
                (is (bytes/ba= x2 (bytes/as-ba 0)))
                (is (=         n3 1))
                (is (=         x3 nil))])))

         (let [dba (bytes/with-out [dos] 1 (dotimes [_ 3] (bytes/write-dynamic-ba dos (bytes/as-ba [1 2 3]))))]
           (bytes/with-in [din] dba
             (let [x1      (bytes/read-dynamic-?ba  din)
                   x2      (bytes/read-dynamic-ba   din)
                   [x3 n3] (bytes/read-dynamic-?ba* din)]

               [(is (bytes/ba= x1 (bytes/as-ba [1 2 3])))
                (is (bytes/ba= x2 (bytes/as-ba [1 2 3])))
                (is (=         n3 4))
                (is (bytes/ba= x3 (bytes/as-ba [1 2 3])))])))])

      (testing "dynamic-strs"
        (let [dba
              (bytes/with-out [dos] 1
                (bytes/write-dynamic-str dos nil)
                (bytes/write-dynamic-str dos nil)
                (bytes/write-dynamic-str dos "")
                (bytes/write-dynamic-str dos "")
                (bytes/write-dynamic-str dos enc/a-utf8-str))]

          (bytes/with-in [din] dba
            (let [x1 (bytes/read-dynamic-?str din)
                  x2 (bytes/read-dynamic-str  din)
                  x3 (bytes/read-dynamic-?str din)
                  x4 (bytes/read-dynamic-str  din)
                  x5 (bytes/read-dynamic-?str din)]
              (is (= [x1 x2 x3 x4 x5] [nil "" nil "" enc/a-utf8-str]))))))

      (testing "bitsets"
        (let [sf {:el0 0, :el1 1, :el2 2}
              st {0 :el0, 1 :el1, 2 :el2}]

          [(is (= (bytes/thaw-set st (bytes/freeze-set sf #{              })) nil))
           (is (= (bytes/thaw-set st (bytes/freeze-set sf  [              ])) nil))
           (is (= (bytes/thaw-set st (bytes/freeze-set sf #{:el0          })) #{:el0}))
           (is (= (bytes/thaw-set st (bytes/freeze-set sf  [:el0          ])) #{:el0}))
           (is (= (bytes/thaw-set st (bytes/freeze-set sf  [:el0 :el1 :el0])) #{:el0 :el1}))
           (is (->                   (bytes/freeze-set sf  [:el1 :el3     ])  truss/throws?))

           (let [sf (assoc sf :freeze/skip-unknown? true)] (is (=  (bytes/thaw-set st (bytes/freeze-set sf [:el1 :el3])) #{:el1})))
           (let [sf (assoc sf :el3 3)]                     (is (-> (bytes/thaw-set st (bytes/freeze-set sf [:el1 :el3])) truss/throws?)))
           (let [sf (assoc sf :el3 3)
                 st (assoc st :thaw/skip-unknown? true)]   (is (=  (bytes/thaw-set st (bytes/freeze-set sf [:el1 :el3])) #{:el1})))]))]))

;;;; Printing

(deftest _printing
  [(vec
     (for [args
           [[nil] ["s"] [:kw] ['sym] [8]
            [[nil "s" :kw 'sym 8]] {:k1 nil, :k2 "s", :k3 :kw, :k4 'sym, :k5 8}
            [nil "s"] ["s" nil] ["s" nil nil "s" nil "s"]
            [nil "s" #{{:k1 ["s" "s" :kw 'sym #{8 "s" :kw} {:k1 {:k2 {:k3a "s" :k3b nil}}}] :k2 nil}} nil "s" nil]]

           [fenc fref]
           [[enc/pr      pr]
            [enc/prn     prn]
            [enc/print   print]
            [enc/println println]]]

       (is (=
             (with-out-str (apply fenc args))
             (with-out-str (apply fref args)))
         "Print output matches under default print options")))

   (is (= ["\"s\"" "[\"s\" \"s\" {:k1 \"s\", :k2 [\"s\" 8 8 nil]}]"]
          (binding [*print-readably* nil, *print-level* 1, *print-length* 1]
            [(enc/pr-edn "s")
             (enc/pr-edn ["s" "s" {:k1 "s" :k2 ["s" 8 8 nil]}])])))])

#?(:cljs
   (deftest _json
     (let [x {:k [:k "s" 1 1.2 {"s" :a/b} enc/a-utf8-str (enc/as-inst 1711619032283)]}]
       [(is (= (enc/pr-json x) "{\"k\":[\"k\",\"s\",1,1.2,{\"s\":\"a/b\"},\"Hi ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸ 10\",\"2024-03-28T09:43:52.283Z\"]}"))
        (is (= (enc/read-json      (enc/pr-json x)) {"k" ["k" "s" 1 1.2 {"s" "a/b"} "Hi ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸ 10" "2024-03-28T09:43:52.283Z"]}))
        (is (= (enc/read-json true (enc/pr-json x)) {:k  ["k" "s" 1 1.2 {:s  "a/b"} "Hi ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸ 10" "2024-03-28T09:43:52.283Z"]}))])))

;;;; Stats

(deftest sorted-nums
  [(testing "Sorted longs"
     [(is (= (vec (stats/sorted-longs nil))               []))
      (is (= (vec (stats/sorted-longs []))                []))
      (is (= (vec (stats/sorted-longs [1]))               [1]))
      (is (= (vec (stats/sorted-longs '(3 2 1)))          [1 2 3]))
      (is (= (vec (stats/sorted-longs [1 2 3 4 5 4 3 2])) [1 2 2 3 3 4 4 5]))])

   (testing "Sorted doubles"
     [(is (= (vec (stats/sorted-doubles nil))               []))
      (is (= (vec (stats/sorted-doubles []))                []))
      (is (= (vec (stats/sorted-doubles [1]))               [1.0]))
      (is (= (vec (stats/sorted-doubles '(3 2 1)))          [1.0 2.0 3.0]))
      (is (= (vec (stats/sorted-doubles [1 2 3 4 5 4 3 2])) [1.0 2.0 2.0 3.0 3.0 4.0 4.0 5.0]))])

   (testing "Sorted nums"
     [(is (= (vec (stats/sorted-nums [1   3   2]))   [1   2   3]))
      (is (= (vec (stats/sorted-nums [1.0 3.0 2.0])) [1.0 2.0 3.0]))
      (is (= (vec (stats/sorted-nums [1.0 3   2]))   [1.0 2.0 3.0]))
      (is (= (vec (stats/sorted-nums [1   3.0 2.0])) [1   2   3]))])])

(deftest weighted-nth
  [(is (= (#'stats/weighted-nth 0.5  [1  3])  2.0))
   (is (= (#'stats/weighted-nth 0.5  [1 10])  5.5))
   (is (= (#'stats/weighted-nth 0.75 [1 10]) 7.75))])

(deftest percentiles
  [(is (= (stats/percentiles                       [1   5   2   4   3])   [1   2   3   4   5   5   5   5]))
   (is (= (stats/percentiles                       [1.0 5.0 2.0 4.0 3.0]) [1.0 2.0 3.0 4.0 5.0 5.0 5.0 5.0]))
   (is (= (stats/percentiles (stats/sorted-longs   [1.0 5.0 2.0 4.0 3.0]))[1   2   3   4   5   5   5   5]))
   (is (= (stats/percentiles (stats/sorted-doubles [1   5   2   4   3]))  [1.0 2.0 3.0 4.0 5.0 5.0 5.0 5.0]))])

;;;;

(defn- sstats-approx== [signf ss1 ss2]
  (reduce-kv
    (fn [acc k v]
      (if (enc/approx== signf v (get ss2 k))
        true
        (reduced false)))
    true
    ss1))

(comment (sstats-approx== 0.001 {:a 100 :b 100} {:a 100 :b 100.0001}))

(do
  (defn- rand-longs   [n] (into [] (repeatedly n #(- ^long   (rand-int 2000) 1000))))
  (defn- rand-doubles [n] (into [] (repeatedly n #(- ^double (rand     2000) 1000.0)))))

(defn- ss-merging-error [n-samples n-sample-size n-tests]
  (enc/reduce-n
    (fn [acc _]
      (let [samples (repeatedly n-samples #(rand-longs n-sample-size))
            ssstats (mapv    stats/summary-stats samples)
            approx  @(reduce stats/summary-stats-merge nil ssstats)
            exact   @(stats/summary-stats (reduce into [] samples))
            pass?
            (and
              (= (:n    exact) (:n    approx))
              (= (:mean exact) (:mean approx)))]

        (if pass? nil (reduced [exact approx]))))
    nil
    n-tests))

(comment (ss-merging-error 100 1000 10))

(let [key-idx
      (into {}
        (map-indexed (fn [n k] [k n])
          [:n :sum :min :max :last :p25 :p50 :p75 :p90 :p95 :p99
           :mean :var :mad :var-sum :mad-sum :meta]))

      comparator (fn [k1 k2] (< (long (get key-idx k1 -1)) (long (get key-idx k2 -1))))]

  (defn- sorted-sstats [m]
    (assoc
      (apply sorted-map-by comparator (interleave (keys m) (vals m)))
      :meta (meta m))))

(deftest summary-stats
  [(is (= (stats/summary-stats nil)           nil))
   (is (= (stats/summary-stats [])            nil))
   (is (= (stats/summary-stats-merge nil nil) nil))

   (is (enc/submap? @(stats/summary-stats [0  ]) {:sum 0,   :min 0,   :max 0,   :p99 0}))
   (is (enc/submap? @(stats/summary-stats [1  ]) {:sum 1,   :min 1,   :max 1,   :p99 1}))
   (is (enc/submap? @(stats/summary-stats [1.0]) {:sum 1.0, :min 1.0, :max 1.0, :p99 1.0}))

   (let [ss (stats/summary-stats [1 2 3])]
     [(is (= @ss @(stats/summary-stats  ss)) "(summary-stats <ss>)")
      (is (= @ss @(stats/summary-stats @ss)) "(summary-stats <map>)")])

   (is
     (= (sorted-sstats @(stats/summary-stats (range 1 1001)))
       {:n 1000, :sum 500500, :min 1, :max 1000, :last 1000, :p25 251, :p50 501, :p75 750, :p90 900, :p95 950, :p99 990,
        :mean 500.5, :var 83333.25, :mad 250.0, :var-sum 8.333325E7, :mad-sum 250000.0, :meta {:floats? false}}))

   (is
     (= (sorted-sstats @(stats/summary-stats (range 0.5 1000)))
       {:n 1000, :sum 500000.0, :min 0.5, :max 999.5, :last 999.5, :p25 250.5, :p50 500.5, :p75 749.5, :p90 899.5, :p95 949.5, :p99 989.5,
        :mean 500.0, :var 83333.25, :mad 250.0, :var-sum 8.333325E7, :mad-sum 250000.0, :meta {:floats? true}}))

   (is
     (= (sorted-sstats
          @(stats/summary-stats-merge
             (stats/summary-stats (range 0   900))
             (stats/summary-stats (range 200 500))))

       {:n 1200, :sum 509400, :min 0, :max 899, :last 499, :p25 238, :p50 425, :p75 612, :p90 724, :p95 762, :p99 792,
        :mean 424.5, :var 52499.916666666664, :mad 187.5, :var-sum 6.29999E7, :mad-sum 225000.0, :meta {:floats? false}}))

   (is
     (= (sorted-sstats
          @(stats/summary-stats-merge
             (stats/summary-stats (range 0.5   900))
             (stats/summary-stats (range 200.5 500))))

       {:n 1200, :sum 510000.0, :min 0.5, :max 899.5, :last 499.5, :p25 238.0, :p50 425.5, :p75 612.0, :p90 724.5, :p95 762.0, :p99 792.0,
        :mean 425.0, :var 52499.916666666664, :mad 187.5, :var-sum 6.29999E7, :mad-sum 225000.0, :meta {:floats? true}}))

   (is
     (= (sorted-sstats
          @(stats/summary-stats-merge
             ;; Mixed long/double vals
             (stats/summary-stats (range 0     900))
             (stats/summary-stats (range 200.5 500))))

       {:n 1200, :sum 509550.0, :min 0.0, :max 899.0, :last 499.5, :p25 237.625, :p50 425.125, :p75 611.625, :p90 724.125, :p95 761.625, :p99 791.625,
        :mean 424.625, :var 52499.916666666664, :mad 187.5, :var-sum 6.29999E7, :mad-sum 225000.0, :meta {:floats? true}}))

   (is (nil? (ss-merging-error 10 100 10)))

   (let [ssb (stats/summary-stats-buffered {:buffer-size 10})]
     (dotimes [n 1e5] (ssb n))
     [(is (enc/submap? @@ssb {:n 100000 :min 0 :max 99999}))
      (is (enc/str-starts-with? (str ssb) "taoensso.encore.stats.SummaryStatsBuffered[{:n 0, :pending 0, :merged 9091}"))])])

;;;;

(deftest _formatting
  [(testing "format-num-fn"
     (let [fmt0 (enc/format-num-fn 0 0)
           fmt2 (enc/format-num-fn 2 2)]
       [(is (= (fmt0 123123123.5555) "123,123,124"))
        (is (= (fmt2 123123123.5555) "123,123,123.56"))
        (is (= (fmt2 123123123)      "123,123,123.00"))
        (is (= (fmt2 123)            "123.00"))]))

   (testing "format-nsecs"
     [(is (= (enc/format-nsecs 0)        "0ns"))
      (is (= (enc/format-nsecs 10)      "10ns"))
      (is (= (enc/format-nsecs 1000)     "1μs"))
      (is (= (enc/format-nsecs 1e6)      "1ms"))
      (is (= (enc/format-nsecs 1.0e9)  "1.00s"))
      (is (= (enc/format-nsecs 1.5e9)  "1.50s"))
      (is (= (enc/format-nsecs 6.0e10) "1.00m"))])])

;;;; Signals API

;;; Misc

(deftest _comp-xfn
  [(testing "Application order"
     (let [v_ (atom []), conj! (fn [idx] (swap! v_ conj idx))]
       [(is (= ((sigs/comp-xfn
                  (sigs/comp-xfn
                    (fn [in] (conj! 1) in)
                    (fn [in] (conj! 2) in)
                    (fn [in] (conj! 3) in)
                    (fn [in] (conj! 4) in)
                    (fn [in] (conj! 5) in))
                  (fn   [in] (conj! 6) in)) "in") "in"))
        (is (= @v_ [1 2 3 4 5 6]))]))

   (testing "nil fns acts as identity"
     [(is (= 0 ((sigs/comp-xfn nil) 0)))
      (is (= 0 ((sigs/comp-xfn [])  0)))
      (is (= 0 ((sigs/comp-xfn [identity]) 0)))
      (is (= 2 ((sigs/comp-xfn [nil inc nil inc]) 0)))
      (is (= 1 ((sigs/comp-xfn  nil inc) 0)))
      (is (= 1 ((sigs/comp-xfn  inc nil) 0)))
      (is (= 1 ((sigs/comp-xfn  inc nil nil) 0)))
      (is (= 1 ((sigs/comp-xfn  nil inc nil) 0)))
      (is (= 1 ((sigs/comp-xfn  nil nil inc) 0)))
      (is (= 3 ((sigs/comp-xfn  nil inc nil inc nil inc) 0)))])

   (testing "nil results short-circuit (skip remaining fns)"
     [(is (nil? ((sigs/comp-xfn      (fn [_] nil) #(/ % 0))  0)))
      (is (nil? ((sigs/comp-xfn  inc (fn [_] nil) #(/ % 0))  0)))
      (is (nil? ((sigs/comp-xfn [inc (fn [_] nil) #(/ % 0)]) 0)))])])

;;; Spec filters

(defn- sf-allow?
  ([[kind-spec kind] [ns-spec ns] [id-spec id] [min-level-spec level]] (if-let [sf (sigs/spec-filter kind-spec ns-spec id-spec min-level-spec)] (sf kind ns id level) true))
  ([                 [ns-spec ns] [id-spec id] [min-level-spec level]] (if-let [sf (sigs/spec-filter nil       ns-spec id-spec min-level-spec)] (sf      ns id level) true))
  ([                 [ns-spec ns]              [min-level-spec level]] (if-let [sf (sigs/spec-filter nil       ns-spec nil     min-level-spec)] (sf      ns    level) true)))

(deftest _spec-filters
  [(testing "Levels"
     [(is (=               (sigs/valid-level-int      -10) -10))
      (is (=               (sigs/valid-level-int    :info)  50))
      ;; (is (->>          (sigs/valid-level-int      nil) (truss/throws? :common "Invalid level"))) ; Macro-time error
      ;; (is (->>          (sigs/valid-level-int     :bad) (truss/throws? :common "Invalid level"))) ; Macro-time error
      (is    (->> ((fn [x] (sigs/valid-level-int x))  nil) (truss/throws? :common "Invalid level")))
      (is    (->> ((fn [x] (sigs/valid-level-int x)) :bad) (truss/throws? :common "Invalid level")))

      (is      (sigs/level>= :error  :info))
      (is      (sigs/level>= :error  :error))
      (is (not (sigs/level>= :info   :error)))
      (is      (sigs/level>= :low--- -10))])

   (testing "update-min-level"
     [(is (=   (sigs/update-min-level nil   nil nil    nil)    nil))
      (is (=   (sigs/update-min-level nil   nil nil  :info)  :info))
      (is (=   (sigs/update-min-level :info nil nil :error) :error))
      (is (->> (sigs/update-min-level nil   nil nil  :__nx) (truss/throws? :common "Invalid level")))

      (is (=   (sigs/update-min-level nil nil nil   [["ns1" :info]]) [["ns1" :info]]))
      (is (=   (sigs/update-min-level nil nil "ns1" :info)           [["ns1" :info]]))

      (is (->> (sigs/update-min-level nil nil "ns1" [["ns1" :info]]) (truss/throws? :common "Invalid level")))
      (is (->> (sigs/update-min-level nil nil nil   [["ns1" :__nx]]) (truss/throws? :common "Invalid level")))
      (is (->> (sigs/update-min-level nil nil -1    :info)           (truss/throws? :common "Invalid name filter")))

      (is (=   (sigs/update-min-level :debug nil "ns1" :info) [["ns1" :info] ["*" :debug]]))
      (is (=   (->
                 :debug
                 (sigs/update-min-level nil  "ns1" :info)
                 (sigs/update-min-level nil  "ns2" :trace)
                 (sigs/update-min-level nil #"ns4" :debug)
                 (sigs/update-min-level nil  "ns3" -20)
                 (sigs/update-min-level nil  "ns2" :warn)
                 (sigs/update-min-level nil #"ns4" :info)
                 (pr-str) ; Since (not= #"ns" #"ns"), etc.
                 )
            "[[#\"ns4\" :info] [\"ns2\" :warn] [\"ns3\" -20] [\"ns1\" :info] [\"*\" :debug]]"))

      (is (=   (sigs/update-min-level [["ns1" :info]]  nil      nil :debug)  :debug))
      (is (=   (sigs/update-min-level :info            :my-kind nil :warn)   {:default :info, :my-kind :warn}))
      (is (=   (sigs/update-min-level {:default :info} :my-kind nil :warn)   {:default :info, :my-kind :warn}))
      (is (=   (sigs/update-min-level {:my-kind :info} nil      nil :warn)   {:my-kind :info, :default :warn}))
      (is (=   (sigs/update-min-level {:default :info} nil      nil :warn)   :warn))
      (is (=   (sigs/update-min-level nil              :my-kind "ns1" :info) {:my-kind [["ns1" :info]]}))
      (is (=   (sigs/update-min-level {:default :info} nil      "ns1" :info) [["ns1" :info] ["*" :info]]))
      (is (=   (sigs/update-min-level ["a" :info] nil nil [["b" :trace] ["c" :info] ["*" :debug]])
            (do                                           [["b" :trace] ["c" :info] ["*" :debug]]))
        "Given min-level can be full replacement [[nf-filter min-level] ...] spec.")])

   (testing "[ns level] filter basics"
     [(is (true?  (sf-allow? [nil   nil] [nil     nil])))
      (is (true?  (sf-allow? [nil "ns1"] [nil   :info])))
      (is (true?  (sf-allow? [nil "ns1"] [:info :info])))
      (is (false? (sf-allow? [nil "ns1"] [:warn :info])))

      (is (true?  (sf-allow? ["ns1"          "ns1"] [nil :info])))
      (is (false? (sf-allow? ["ns2"          "ns1"] [nil :info])))
      (is (true?  (sf-allow? ["ns*"          "ns1"] [nil :info])))
      (is (true?  (sf-allow? [#{"ns1" "ns2"} "ns1"] [nil :info])))
      (is (false? (sf-allow? [#{"ns2" "ns3"} "ns1"] [nil :info])))
      (is (true?  (sf-allow? [#"ns(\d*)?"    "ns5"] [nil :info])))
      (is (true?  (sf-allow? [#"ns(\d*)?"     "ns"] [nil :info])))

      (is (true?  (sf-allow? [{:allow #{"ns1"}}  "ns1"] [nil :info])))
      (is (true?  (sf-allow? [{:allow #{"ns*"}}  "ns1"] [nil :info])))
      (is (true?  (sf-allow? [{:allow #{"*"}}    "ns1"] [nil :info])))
      (is (true?  (sf-allow? [{:allow #{:any}}   "ns1"] [nil :info])))
      (is (true?  (sf-allow? [{:allow "*"}       "ns1"] [nil :info])))
      (is (true?  (sf-allow? [{:allow :any}      "ns1"] [nil :info])))
      (is (false? (sf-allow? [{:allow #{}}       "ns1"] [nil :info])))

      (is (true?  (sf-allow? [{:allow #{"a.*.c"}}    "a.b3.c"] [nil :info])))
      (is (true?  (sf-allow? [{:allow #{"a.*.c"}} "a.b1.b2.c"] [nil :info])))

      (is (true?  (sf-allow? [{:disallow #{}}     "ns1"] [nil :info])))
      (is (false? (sf-allow? [{:disallow #{"*"}}  "ns1"] [nil :info])))
      (is (false? (sf-allow? [{:disallow #{:any}} "ns1"] [nil :info])))
      (is (false? (sf-allow? [{:disallow "*"}     "ns1"] [nil :info])))
      (is (false? (sf-allow? [{:disallow :any}    "ns1"] [nil :info])))

      (is (false? (sf-allow? [{:allow :any :disallow :any} "ns1"] [nil :info])) "Disallow > allow")

      (is (false? (sf-allow? ["ns1" nil] [nil :info]))                                          "ns spec without ns")
      (is (->>    (sf-allow? [nil   nil] [:info nil]) (truss/throws? :common "Invalid level")) "level spec without level")])

   (testing "[ns level] filter with ns-specific min-levels"
     [(is (true?  (sf-allow? [nil "ns1"] [[["*"   :info]                          ]  :info])))
      (is (false? (sf-allow? [nil "ns1"] [[["*"   :info]                          ]  :debug])))
      (is (true?  (sf-allow? [nil "ns1"] [[["ns1" :info] ["ns1" :warn] ["*" :warn]]  :info])) "Match sequentially")
      (is (false? (sf-allow? [nil "ns1"] [[["ns1" :warn] ["ns1" :info] ["*" :warn]]  :info])) "Match sequentially")

      (is (true?  (sf-allow? [nil   "ns.public.foo"] [[["ns.internal.*" :warn] ["ns.public.*" :info] ["*" :warn]] :info])))
      (is (false? (sf-allow? [nil "ns.internal.foo"] [[["ns.internal.*" :warn] ["ns.public.*" :info] ["*" :warn]] :info])))

      (is (->>    (sf-allow? [nil "ns1"] [[["*" :info]] nil]) (truss/throws? :common "Invalid level")))
      (is (->>    (sf-allow? [nil   nil] [[["*" :info]] nil]) (truss/throws? :common "Invalid level")))])

   (testing "[kind ns id level] filter basics"
     [(is (true?  (sf-allow? [nil nil] [nil   nil] [nil   nil] [nil nil])))
      (is (true?  (sf-allow? [:k1 :k1] [nil   nil] [nil   nil] [nil nil])))
      (is (false? (sf-allow? [:k1 :k2] [nil   nil] [nil   nil] [nil nil])))

      (is (true?  (sf-allow? [:k1 :k1] [:ns1 :ns1] [nil   nil] [nil nil])))
      (is (false? (sf-allow? [:k1 :k1] [:ns1 :ns2] [nil   nil] [nil nil])))

      (is (true?  (sf-allow? [:k1 :k1] [:ns1 :ns1] [:id1 :id1] [nil nil])))
      (is (false? (sf-allow? [:k1 :k1] [:ns1 :ns1] [:id1 :id2] [nil nil])))

      (is (true?  (sf-allow? [:k1 :k1] [:ns1 :ns1] [:id1 :id1] [{:k1 10} 10])))
      (is (false? (sf-allow? [:k1 :k1] [:ns1 :ns1] [:id1 :id1] [{:k1 20} 10])))

      (is (true?  (sf-allow? [:k1 :k1] [:ns1 :ns1] [:id1 :id1] [{:default 10} 10])))
      (is (false? (sf-allow? [:k1 :k1] [:ns1 :ns1] [:id1 :id1] [{:default 20} 10])))])

   (testing "Compile-time arity (1), allow nils!"
     [(is (true?  ((sigs/spec-filter {:kind-filter :k1}) {:kind :k1})))
      (is (false? ((sigs/spec-filter {:kind-filter :k1}) {:kind :k2})))
      (is (true?  ((sigs/spec-filter {:kind-filter :k1}) {:kind nil})) "Allow nil")

      (is (true?  ((sigs/spec-filter {:ns-filter :ns1}) {:ns :ns1})))
      (is (false? ((sigs/spec-filter {:ns-filter :ns1}) {:ns :ns2})))
      (is (true?  ((sigs/spec-filter {:ns-filter :ns1}) {:ns  nil})) "Allow nil")

      (is (true?  ((sigs/spec-filter {:id-filter :id1}) {:id :id1})))
      (is (false? ((sigs/spec-filter {:id-filter :id1}) {:id :id2}))) ;X
      (is (true?  ((sigs/spec-filter {:id-filter :id3}) {:id  nil})) "Allow nil")

      (is (true?  ((sigs/spec-filter {:min-level :info}) {:level  :info})))
      (is (false? ((sigs/spec-filter {:min-level :info}) {:level :debug})))
      (is (true?  ((sigs/spec-filter {:min-level :info}) {:level    nil})) "Allow nil")])

   (testing "Local API"
     [(is (nil? (enc/set-var-root! rns/*rt-call-filter* nil)))
      (is (= (rns/get-filters)              nil))
      (is (= (rns/set-kind-filter!     "*") {:kind-filter "*"                                                  }))
      (is (= (rns/set-ns-filter!       "*") {:kind-filter "*", :ns-filter "*"                                  }))
      (is (= (rns/set-id-filter!       "*") {:kind-filter "*", :ns-filter "*", :id-filter "*"                  }))
      (is (= (rns/set-min-level! nil :info) {:kind-filter "*", :ns-filter "*", :id-filter "*", :min-level :info}))
      (is (= @rns/*rt-call-filter*          {:kind-filter "*", :ns-filter "*", :id-filter "*", :min-level :info}))
      (is (= (rns/get-filters)    {:runtime {:kind-filter "*", :ns-filter "*", :id-filter "*", :min-level :info}}))
      (is (= (rns/get-min-levels) {:runtime                                                               :info}))

      (is (= (rns/without-filters (rns/get-filters))    nil))
      (is (= (rns/without-filters (rns/get-min-levels)) nil))

      (is (enc/submap? (rns/with-kind-filter "-" @rns/*rt-call-filter*) {:kind-filter "-"}))
      (is (enc/submap? (rns/with-ns-filter   "-" @rns/*rt-call-filter*) {:ns-filter   "-"}))
      (is (enc/submap? (rns/with-id-filter   "-" @rns/*rt-call-filter*) {:id-filter   "-"}))

      (is (enc/submap? (rns/with-min-level :kind1       100 @rns/*rt-call-filter*) {:min-level {:default :info, :kind1         100  }}))
      (is (enc/submap? (rns/with-min-level :kind1 "ns1" 100 @rns/*rt-call-filter*) {:min-level {:default :info, :kind1 [["ns1" 100]]}}))
      (is (enc/submap?
            (rns/with-min-level :kind1 "ns1" :warn
              {:l1 (rns/get-min-levels)
               :l2 (rns/get-min-levels :kind1)
               :l3 (rns/get-min-levels :kind1 "ns1")})

            {:l1 {:runtime :info}, :l2 {:runtime :info}, :l3 {:runtime :warn}}))

      (is (false? (rns/with-ns-filter "-"          (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))
      (is (true?  (rns/with-ns-filter "ns1"        (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))

      (is (false? (rns/with-kind-filter "-"        (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))
      (is (true?  (rns/with-kind-filter "kind1"    (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))

      (is (false? (rns/with-id-filter "-"          (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))
      (is (true?  (rns/with-id-filter "id1"        (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))

      (is (false? (rns/with-min-level :kind1 :warn (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))
      (is (true?  (rns/with-min-level :kind2 :warn (rns/*rt-call-filter* :kind1 "ns1" :id1 :info))))

      (is (enc/submap? (rns/with-min-level :kind1 "ns2" 100 @rns/*rt-call-filter*) {:min-level {:default :info, :kind1 [["ns2" 100]]}}))
      (is (true?       (rns/with-min-level :kind1 "ns2" 100 (rns/*rt-call-filter* :kind1 "ns1" :id 50))) "Fall back to :default kind on unmatched ns")
      (is (false?      (rns/with-min-level :kind1 "ns2" 100 (rns/*rt-call-filter* :kind1 "ns2" :id 50))))

      (is (nil? (enc/set-var-root! rns/*rt-call-filter* nil)))

      (testing "Call filtering"
        [(is (enc/submap? (rns/filter-call {:level :info, :elide? true}) {:elide? true}) "Can override `elide?`")
         (testing "Can override `allow?`"
           [(is (enc/submap? (rns/filter-call {:level :info, :allow?             true}) {:allow? true}))
            (is (enc/submap? (rns/filter-call {:level :info, :allow?            false}) {:allow? false}))
            (is (enc/submap? (rns/filter-call {:level :info, :allow? (enc/chance 0.5)}) {:allow? '(enc/chance 0.5)}) "Runtime forms allowed")])

         (is (enc/submap? (rns/filter-call {:level :info})
               {:callsite-id nat-int?
                :elide? :submap/nx
                :allow?
                '(taoensso.encore/and?
                   (let [sf taoensso.encore-tests.required-ns/*rt-call-filter*]
                     (if sf (sf nil "taoensso.encore-tests" nil :info) true)))})
           "With basic filtering")

         (is (enc/submap? (rns/filter-call {:level :info, :kind :my-sig-kind, :ns "my-ns", :id :my-sig-id, :callsite-id 1234
                                            :sample 0.5, :when (> 1 0), :limit [[1 1000]],
                                            :local-forms {:ns __ns}})
               {:callsite-id 1234
                :elide? :submap/nx
                :allow?
                '(taoensso.encore/and?
                   (< (Math/random) 0.5)
                   (let [sf taoensso.encore-tests.required-ns/*rt-call-filter*]
                     (if sf (sf :my-sig-kind __ns :my-sig-id :info) true))
                   (> 1 0)
                   (if (taoensso.encore.signals/call-limited!? 1234 [[1 1000]]) false true))})
           "With rich filtering")])])])

;;; Misc

(deftest _signal-misc
  [(testing "Dynamic context (`*ctx*`)"
     [(is (= (binding [rns/*ctx* "my-ctx"] rns/*ctx*) "my-ctx") "Supports manual `binding`")
      (is (= (rns/with-ctx       "my-ctx"  rns/*ctx*) "my-ctx") "Supports any data type")

      (is (= (rns/with-ctx "my-ctx1"       (rns/with-ctx+ nil                        rns/*ctx*)) "my-ctx1")              "nil update => keep old-ctx")
      (is (= (rns/with-ctx "my-ctx1"       (rns/with-ctx+ (fn [old] [old "my-ctx2"]) rns/*ctx*)) ["my-ctx1" "my-ctx2"])  "fn  update => apply")
      (is (= (rns/with-ctx {:a :A1 :b :B1} (rns/with-ctx+ {:a :A2 :c :C2}            rns/*ctx*)) {:a :A2 :b :B1 :c :C2}) "map update => merge")])

   (testing "Dynamic transform (`*xfn*`)"
     [(is (= (binding [rns/*xfn* identity] rns/*xfn*) identity)           "via `binding`")
      (is (= (rns/with-xfn       identity  rns/*xfn*) identity)           "via `with-xfn`")
      (is (= (rns/with-xfn inc (rns/with-xfn+ #(* 2 %) (rns/*xfn* 1))) 4) "via `with-xfn+`")])

   (testing "Utils"
     [(is (= (sigs/upper-qn :foo/bar) "FOO/BAR"))

      (is (= (sigs/format-level :info) "INFO"))
      (is (= (sigs/format-level     8) "LEVEL:8"))

      (is (= (sigs/format-id "foo.bar" :foo.bar/qux) "::qux"))
      (is (= (sigs/format-id "foo.baz" :foo.bar/qux) ":foo.bar/qux"))
      (is (= (sigs/format-id nil       :foo.bar/qux) ":foo.bar/qux"))

      (is (= nil       (sigs/format-callsite nil  nil)     (sigs/format-callsite nil)))
      (is (= "ns"      (sigs/format-callsite "ns" [nil 0]) (sigs/format-callsite {:ns "ns", :column 0})))
      (is (= "ns[0]"   (sigs/format-callsite "ns" [0])     (sigs/format-callsite {:ns "ns", :line 0})))
      (is (= "ns[0,1]" (sigs/format-callsite "ns" [0 1])   (sigs/format-callsite {:ns "ns", :line 0, :column 1})))

      (is (= (sigs/signal-with-combined-sample-rate nil {})            {}))
      (is (= (sigs/signal-with-combined-sample-rate 0.2 {})            {:sample 0.2}))
      (is (= (sigs/signal-with-combined-sample-rate nil {:sample 0.6}) {:sample 0.6}))
      (is (= (sigs/signal-with-combined-sample-rate 0.2 {:sample 0.6}) {:sample 0.12}))])])

;;; Handling

(def cnt (enc/counter 0))
(defn clear-handlers! [] (enc/update-var-root! rns/*sig-handlers* (fn [_] nil)))

(deftype MySignal [level cnt]
  sigs/ISignalHandling
  (allow-signal? [_ spec-filter] (spec-filter 'taoensso.encore-tests :my-id level))
  (signal-value  [_ _] cnt)
  (signal-debug  [_] {:level level}))

(deftest _signal-handling
  [(testing "Basics"
     [(is (nil? (clear-handlers!)))
      (is (nil? (cnt :set 0)))

      (is (=           (rns/get-handlers) nil))
      (is (enc/submap? (rns/add-handler! :hid1 (fn ([]) ([_] (cnt))) {:async nil, :sample 0.0}) {:hid1 {:dispatch-opts {:async nil, :sample 0.0}, :handler-fn fn?}}))
      (is (enc/submap? (rns/add-handler! :hid2 nil                   {:async nil, :sample 0.5}) {:hid1 {:dispatch-opts {:async nil, :sample 0.0}, :handler-fn fn?}}))
      (is (enc/submap? (rns/get-handlers)                                                       {:hid1 {:dispatch-opts {:async nil, :sample 0.0}, :handler-fn fn?}}))

      (is (nil? (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info "foo"))))
      (is (= @cnt 0))

      (is (enc/submap? (rns/add-handler! :hid1 (fn ([]) ([_] (cnt))) {:async nil, :sample 1.0}) {:hid1 {:dispatch-opts {:async nil, :sample 1.0}, :handler-fn fn?}}))
      (is (enc/submap? (rns/get-handlers)                                                       {:hid1 {:dispatch-opts {:async nil, :sample 1.0}, :handler-fn fn?}}))

      (is (nil? (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info  "foo"))))
      (is (nil? (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info  "foo"))))
      (is (= @cnt 2))

      (is (enc/submap? (rns/add-handler! :hid1 (fn ([]) ([_] (cnt))) {:async nil, :min-level :info}) {:hid1 {:dispatch-opts {:async nil, :min-level :info}, :handler-fn fn?}}))
      (is (nil? (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info  "foo"))) "Signal level >= handler's min level")
      (is (nil? (sigs/call-handlers! rns/*sig-handlers* (MySignal. :debug "foo"))) "Signal level <  handler's min level")
      (is (= @cnt 3))

      (is (nil? (rns/remove-handler! :hid1)))
      (is (nil? rns/*sig-handlers*) "Removal yields non-empty map")

      (let [a (atom nil)
            handlers
            [(sigs/wrap-handler :hid1 (fn [x] (reset! a *dynamic-var*))
               nil #?(:clj {:async {:mode :dropping}} :cljs nil))]]

        (binding [*dynamic-var* "bound"]
          (sigs/call-handlers! handlers (MySignal. :info "foo"))
          #?(:clj (Thread/sleep 500))
          (is (= @a "bound") "Handler binding conveyance")))

      (testing "with-handler/+"
        (let [sleep! (fn [] (enc/hot-sleep 1000))
              sig1_  (atom ::nx)
              sig2_  (atom ::nx)
              sig3_  (atom ::nx)
              st1?_  (atom false)
              st2?_  (atom false)
              st3?_  (atom false)]

          (rns/with-handler      :hid1 (fn ([x] (sleep!) (reset! sig1_ (str x ".1")))          ([] (sleep!) (reset! st1?_ true)))        {}
            (rns/with-handler+   :hid2 (fn ([x] (sleep!) (reset! sig2_ (str x ".2")))          ([] (sleep!) (reset! st2?_ true) (ex1!))) {}
              (rns/with-handler+ :hid3 (fn ([x] (sleep!) (reset! sig3_ (str x ".3"))) #?(:cljs ([])))                                    {}
                (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info "foo")))))

          [(is (= [@sig1_ @sig2_ @sig3_] ["foo.1" "foo.2" "foo.3"]) "Drained all")
           (is (= [@st1?_ @st2?_ @st3?_] [true true false])         "Stopped all")]))])

   (testing "Handler priorities"
     (let [handler-order
           (fn [p1 p2 p3 p4]
             (let [log_ (atom [])
                   log! (fn [hid] (swap! log_ conj hid))
                   handlers
                   (-> []
                     (sigs/add-handler :hid1 (fn [_] (log! 1)) nil {:async nil, :priority p1})
                     (sigs/add-handler :hid2 (fn [_] (log! 2)) nil {:async nil, :priority p2})
                     (sigs/add-handler :hid3 (fn [_] (log! 3)) nil {:async nil, :priority p3})
                     (sigs/add-handler :hid4 (fn [_] (log! 4)) nil {:async nil, :priority p4}))]

               (sigs/call-handlers! handlers (MySignal. :info "foo"))
               @log_))]

       [(is (= (handler-order 4 3 2 1) [1 2 3 4]))
        (is (= (handler-order 1 2 3 4) [4 3 2 1]))
        (is (= (handler-order 2 1 4 3) [3 4 1 2]))]))

   (testing "Handler sampling"
     (let [n-sampled
           (fn [sample]
             (let [c (enc/counter)
                   handlers
                   [(sigs/wrap-handler :hid1 (fn [x] (c) x) nil
                      {:sample sample, :async {:mode :sync}})]]

               (dotimes [_ 1000]
                 (sigs/call-handlers! handlers (MySignal. :info "foo")))

               @c))]

       [(is (=  1000 (n-sampled        nil))        "No sampling (const)")
        (is (=  1000 (n-sampled (fn [] nil)))       "No sampling (fn)")
        (is (=  1000 (n-sampled        1.0))      "100% sampling (const)")
        (is (=  1000 (n-sampled (fn [] 1.0)))     "100% sampling (fn)")
        (is (=  0    (n-sampled        0.0))        "0% sampling (const)")
        (is (=  0    (n-sampled (fn [] 0.0)))       "0% sampling (fn)")
        (is (<= 400  (n-sampled        0.5)  600)  "50% sampling (const)")
        (is (<= 400  (n-sampled (fn [] 0.5)) 600)  "50% sampling (fn)")]))

   (testing "Handler transforms"
     (let [v1 (atom ::nx)
           v2 (atom ::nx)
           v3 (atom ::nx)
           cx sigs/comp-xfn]

       [(is (nil? (clear-handlers!)))
        (is (nil? (cnt :set 0)))
        (is (enc/submap? (rns/add-handler! :hid1 (fn hf1 [x] (reset! v1 [(cnt) x])) {:async nil, :priority 3                                                  }) {:hid1 :submap/ex}))
        (is (enc/submap? (rns/add-handler! :hid2 (fn hf2 [x] (reset! v2 [(cnt) x])) {:async nil, :priority 2, :xfn (cx #(str % ".mw1") #(str % ".mw2"))}) {:hid2 :submap/ex}))
        (is (enc/submap? (rns/add-handler! :hid3 (fn hf3 [x] (reset! v3 [(cnt) x])) {:async nil, :priority 1, :xfn (fn [_] nil)})                         {:hid3 :submap/ex}))
        (is (nil? (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info "foo"))))

        #?(:clj (do (Thread/sleep 4000) :sleep))

        (is (= @v1 [0 "foo"]))
        (is (= @v2 [1 "foo.mw1.mw2"]))
        (is (= @v3 ::nx))
        (is (= @cnt 2) "hf3 never called")

        (is (map? (rns/remove-handler! :hid1)))
        (is (map? (rns/remove-handler! :hid2)))
        (is (nil? (rns/remove-handler! :hid3)))]))

   (testing "Handler error-fn (wrapped handlers trap exceptions, send to `error-fn`)"
     (let [fn-arg_ (atom nil)]
       (clear-handlers!)
       (rns/add-handler! :hid1 (fn [_] (ex1!)) {:error-fn (fn [x] (reset! fn-arg_ x)), :async nil})
       (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info "foo"))
       (is (enc/submap? @fn-arg_ {:handler-id :hid1, :error truss/error?}))))

   #?(:clj
      (testing "Handler backp-fn (handler dispatch detects back pressure, triggers `backp-fn`)"
        (let [fn-arg_ (atom nil)]
          (clear-handlers!)
          (rns/add-handler! :hid1 (fn [_] (Thread/sleep 2000)) {:backp-fn (fn [x] (reset! fn-arg_ x)), :async {:mode :blocking, :buffer-size 1}})
          (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info "1"))
          (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info "2")) ; Should trigger back pressure
          (Thread/sleep 4000) ; Wait for second signal to enqueue
          (is (enc/submap? @fn-arg_ {:handler-id :hid1})))))

   (testing "Handler stopping"
     [(is (= (sigs/stop-handlers! []) nil))
      (let [st?_ (atom false)]
        (clear-handlers!)
        (rns/add-handler!    :hid1 (fn ([_]) ([] (enc/hot-sleep 2000) (reset! st?_ true))))
        (rns/remove-handler! :hid1)
        (is (true? @st?_) "Removing handler stops it"))

      (let [st?_ (atom false)]
        (clear-handlers!)
        (rns/add-handler! :hid1 (fn ([_]) ([] (enc/hot-sleep 2000) (reset! st?_ true))))
        (rns/add-handler! :hid1 (fn ([_]) ([])))
        (is (true? @st?_) "Replacing handler stops old handler"))

      (is (nil? (clear-handlers!)))
      (is (= (let [sleep! (fn [] (enc/hot-sleep 500))
                   handlers
                   (-> []
                     (sigs/add-handler :hid1 (fn ([_] (sleep!)) #?(:cljs ([])))       {} {})
                     (sigs/add-handler :hid2 (fn ([_] (sleep!)) #?(:cljs ([])))       {} {:async {:drain-msecs nil}})
                     (sigs/add-handler :hid3 (fn ([_] (sleep!)) #?(:cljs ([])))       {} {:async {:drain-msecs 1000}})
                     (sigs/add-handler :hid4 (fn ([_] (sleep!)) #?(:cljs ([])))       {} {:async {:drain-msecs 100}})
                     (sigs/add-handler :hid5 (fn ([_] (sleep!))          ([]))        {} {})
                     (sigs/add-handler :hid6 (fn ([_] (sleep!))          ([] (ex1!))) {} {}))]

               (sigs/call-handlers!  handlers (MySignal. :info "foo"))
               [(sigs/stop-handlers! handlers)
                (sigs/stop-handlers! handlers)])

            [{:hid1 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid2 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid3 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid4 {:okay :stopped, #?@(:clj [:drained? false])},
              :hid5 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid6 {:error ex1,     #?@(:clj [:drained? true])}}

             {:hid1 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid2 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid3 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid4 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid5 {:okay :stopped, #?@(:clj [:drained? true])},
              :hid6 {:okay :stopped, #?@(:clj [:drained? true])}}]))])

   (testing "Handler stats"
     (clear-handlers!)
     (rns/add-handler! :hid1 (fn [_]) {:async nil, #?@(:cljs [:track-stats? true])})
     (rns/add-handler! :hid2 (fn [_]) {:async nil, #?@(:cljs [:track-stats? true])})
     (rns/add-handler! :hid3 (fn [_]) {:async nil             :track-stats? false})
     (dotimes [_ 1e3] (sigs/call-handlers! rns/*sig-handlers* (MySignal. :info "foo")))

     [(is (enc/submap? (rns/get-handlers-stats)
            {:hid1 {:handling-nsecs {:n 1000}, :counts {:handled 1000, :allowed 1000, :errors 0}}
             :hid2 {:handling-nsecs {:n 1000}, :counts {:handled 1000, :allowed 1000, :errors 0}}
             :hid3 :submap/nx}))

      (is (enc/submap? @(get-in (rns/get-handlers) [:hid1 :handler-stats_]) {:handling-nsecs {:n 1000}, :counts {:handled 1000, :allowed 1000, :errors 0}}))
      (is (enc/submap? @(get-in (rns/get-handlers) [:hid2 :handler-stats_]) {:handling-nsecs {:n 1000}, :counts {:handled 1000, :allowed 1000, :errors 0}}))
      (is (not (contains? (get  (rns/get-handlers) :hid3) :handler-stats_)))])])

;;;;

#?(:cljs
   (defmethod test/report [:cljs.test/default :end-run-tests] [m]
     (when-not (test/successful? m)
       ;; Trigger non-zero `lein test-cljs` exit code for CI
       (throw (ex-info "ClojureScript tests failed" {})))))

#?(:cljs (test/run-tests))
