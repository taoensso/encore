(ns taoensso.encore-tests
  (:require
   [clojure.test                     :as test :refer [deftest testing is]]
   ;; [clojure.test.check            :as tc]
   ;; [clojure.test.check.generators :as tc-gens]
   ;; [clojure.test.check.properties :as tc-props]
   [clojure.string  :as str]
   [taoensso.encore :as enc]
   [taoensso.encore.runner :as runner]
   [taoensso.encore.ctx-filter :as cf])

  #?(:cljs
     (:require-macros
      [taoensso.encore-tests
       :refer
       [test-macro-alias test-if-cljs test-get-source resolve-sym
        callsite-inner callsite-outer1 callsite-outer2]])))

(comment
  (remove-ns      'taoensso.encore-tests)
  (test/run-tests 'taoensso.encore-tests))

;;;;

;; (deftest pass (is (= 1 1)))
;; (deftest fail (is (= 1 0)))

(defn- throw! [x] (throw (ex-info "Error" {:arg {:value x :type (type x)}})))

;;;; Core

(do
  (defn- test-fn "doc a" [x] x)
  (enc/defalias                 test-fn-alias-1 test-fn)
  (enc/defalias ^{:doc "doc b"} test-fn-alias-2 test-fn)
  (enc/defalias ^{:doc "doc b"} test-fn-alias-3 test-fn {:doc "doc c"})
  (enc/defaliases {:src test-fn :alias test-fn-alias-4 :attrs {:doc "doc d"}})
  (enc/defaliases {:src test-fn :alias test-fn-alias-5         :doc "doc e"})

  #?(:clj (defmacro ^:private test-macro [x] `~x))
  #?(:clj (enc/defalias test-macro-alias test-macro))

  #?(:cljs (def ^:private cljs-var "cljs-var-doc" "cljs-var-val"))
  #?(:cljs (enc/defalias  cljs-var-alias           cljs-var)))

(deftest _defalias
  ;; [1] v3.47.0+: Cljs aliases no longer copy metadata
  [(do     (is (=    (test-fn-alias-1 :x) :x)))
   #?(:clj (is (= (-> test-fn-alias-1 var meta :doc) "doc a"))) ; [1]
   (do     (is (= (-> test-fn-alias-2 var meta :doc) "doc b")))
   (do     (is (= (-> test-fn-alias-3 var meta :doc) "doc c")))
   (do     (is (= (-> test-fn-alias-4 var meta :doc) "doc d")))
   (do     (is (= (-> test-fn-alias-5 var meta :doc) "doc e")))

   (is (= (test-macro-alias :x) :x))

      #?(:cljs (is (=     cljs-var-alias                "cljs-var-val")))
      #?(:cljs (is (= (-> cljs-var       var meta :doc) "cljs-var-doc")))
   ;; #?(:cljs (is (= (-> cljs-var-alias var meta :doc) "cljs-var-doc"))) ; [1]
   ])

(deftest _truss-invariants
  ;; Tested properly in Truss, just confirm successful imports here
  [(is (= (enc/have  string? "foo") "foo"))
   (is (= (enc/have! string? "foo") "foo"))
   (is (= (enc/have? string? "foo")  true))
   (is (= (enc/have?         "foo")  true))
   (is (enc/throws? (enc/have string? 5)))
   (is (enc/throws? :any
         {:data {:dynamic :dynamic-data
                 :arg     :arg-data}}
         (enc/with-truss-data :dynamic-data
           (enc/have? string? 5 :data :arg-data))))])

;;;; Errors

(deftest _throws?
  (let [throw-common   (fn [] (throw (ex-info "Shenanigans" {:a :a1 :b :b1})))
        throw-uncommon (fn [] (throw #?(:clj (Error.) :cljs "Error")))]

    [(is      (enc/throws?                            (throw-common)))
     (is      (enc/throws? :common                    (throw-common)))
     (is      (enc/throws? :any                       (throw-common)))
     (is (not (enc/throws? :common                    (throw-uncommon))))
     (is      (enc/throws? :any                       (throw-uncommon)))

     (is      (enc/throws? :default #"Shenanigans"    (throw-common)))
     (is (not (enc/throws? :default #"Brouhaha"       (throw-common))))

     (is      (enc/throws? :default {:a :a1}          (throw-common)))
     (is (not (enc/throws? :default {:a :a1 :b :b2}   (throw-common))))

     (is      (enc/throws? :default {:a :a1} (throw (ex-info "Test" {:a :a1 :b :b1}))))
     (is (not (enc/throws? :default {:a :a1} (throw (ex-info "Test" {:a :a2 :b :b1})))))

     (is (= (ex-data (enc/throws :ex-info     {:a :a3}
                       (throw
                         (ex-info       "ex1" {:a :a1}
                           (ex-info     "ex2" {:a :a2}
                             (ex-info   "ex3" {:a :a3} ; <- Match this
                               (ex-info "ex4" {:a :a4})))))))
           {:a :a3})
       "Check nested causes for match")

     ;; Form must throw error, not return it
    #?(:clj
       [(is      (enc/throws? Exception (throw (Exception.))))
        (is (not (enc/throws? Exception        (Exception.))))]

       :cljs
       [(is      (enc/throws? js/Error (throw (js/Error.))))
        (is (not (enc/throws? js/Error        (js/Error.))))])]))

(deftest _catching-rf
  [(is (=   (reduce (enc/catching-rf            (fn [acc in] (conj acc         in)))  [] [:a :b]) [:a :b]))
   (is (=   (reduce (enc/catching-rf {:id :foo} (fn [acc in] (conj acc         in)))  [] [:a :b]) [:a :b]))
   (is (->> (reduce (enc/catching-rf {:id :foo} (fn [acc in] (conj acc (throw! in)))) [] [:a :b])
         (enc/throws? :common {:id :foo :call '(rf acc in) :args {:in {:value :a}}})))

   (is (=   (reduce-kv (enc/catching-rf (fn [acc k v] (assoc acc k         v)))  {} {:a :A}) {:a :A}))
   (is (->> (reduce-kv (enc/catching-rf (fn [acc k v] (assoc acc k (throw! v)))) {} {:a :A})
         (enc/throws? :common {:call '(rf acc k v) :args {:k {:value :a} :v {:value :A}}})))])

(deftest _catching-xform
  [(is (=   (transduce (enc/catching-xform (map identity)) (completing (fn [acc in] (conj acc in))) [] [:a :b]) [:a :b]))
   (is (->> (transduce (enc/catching-xform (map throw!))   (completing (fn [acc in] (conj acc in))) [] [:a :b])
         (enc/throws? :common {:call '(rf acc in) :args {:in {:value :a}}}))
     "Error in xform")

   (is (=   (transduce (enc/catching-xform (map identity)) (completing (fn [acc in] (conj acc         in)))  [] [:a :b]) [:a :b]))
   (is (->> (transduce (enc/catching-xform (map identity)) (completing (fn [acc in] (conj acc (throw! in)))) [] [:a :b])
         (enc/throws? :common {:call '(rf acc in) :args {:in {:value :a}}}))
     "Error in rf")])

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
   (is (= (enc/reduce-zip (fn [acc k v] (reduced ::reduced!)) {} [:a :b :c] '(1 2 3)) ::reduced!))])

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

;;;; Collections

(deftest _get1
  [(is (= (enc/get1 {:a :A}  :b       ::nx) ::nx))
   (is (= (enc/get1 {:a nil} :a       ::nx) nil))
   (is (= (enc/get1 {:a nil} :b :a    ::nx) nil))
   (is (= (enc/get1 {:a nil} :c :b :a ::nx) nil))])

(deftest _submap?
  [(is      (enc/submap? {:a {:b :B1 :c :C1}} {:a {:b :B1}}))
   (is      (enc/submap? {:a {:b :B1       }} {:a {:c :submap/nx}}))
   (is (not (enc/submap? {:a {:b :B1 :c nil}} {:a {:c :submap/nx}})))
   (is      (enc/submap? {:a {:b :B1}}        {:a {:b :submap/ex}}))
   (is (not (enc/submap? {:a {:b :B1}}        {:a {:c :submap/ex}})))
   (is      (enc/submap? {:a {:b :B1}}        {:a {:b :submap/some}}))
   (is (not (enc/submap? {:a {:b nil}}        {:a {:b :submap/some}})))])

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

   (is (enc/throws? #?(:clj ClassCastException) (enc/update-in {:a :A} [:a :b] (fn [_] :x)))
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
   (is (enc/throws? #?(:clj ClassCastException) (enc/dissoc-in {:a :A} [:a :b])))])

(deftest _merge-with
  [(is (= (enc/merge {:a :A1} {:b :B1})          {:a :A1, :b :B1}))
   (is (= (enc/merge {:a :A1} {:b :B1} {:a :A2}) {:a :A2, :b :B1}))

   (is (= (enc/merge {:a :A1 :b :B2} {:a :A1 :b :merge/dissoc})      {:a :A1}))
   (is (= (enc/merge {:a :A1}        {:b :B1 :merge/replace? true})  {:b :B1}))
   (is (= (enc/merge {:a :A1}        {:b :B1 :merge/replace? false}) {:a :A1, :b :B1}))

   (is (= (enc/nested-merge
            {:a1 :A1 :b1 :B1  :c1 {:a2 :A2 :b2 {:a3 :A3 :b3 :B3  :d1 :D1 :e1 :E1}}}
            {        :b1 :B1* :c1 {        :b2 {        :b3 :B3* :d1 nil :e1 :swap/dissoc}}}
            nil)
         {:a1 :A1, :b1 :B1*, :c1 {:a2 :A2, :b2 {:a3 :A3, :b3 :B3*, :d1 nil}}}))])

;;;; Strings

#?(:clj
   (deftest _utf8-byte-strings
     (let [s "hello ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸ"]
       (is (= (-> s enc/str->utf8-ba enc/utf8-ba->str) s)))))

(deftest  _get-substr-by-idx
  [(is (= (enc/get-substr-by-idx nil            nil)         nil))
   (is (= (enc/get-substr-by-idx "123456789"    nil) "123456789"))
   (is (= (enc/get-substr-by-idx "123456789"      1)  "23456789"))
   (is (= (enc/get-substr-by-idx "123456789"     -3)       "789"))
   (is (= (enc/get-substr-by-idx "123456789"   -100) "123456789"))
   (is (= (enc/get-substr-by-idx "123456789"  0 100) "123456789"))
   (is (= (enc/get-substr-by-idx "123456789"  0   0)         nil))
   (is (= (enc/get-substr-by-idx "123456789"  0   1) "1"        ))
   (is (= (enc/get-substr-by-idx "123456789"  0  -1) "12345678" ))
   (is (= (enc/get-substr-by-idx "123456789"  0  -5) "1234"     ))
   (is (= (enc/get-substr-by-idx "123456789" -5  -3)      "56"  ))
   (is (= (enc/get-substr-by-idx "123456789"  4   3)         nil))])

(deftest  _get-substr-by-len
  [(is (= (enc/get-substr-by-len nil            nil)         nil))
   (is (= (enc/get-substr-by-len "123456789"    nil) "123456789"))
   (is (= (enc/get-substr-by-len "123456789"      1)  "23456789"))
   (is (= (enc/get-substr-by-len "123456789"     -3)       "789"))
   (is (= (enc/get-substr-by-len "123456789"   -100) "123456789"))
   (is (= (enc/get-substr-by-len "123456789"  0 100) "123456789"))
   (is (= (enc/get-substr-by-len "123456789"  0   0)         nil))
   (is (= (enc/get-substr-by-len "123456789"  0   1) "1"        ))
   (is (= (enc/get-substr-by-len "123456789"  0  -5)         nil))
   (is (= (enc/get-substr-by-len "123456789" -5   2)      "56"  ))])

#?(:clj
   (deftest _hex-strings
     [(is (= (enc/ba->hex-str (byte-array  0))    ""))
      (is (= (enc/ba->hex-str (byte-array [0]))   "00"))
      (is (= (enc/ba->hex-str (byte-array [0 1])) "0001"))
      (let [v (vec (range -128 128))]
        (is (= (-> v byte-array enc/ba->hex-str enc/hex-str->ba vec) v)))]))

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

         (is (enc/throws? #?(:clj ClassCastException) (enc/swap-in! a [:b :c] (fn [_] :x)))
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

         (is (enc/throws? #?(:clj ClassCastException) (enc/swap-in! a [:a :b2 :c] (fn [_] :x)))
           "Non-associative val")

         (is (= (enc/swap-in! a [:a :b2] (fn [v] (enc/swapped :swap/dissoc v))) :x2))
         (is (= @a {:a {}}))])])])

;;;; Callsites, etc.

(defmacro ^:private callsite-inner  [] (meta &form))
(defmacro ^:private callsite-outer1 []                    `(callsite-inner))
(defmacro ^:private callsite-outer2 [] (enc/keep-callsite `(callsite-inner)))

(deftest _keep-callsite
  [(is (map? (callsite-inner)))
   (is (nil? (callsite-outer1)))
   (is (map? (callsite-outer2)))])

(defmacro test-if-cljs [caller]
  `(enc/if-cljs
     {:target :cljs, :caller ~caller}
     {:target :clj,  :caller ~caller}))

(deftest _if-cljs
  [#?(:clj  (is (= (test-if-cljs :clj)  {:target :clj,  :caller :clj})))
   #?(:cljs (is (= (test-if-cljs :cljs) {:target :cljs, :caller :cljs})))])

#?(:clj
   (defmacro test-get-source [caller]
     #_(spit "debug.txt" (str [(boolean (:ns &env)) (:file (meta &form))] "\n") :append true)
     `{:caller    ~caller
       :target    ~(if (:ns &env) :cljs :clj)
       :form      '~&form
       :*file*    ~(str *file*)
       :env       ~(-> &env       (select-keys [:line :column]))
       :form-meta ~(-> &form meta (select-keys [:line :column :file]))
       :source    ~(enc/get-source &form &env)}))

(comment (test-get-source :repl))

(deftest _get-source
  [#?(:clj
      (let [m (test-get-source :clj)]
        [(is (enc/submap? m {:target :clj, :caller :clj, :source {:file :submap/some}}))
         (is (= (get-in   m [:source :file]) (get-in m [:*file*])))]))

   #?(:cljs
      (let [m (test-get-source :cljs)]
        [(is (enc/submap?  m {:target :cljs, :caller :cljs, :source {:file :submap/some}}))
         (is (not= (get-in m [:source :file]) (get-in m [:*file*])))]))])

;;;; Resolving

(def myvar #?(:clj "local clj var" :cljs "local cljs var"))

#?(:clj (defmacro resolve-sym [sym] (keyword (enc/resolve-sym &env sym true))))

(deftest _resolve
  [(is (= (resolve-sym __nx)     nil))
   (is (= (resolve-sym __nx/foo) nil))

   (is (= (resolve-sym                                     myvar) :taoensso.encore-tests/myvar))
   (is (= (resolve-sym taoensso.encore-tests.unrequired-ns/myvar) :taoensso.encore-tests.unrequired-ns/myvar))])

;;;; Config API

(deftest config-api
  [(is (enc/submap? (enc/get-config {:debug? true}) {:config :submap/nx, :search []}))
   (is (enc/submap? (enc/get-config {:debug? true :prop [:taoensso.prop-a1 :taoensso.prop-a2 :taoensso.prop-a3]})
         {:config :submap/nx
          :search
          [[:prop "taoensso.prop-a1"]
           [:env  "TAOENSSO_PROP_A1"]
           [:res  "taoensso.prop-a1"]
           [:prop "taoensso.prop-a2"]
           [:env  "TAOENSSO_PROP_A2"]
           [:res  "taoensso.prop-a2"]
           [:prop "taoensso.prop-a3"]
           [:env  "TAOENSSO_PROP_A3"]
           [:res  "taoensso.prop-a3"]]}))

   (is (enc/submap? (enc/get-config {:debug? true :prop :taoensso.encore-tests.config.str})
         {:config "foo",
          :search [[:prop "taoensso.encore-tests.config.str"]
                   [:env  "TAOENSSO_ENCORE_TESTS_CONFIG_STR"]
                   [:res  "taoensso.encore-tests.config.str"]]}))

   (is (enc/submap? (enc/get-config {:debug? true :prop #?(:clj  [:taoensso.encore-tests.config.clj.str  :taoensso.encore-tests.config.str]
                                                           :cljs [:taoensso.encore-tests.config.cljs.str :taoensso.encore-tests.config.str])})
         {:config #?(:clj "foo/clj" :cljs "foo/cljs")}))

   (is (enc/submap? (enc/get-config {:debug? true :debug/match ["taoensso.encore-tests.unrequired-ns/myvar-embeddable" [:debug]] :as :edn})
         {:config {:embeddable? true, :foo :bar}}))

   #?(:clj
      (is (enc/submap? (enc/get-config {:debug? true :debug/match ["taoensso.encore-tests.unrequired-ns/myvar-unembeddable" [:debug]] :as :edn})
            {:config {:embeddable? false, :fn :submap/ex}})))

   (is (= (enc/get-sys-val*        [::nx :taoensso.encore-tests.config.str]) "foo"))
   (is (= (enc/get-sys-bool* false [::nx :taoensso.encore-tests.config.bool]) true))
   (is (= (enc/read-sys-val*       [::nx :taoensso.encore-tests.config.edn]) {:kw :my-kw :str "foo" :int 5 :vec [:x]}))
   (is (= (enc/read-sys-val*       [::nx :taoensso.encore-tests.config])     {:kw :my-kw :str "foo" :int 5 :vec [:x]}) "Auto .edn extension")])

(comment
  (get-config {})
  (def foo {:fn (fn [x] (* x x))})
  (get-config {:debug/match ["taoensso.encore/foo" [:debug]] :as :edn})
  (get-config {:debug? true :prop [:p1] :as :edn}))

;;;; Misc

#?(:clj
   (deftest _secure-rng-mock
     [(is (=
            (let [msrng (enc/secure-rng-mock!!! 5)] [(.nextLong msrng) (.nextDouble msrng)])
            (let [msrng (enc/secure-rng-mock!!! 5)] [(.nextLong msrng) (.nextDouble msrng)])))

      (is (not=
            (let [msrng (enc/secure-rng-mock!!! 5)] [(.nextLong msrng) (.nextDouble msrng)])
            (let [msrng (enc/secure-rng-mock!!! 2)] [(.nextLong msrng) (.nextDouble msrng)])))]))

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

;;;; Name filter

(deftest _name-filter
  [(is (enc/throws? (enc/name-filter  nil)))

   (is (true?  ((enc/name-filter :any)      "foo")))

   (is (true?  ((enc/name-filter #{:foo*})  "foo")))
   (is (true?  ((enc/name-filter #{"foo*"}) 'foo)))
   (is (false? ((enc/name-filter #{"foo*"}) 'bar)))

   (is (true?  ((enc/name-filter ["foo" "bar"]) "bar")))
   (is (true?  ((enc/name-filter ["foo" "bar"]) :bar)))
   (is (false? ((enc/name-filter ["foo" "bar"]) :baz)))

   (is (true?  ((enc/name-filter ["foo" "b*"])  "bar")))
   (is (true?  ((enc/name-filter ["foo" "b*"])  :bar)))
   (is (false? ((enc/name-filter ["foo" "b*"])  :qux)))

   (is (false? ((enc/name-filter {:allow :any :deny :any}) "foo")))
   (is (true?  ((enc/name-filter {:allow :any :deny  #{}}) "foo")))

   (is (true? ((enc/name-filter {:allow "foo*"}) "foo")))
   (is (true? ((enc/name-filter {:allow "foo*"}) :foobar)))

   (is (false? ((enc/name-filter {:allow "foo*" :deny "foobar"}) :foobar)))
   (is (true?  ((enc/name-filter {:allow "foo*" :deny "foobar"}) :foobaz)))])

;;;; Runner

#?(:clj
   (deftest _runner
     [(is (= (let [a (atom nil)
                   r (runner/runner {:mode :sync})]
               [(r (fn [] (Thread/sleep 1000) (reset! a :done))) @a])
            [true :done]))

      (is (= (let [a (atom [])
                   r (runner/runner {:mode :dropping, :buffer-size 3, :debug/init-after 100})]

               [(vec (for [n (range 6)] (r (fn [] (Thread/sleep 20) (swap! a conj n)))))
                (do (Thread/sleep 500) @a)])

            [[true true true false false false] [0 1 2]]))

      (is (= (let [a (atom [])
                   r (runner/runner {:mode :sliding, :buffer-size 3, :debug/init-after 100})]

               [(vec (for [n (range 6)] (r (fn [] (Thread/sleep 20) (swap! a conj n)))))
                (do (Thread/sleep 500) @a)])

            [[true true true false false false] [3 4 5]]))

      (is (= (let [a (atom [])
                   r (runner/runner {:mode :blocking, :buffer-size 3, :debug/init-after 100})]

               [(vec (for [n (range 6)] (r (fn [] (Thread/sleep 20) (swap! a conj n)))))
                (do (Thread/sleep 500) @a)])

            [[true true true false false false] [0 1 2 3 4 5]]))]))

;;;; Context filter

(deftest _context-filter
  [(testing "Levels"
     [(is (=   (cf/get-level-int   -10) -10))
      (is (=   (cf/get-level-int :info)  50))
      (is (=   (cf/get-level-int   nil)  nil))
      (is (=   (cf/get-level-int :__nx)  nil))

      (is (=   (cf/valid-level-int   -10) -10))
      (is (=   (cf/valid-level-int :info)  50))
      ;; (is (->>          (cf/valid-level-int     nil) (enc/throws? :common "Invalid level"))) ; Macro-time error
      (is    (->> ((fn [x] (cf/valid-level-int x)) nil) (enc/throws? :common "Invalid level")))

      (is      (cf/level>= :error :info))
      (is      (cf/level>= :error :error))
      (is (not (cf/level>= :info  :error)))
      (is      (cf/level>= :min   -10))])

   (testing "Utils"
     [(testing "update-level-spec"
        [(is (=   (cf/update-level-spec nil      nil) nil))
         (is (=   (cf/update-level-spec nil    :info) :info))
         (is (=   (cf/update-level-spec :info :error) :error))
         (is (->> (cf/update-level-spec nil    :__nx) (enc/throws? :common "Invalid level")))

         (is (=   (cf/update-level-spec nil  nil  [["ns1" :info]]) [["ns1" :info]]))
         (is (=   (cf/update-level-spec nil "ns1" :info)           [["ns1" :info]]))
         (is (->> (cf/update-level-spec nil "ns1" [["ns1" :info]]) (enc/throws? :common "Invalid level")))
         (is (->> (cf/update-level-spec nil  nil  [["ns1" :__nx]]) (enc/throws? :common "Invalid level")))
         (is (->> (cf/update-level-spec nil  -1   :info)           (enc/throws? :common "Invalid name filter spec")))

         (is (=   (cf/update-level-spec :debug "ns1" :info) [["ns1" :info] ["*" :debug]]))
         (is (=   (->
                    :debug
                    (cf/update-level-spec  "ns1" :info)
                    (cf/update-level-spec  "ns2" :trace)
                    (cf/update-level-spec  "ns3" -20)
                    (cf/update-level-spec  "ns2" :warn))
               [["ns2" :warn] ["ns3" -20] ["ns1" :info] ["*" :debug]]))])])

   (testing "Filtering"
     [(testing "basics"
        [(is (false? (cf/filter? nil nil   nil    nil)))
         (is (false? (cf/filter? nil nil   "ns1" :info)))
         (is (false? (cf/filter? nil :info "ns1" :info)))
         (is (true?  (cf/filter? nil :warn "ns1" :info)))

         ;; (is (false? (cf/filter? (fn [_] true)  nil "ns1" :info)) "Arb ns-spec fn")
         ;; (is (true?  (cf/filter? (fn [_] false) nil "ns1" :info)) "Arb ns-spec fn")

         (is (false? (cf/filter? "ns1"          nil "ns1" :info)))
         (is (true?  (cf/filter? "ns2"          nil "ns1" :info)))
         (is (false? (cf/filter? "ns*"          nil "ns1" :info)))
         (is (false? (cf/filter? #{"ns1" "ns2"} nil "ns1" :info)))
         (is (true?  (cf/filter? #{"ns2" "ns3"} nil "ns1" :info)))
         (is (false? (cf/filter? #"ns(\d*)?"    nil "ns5" :info)))
         (is (false? (cf/filter? #"ns(\d*)?"    nil "ns"  :info)))

         (is (false? (cf/filter? {:allow #{"ns1"}}  nil "ns1" :info)))
         (is (false? (cf/filter? {:allow #{"ns*"}}  nil "ns1" :info)))
         (is (false? (cf/filter? {:allow #{"*"}}    nil "ns1" :info)))
         (is (false? (cf/filter? {:allow #{:any}}   nil "ns1" :info)))
         (is (false? (cf/filter? {:allow "*"}       nil "ns1" :info)))
         (is (false? (cf/filter? {:allow :any}      nil "ns1" :info)))
         (is (true?  (cf/filter? {:allow #{}}       nil "ns1" :info)))

         (is (false? (cf/filter? {:allow #{"a.*.c"}} nil "a.b3.c"    :info)))
         (is (false? (cf/filter? {:allow #{"a.*.c"}} nil "a.b1.b2.c" :info)))

         (is (false? (cf/filter? {:deny #{}}     nil "ns1" :info)))
         (is (true? (cf/filter?  {:deny #{"*"}}  nil "ns1" :info)))
         (is (true? (cf/filter?  {:deny #{:any}} nil "ns1" :info)))
         (is (true? (cf/filter?  {:deny "*"}     nil "ns1" :info)))
         (is (true? (cf/filter?  {:deny :any}    nil "ns1" :info)))

         (is (true? (cf/filter? {:allow :any :deny :any} nil "ns1" :info)) "Deny > allow")

         (is (true? (cf/filter? "ns1" nil   nil :info))                                     "ns-spec without ns")
         (is (->>   (cf/filter? nil   :info nil nil) (enc/throws? :common "Invalid level")) "level-spec without level")])

      (testing "ns-specific levels"
        [(is (false? (cf/filter? nil [["*" :info]] "ns1" :info)))
         (is (true?  (cf/filter? nil [["*" :info]] "ns1" :debug)))
         (is (false? (cf/filter? nil [["ns1" :info] ["ns1" :warn] ["*" :warn]] "ns1" :info)) "Match sequentially")
         (is (true?  (cf/filter? nil [["ns1" :warn] ["ns1" :info] ["*" :warn]] "ns1" :info)) "Match sequentially")

         (is (false? (cf/filter? nil [["ns.internal.*" :warn] ["ns.public.*" :info] ["*" :warn]] "ns.public.foo"   :info)))
         (is (true?  (cf/filter? nil [["ns.internal.*" :warn] ["ns.public.*" :info] ["*" :warn]] "ns.internal.foo" :info)))

         (is (->> (cf/filter? nil [["*" :info]] "ns1" nil) (enc/throws? :common "Invalid level")))
         (is (->> (cf/filter? nil [["*" :info]] nil   nil) (enc/throws? :common "Invalid level")))])])])

;;;;

#?(:cljs (test/run-tests))
