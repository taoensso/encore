(ns taoensso.encore-tests
  (:require
   [clojure.test                     :as test :refer [deftest testing is]]
   ;; [clojure.test.check            :as tc]
   ;; [clojure.test.check.generators :as tc-gens]
   ;; [clojure.test.check.properties :as tc-props]
   [clojure.string  :as str]
   [taoensso.encore :as enc]))

(comment
  (remove-ns      'taoensso.encore-tests)
  (test/run-tests 'taoensso.encore-tests))

;;;;

;; (deftest pass (is (= 1 1)))
;; (deftest fail (is (= 1 0)))

(defn- throw! [x] (throw (ex-info "Error" {:arg {:value x :type (type x)}})))

;;;;

(deftest _submap?
  [(is (enc/submap? {:a {:b :B1 :c :C1}} {:a {:b :B1}}))
   (is (enc/submap? {:a {:b :B1       }} {:a {:c :submap/nx}}))])

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

(deftest _reduce-zip
  [(is (= (enc/reduce-zip assoc {}  [:a :b :c]     [1 2 3])   {:a 1, :b 2, :c 3}) "Vec,  normal")
   (is (= (enc/reduce-zip assoc {} '(:a :b :c)    '(1 2 3))   {:a 1, :b 2, :c 3}) "List, normal")
   (is (= (enc/reduce-zip assoc {}  [:a :b :c :a]  [1 2 3 4]) {:a 4, :b 2, :c 3}) "Vec,  replacing")
   (is (= (enc/reduce-zip assoc {} '(:a :b :c :a) '(1 2 3 4)) {:a 4, :b 2, :c 3}) "List, replacing")
   (is (= (enc/reduce-zip assoc {}  [:a :b :c]     [1 2])     {:a 1, :b 2})       "Vec,  uneven")
   (is (= (enc/reduce-zip assoc {} '(:a :b :c)    '(1 2))     {:a 1, :b 2})       "List, uneven")
   (is (= (enc/reduce-zip assoc {}  []             [1])       {})                 "Vec,  empty")
   (is (= (enc/reduce-zip assoc {} '()            '(1))       {})                 "List, empty")])

;;;;

#?(:cljs (test/run-tests))
