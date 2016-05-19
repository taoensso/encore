(ns taoensso.encore.tests
  #?(:clj
     (:require
       [clojure.test :as test :refer (is are deftest run-tests)]
       [clojure.test.check :as tc]
       [clojure.test.check.generators :as tc-gens]
       [clojure.test.check.properties :as tc-props]
       [taoensso.encore :as enc :refer ()]))

  #?(:cljs
     (:require
       [cljs.test :as test :refer-macros (is are deftest run-tests)]
       [clojure.test.check :as tc]
       [clojure.test.check.generators :as tc-gens]
       [clojure.test.check.properties :as tc-props :include-macros true]
       [taoensso.encore :as enc :refer ()])))

(comment (run-tests))

#?(:cljs
   (do
     (enable-console-print!)
     ;; (set! *main-cli-fn* (fn -main [] (test/run-tests)))
     ))

(deftest foo
  (is (= 1 1)))
