(ns taoensso.encore-tests
  (:require
   [clojure.test                     :as test :refer [is]]
   ;; [clojure.test.check            :as tc]
   ;; [clojure.test.check.generators :as tc-gens]
   ;; [clojure.test.check.properties :as tc-props]
   [clojure.string  :as str]
   [taoensso.encore :as enc]))

(comment
  (remove-ns      'taoensso.encore-tests)
  (test/run-tests 'taoensso.encore-tests)
  (test/run-all-encore-tests))

(defn run-all-encore-tests []
  (test/run-tests
    'taoensso.encore
    'taoensso.encore-tests))

#?(:cljs (do (enable-console-print!) (run-all-encore-tests)))

(test/deftest foo
  (is (= 1 1)))
