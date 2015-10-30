(ns taoensso.encore.tests
  #+clj
  (:require
   [clojure.test :as test :refer (is are deftest run-tests)]
   [clojure.test.check            :as tc]
   [clojure.test.check.generators :as tc-gen]
   [clojure.test.check.properties :as tc-prop]
   [taoensso.encore :as enc :refer ()])

  #+cljs
  (:require
   [cljs.test :as test :refer-macros (is are deftest run-tests)]
   [clojure.test.check            :as tc]
   [clojure.test.check.generators :as tc-gen]
   [clojure.test.check.properties :as tc-prop :include-macros true]
   [taoensso.encore :as enc :refer ()]))

(comment (run-tests))

#+cljs
(do
  (enable-console-print!)
  ;; (set! *main-cli-fn* (fn -main [] (test/run-tests)))
  )

(deftest foo
  (is (= 1 1)))
