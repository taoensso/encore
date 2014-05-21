(ns taoensso.encore.tests.main-common
  #+clj
  (:require [clojure.test :refer :all]
            [taoensso.encore :as encore :refer :all])

  #+cljs
  (:require [cemerick.cljs.test :as t]
            [taoensso.encore :as encore :refer [format]])
  #+cljs
  (:require-macros [cemerick.cljs.test :refer [deftest is testing]]))

(deftest test-format
  (is (= "Answer is 42" (format "%s is %d" "Answer" 42)))
  #+cljs
  (is (= "Nils become: null, NaN" (format "Nils become: %s, %d" nil nil))
      "goog.format prints null based on the format character: %s => null, %d => NaN"))
