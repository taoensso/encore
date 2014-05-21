(ns taoensso.encore.tests.main-common
  #+clj
  (:require [clojure.test :refer :all]
            [taoensso.encore :as encore :refer :all])

  #+cljs
  (:require [cemerick.cljs.test :as t]
            [taoensso.encore :as encore :refer []])
  #+cljs
  (:require-macros [cemerick.cljs.test :refer [deftest is testing]]))

