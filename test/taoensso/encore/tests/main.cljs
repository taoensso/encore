(ns taoensso.encore.tests.main "Tests specific to ClojureScript"
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)])
  (:require [cemerick.cljs.test :as t]
            [taoensso.encore :as encore :refer []]))
