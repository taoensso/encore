(ns taoensso.encore.tests.main
  (:require [expectations    :as test   :refer :all]
            [taoensso.encore :as encore :refer ()]))

(comment (test/run-tests '[taoensso.encore.tests.main]))

(defn- before-run {:expectations-options :before-run} [])
(defn- after-run  {:expectations-options :after-run}  [])
