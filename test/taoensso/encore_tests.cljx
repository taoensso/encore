(ns taoensso.encore-tests
  #+clj
  (:require
    [expectations :refer [expect]]
    ;; [clojure.test.check            :as dc]
    ;; [clojure.test.check.generators :as dc-gen]
    ;; [clojure.test.check.properties :as dc-prop]
    [taoensso.encore :as encore])

  #+cljs
  (:require
    [expectations :refer-macros [expect]]
    ;; [clojure.test.check            :as dc]
    ;; [clojure.test.check.generators :as dc-gen]
    ;; [clojure.test.check.properties :as dc-prop :include-macros true]
    [taoensso.encore :as encore]))


(defn before-run {:expectations-options :before-run} [])
(defn after-run  {:expectations-options :after-run}  [])

(expect true)
