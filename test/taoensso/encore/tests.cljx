(ns taoensso.encore.tests
  #+clj
  (:require
   [expectations :as expect :refer (expect)]
   ;; [clojure.test.check            :as dc]
   ;; [clojure.test.check.generators :as dc-gen]
   ;; [clojure.test.check.properties :as dc-prop]
   [taoensso.encore :as encore :refer ()])

  #+cljs
  (:require-macros
   [expectations.cljs :as expect-cljs])

  #+cljs
  (:require
   [expectations :as expect :refer-macros (expect)]
   ;; [clojure.test.check            :as dc]
   ;; [clojure.test.check.generators :as dc-gen]
   ;; [clojure.test.check.properties :as dc-prop :include-macros true]
   [taoensso.encore :as encore :refer ()]))

(comment (test/run-tests '[taoensso.encore.tests]))

#+cljs
(do
  (enable-console-print!)
  ;; TODO expct-cljs/run-all-tests appears to choke on macros?:
  (set! *main-cli-fn* (fn -main [] (expect-cljs/run-all-tests))))

(defn- before-run {:expectations-options :before-run} [])
(defn- after-run  {:expectations-options :after-run}  [])

(expect true)
