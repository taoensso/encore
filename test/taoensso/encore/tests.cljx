(ns taoensso.encore.tests
  #+cljs
  (:require-macros
    [expectations.cljs :as expect-cljs])

  (:require
    [taoensso.encore-tests]))

(comment (test/run-tests '[taoensso.encore.tests]))

#+cljs
(do
  (enable-console-print!)
  ;; TODO expct-cljs/run-all-tests appears to choke on macros?:
  (set! *main-cli-fn* (fn -main [] (expect-cljs/run-all-tests))))
