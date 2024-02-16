(ns taoensso.encore-tests.unrequired-ns
  "A namespace not required by any of the others,
  for testing auto-requiring resolves, etc.")

(def ^:dummy-meta var-cljc
           "foreign.doc:var-cljc"
  #?(:clj  "foreign.val:var-cljc/clj"
     :cljs "foreign.val:var-cljc/cljs"))
