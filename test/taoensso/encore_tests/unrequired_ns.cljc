(ns taoensso.encore-tests.unrequired-ns
  "For unit tests that need to check functionality
  involving foreign namespaces. NOT required by tests ns.")

(def ^:dummy-meta var-cljc
           "foreign.doc:var-cljc"
  #?(:clj  "foreign.val:var-cljc/clj"
     :cljs "foreign.val:var-cljc/cljs"))
