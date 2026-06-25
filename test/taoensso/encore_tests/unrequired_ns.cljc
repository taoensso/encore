(ns taoensso.encore-tests.unrequired-ns
  "For unit tests that need to check functionality
  involving foreign namespaces. NOT required by tests ns.")

(def ^:dummy-meta var-cljc
           "foreign.doc.cljc"
  #?(:clj  "foreign.val.cljc.clj"
     :cljs "foreign.val.cljc.cljs"))
