(ns taoensso.encore-tests.unrequired-ns
  "A namespace not required by any of the others,
  for testing auto-requiring resolves, etc.")

(def var-cljc
  #?(:clj  "foreign var-cljc/clj"
     :cljs "foreign var-cljc/cljs"))
