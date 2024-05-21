(ns taoensso.encore-tests.signals-api
  (:require
   [taoensso.encore.signals :as sigs]))

(comment (remove-ns 'taoensso.encore-tests.signals-api))

(do
  (def            ct-sig-filter  nil)
  (def ^:dynamic *rt-sig-filter* nil)
  (def ^:dynamic *sig-handlers*  nil)
  (def ^:dynamic *auto-stop-handlers?* true))

(sigs/def-api
  {:purpose  "testing"
   :sf-arity 4
   :ct-sig-filter         ct-sig-filter
   :*rt-sig-filter*       *rt-sig-filter*
   :*sig-handlers*        *sig-handlers*
   :*auto-stop-handlers?* *auto-stop-handlers?*})

#?(:clj
   (defmacro sig-exp
     "Macro wrapper around `sigs/filterable-expansion`."
     {:arglists (:arglists (meta #'sigs/filterable-expansion))}
     [opts]
     `(quote
        ~(sigs/filterable-expansion
           {:macro-form &form
            :macro-env  &env
            :sf-arity   4
            :ct-sig-filter     ct-sig-filter
            :*rt-sig-filter* `*rt-sig-filter*}
           opts))))
