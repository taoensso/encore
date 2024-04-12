(ns taoensso.encore-tests.signals-api
  (:require
   [taoensso.encore.signals :as sigs]))

(do
  (def            ct-sig-filter  nil)
  (def ^:dynamic *rt-sig-filter* nil)
  (def ^:dynamic *sig-handlers*  nil))

(sigs/def-api
  {:purpose  "testing"
   :sf-arity 4
   :ct-sig-filter    ct-sig-filter
   :*rt-sig-filter* *rt-sig-filter*
   :*sig-handlers*  *sig-handlers*})

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
