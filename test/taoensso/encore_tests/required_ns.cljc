(ns taoensso.encore-tests.required-ns
  "For unit tests that need to check functionality
  involving foreign namespaces. Required by tests ns."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]))

(comment (remove-ns 'taoensso.encore-tests.required-ns))

;;;; Signal API

(do
  (def            ct-sig-filter  nil)
  (def ^:dynamic *rt-sig-filter* nil)
  (def ^:dynamic *sig-handlers*  nil))

(sigs/def-api
  {:sf-arity 4
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
           {:sf-arity   4
            :ct-sig-filter     ct-sig-filter
            :*rt-sig-filter* `*rt-sig-filter*}
           (assoc opts
             :location* (enc/get-source &form &env))))))

(comment
  (add-handler!    :hid1 (fn [x]) {})
  (remove-handler! :hid1)
  (get-handlers)

  (set-ctx!  {:base 1})
  (with-ctx  {:ctx 2} (do *ctx*))
  (with-ctx+ {:ctx 2} (do *ctx*))

  (set-middleware! nil)
  (with-middleware identity (do)))

;;;; Stubs

(do     (enc/defstub astub-f1))
(do     (enc/defstub astub-fn))
#?(:clj (enc/defstub astub-m1))
#?(:clj (enc/defstub astub-d1))
