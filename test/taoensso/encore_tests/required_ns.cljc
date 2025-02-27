(ns taoensso.encore-tests.required-ns
  "For unit tests that need to check functionality
  involving foreign namespaces. Required by tests ns."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]))

(comment (remove-ns 'taoensso.encore-tests.required-ns))

;;;; Signal API

(do
  (def            ct-call-filter  nil)
  (def ^:dynamic *rt-call-filter* nil)
  (def ^:dynamic *sig-handlers*   nil))

(sigs/def-api
  {:sf-arity 4
   :ct-call-filter    ct-call-filter
   :*rt-call-filter* *rt-call-filter*
   :*sig-handlers*   *sig-handlers*})

#?(:clj
   (defmacro filter-call
     "Example macro wrapper around `sigs/filter-call`."
     {:arglists  (:arglists (meta #'sigs/filter-call))}
     [opts]
     (let [result
           (sigs/filter-call
             {:cljs? (boolean (:ns &env))
              :sf-arity 4
              :ct-call-filter     ct-call-filter
              :*rt-call-filter* `*rt-call-filter*} opts)]
       `'~result)))

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
