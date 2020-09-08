(ns taoensso.encore.test
  ;; Unfortunately needs to be a separate namespace for now due
  ;; to https://github.com/ptaoussanis/encore/issues/37
  #?(:clj
     (:require
      [clojure.test    :as test :refer [is]]
      [taoensso.encore :as enc  :refer [have? if-cljs]])

     :cljs
     (:require
      [cljs.test       :as test :refer-macros [is]]
      [taoensso.encore :as enc  :refer-macros [have? if-cljs]])))

(defmacro expect
  ([             expr] `(is                        ~expr))
  ([         val expr] `(is                (= ~val ~expr)))
  ([bindings val expr] `(is (let ~bindings (= ~val ~expr)))))

(comment
  (expect-let [foo {:a :A}] :A (:a foo))
  (expect (thrown? Exception "foo")))

(defn- fixture-map->fn [{:keys [before after] :or {before 'do after 'do}}]
  `(fn [f#] (~before) (f#) (~after)))

(defmacro use-fixtures "Cross-platform `test/use-fixtures`"
  [fixture-type & fixtures]
  (have? [:el #{:each :once}] fixture-type)
  (have? map? :in fixtures)
  `(if-cljs
        (cljs.test/use-fixtures ~fixture-type ~@fixtures)
     (clojure.test/use-fixtures ~fixture-type ~@(map fixture-map->fn fixtures))))

(comment (use-fixtures :each {:before (fn []) :after (fn [])}))
