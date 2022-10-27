(ns taoensso.encore.test
  "DEPRECATED"
  (:require
   [clojure.test    :as test]
   [taoensso.encore :as enc  :refer [have? if-cljs]]))

(enc/deprecated ; 2022-10-26
  (defmacro expect
    "DEPRECATED"
    ([             expr] `(test/is                        ~expr))
    ([         val expr] `(test/is                (= ~val ~expr)))
    ([bindings val expr] `(test/is (let ~bindings (= ~val ~expr)))))

  (comment
    (expect-let [foo {:a :A}] :A (:a foo))
    (expect (thrown? Exception "foo")))

  (defn- fixture-map->fn [{:keys [before after] :or {before 'do after 'do}}]
    `(fn [f#] (~before) (f#) (~after)))

  (defmacro use-fixtures
    "DEPRECATED, prefer `encore/test-fixtures`."
    [fixture-type & fixtures]
    (have? [:el #{:each :once}] fixture-type)
    (have? map? :in fixtures)
    `(if-cljs
       (test/use-fixtures ~fixture-type ~@fixtures)
       (test/use-fixtures ~fixture-type ~@(map fixture-map->fn fixtures))))

  (comment (use-fixtures :each {:before (fn []) :after (fn [])})))
