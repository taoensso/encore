(ns ^:no-doc taoensso.encore.test
  {:deprecated "Encore v3.31.0 (2022-10-27)"}
  (:require
   [clojure.test    :as test]
   [taoensso.truss  :as truss]
   [taoensso.encore :as enc]))

(enc/deprecated
  #?(:clj
     (defmacro ^:no-doc expect
       {:deprecated "Encore v3.31.0 (2022-10-27)"
        :doc "Prefer `clojure.test/is`, etc."}
       ([             expr] `(test/is                        ~expr))
       ([         val expr] `(test/is                (= ~val ~expr)))
       ([bindings val expr] `(test/is (let ~bindings (= ~val ~expr))))))

  (comment
    (expect-let [foo {:a :A}] :A (:a foo))
    (expect (thrown? Exception "foo")))

  (defn- fixture-map->fn [{:keys [before after] :or {before 'do after 'do}}]
    `(fn [f#] (~before) (f#) (~after)))

  #?(:clj
     (defmacro ^:no-doc use-fixtures
       {:deprecated "_Encore v3.31.0 (2022-10-27)"
        :doc "Prefer `encore/test-fixtures`"}
       [fixture-type & fixtures]
       (truss/have? [:el #{:each :once}] fixture-type)
       (truss/have? map? :in fixtures)
       `(enc/if-cljs
          (test/use-fixtures ~fixture-type ~@fixtures)
          (test/use-fixtures ~fixture-type ~@(map fixture-map->fn fixtures)))))

  (comment (use-fixtures :each {:before (fn []) :after (fn [])})))
