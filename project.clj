(defproject com.taoensso/encore "3.144.0"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Core utils library for Clojure/Script"
  :url "https://www.taoensso.com/encore"

  :license
  {:name "Eclipse Public License - v 1.0"
   :url  "https://www.eclipse.org/legal/epl-v10.html"}

  :test-paths ["test" #_"src"]

  :java-source-paths ["src/java"]
  :javac-options     ["--release" "8" "-g"] ; Support Java >= v8
  :dependencies
  [[org.clojure/tools.reader "1.5.2"]
   [com.taoensso/truss       "2.0.5"]]

  :profiles
  {;; :default [:base :system :user :provided :dev]
   :provided {:injections   [(println "Lein profile: :provided")]
              :dependencies [[org.clojure/clojurescript "1.11.132"]
                             [org.clojure/clojure       "1.11.4"]]}
   :c1.12    {:dependencies [[org.clojure/clojure       "1.12.0"]]}
   :c1.11    {:dependencies [[org.clojure/clojure       "1.11.4"]]}
   :c1.10    {:dependencies [[org.clojure/clojure       "1.10.3"]]}

   :graal-tests
   {:injections   [(println "Lein profile: :graal-tests")]
    :source-paths ["test"]
    :main taoensso.graal-tests
    :aot [taoensso.graal-tests]
    :uberjar-name "graal-tests.jar"
    :dependencies
    [[org.clojure/clojure                  "1.11.4"]
     [com.github.clj-easy/graal-build-time "1.0.5"]]}

   :test
   {:injections [(println "Lein profile: :test")]
    :aot [taoensso.encore-tests]}

   :test-shutdown
   {:injections   [(println "Lein profile: :test-shutdown")]
    :source-paths ["test-shutdown"]
    :main           test-shutdown}

   :dev
   {:injections [(println "Lein profile: :dev")]
    :jvm-opts
    ["-server"
     "-Dtaoensso.elide-deprecated=true"
     "-Dtaoensso.encore-tests.config.str=foo"
     "-Dtaoensso.encore-tests.config.clj.str=foo/clj"
     "-Dtaoensso.encore-tests.config.cljs.str=foo/cljs"
     "-Dtaoensso.encore-tests.config.bool=t"
     "-Dtaoensso.encore-tests.config.edn={:kw :my-kw, :str \"foo\", :int 5, :vec [:x]}"]

    :global-vars
    {*warn-on-reflection* true
     *assert*             true
     *unchecked-math*     false #_:warn-on-boxed}

    :dependencies
    [[org.clojure/test.check "1.1.1"]
     [org.clojure/core.async "1.8.741"]
     [thheller/shadow-cljs   "2.28.23"]
     [nrepl                  "1.3.1"]
     [cider/cider-nrepl      "0.55.1"]
     [com.google.javascript/closure-compiler-unshaded "v20240317"]]

    :plugins
    [[lein-pprint    "1.3.2"]
     [lein-ancient   "0.7.0"]
     [lein-cljsbuild "1.1.8"]]}}

  :cljsbuild
  {:test-commands {"node" ["node" "target/test.js"]}
   :builds
   [{:id :main
     :source-paths ["src"]
     :compiler
     {:output-to "target/main.js"
      :optimizations :advanced}}

    {:id :test
     :source-paths ["src" "test"]
     :compiler
     {:output-to "target/test.js"
      :target :nodejs
      :optimizations :simple}}]}

  :aliases
  {"start-dev"  ["with-profile" "+dev" "repl" ":headless"]
   "build-once" ["do" ["clean"] ["cljsbuild" "once"]]
   "deploy-lib" ["do" ["build-once"] ["deploy" "clojars"] ["install"]]

   "test-clj"      ["with-profile" "+c1.12:+c1.11:+c1.10" "test"]
   "test-cljs"     ["with-profile" "+c1.12" "cljsbuild"   "test"]
   "test-all"      ["do" ["clean"] ["test-clj"] ["test-cljs"] ["test-shutdown"]]
   "test-shutdown" ["with-profile" "+c1.12,+test-shutdown" "run"]})
