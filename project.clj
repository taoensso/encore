(defproject com.taoensso/encore "3.28.3"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Core utils library for Clojure/Script"
  :url "https://github.com/ptaoussanis/encore"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "Same as Clojure"}
  :min-lein-version "2.3.3"
  :global-vars
  {*warn-on-reflection* true
   *assert*             true
   *unchecked-math*     false #_:warn-on-boxed}

  :dependencies
  [[org.clojure/tools.reader "1.3.6"]
   [com.taoensso/truss       "1.6.0"]]

  :plugins
  [[lein-pprint    "1.3.2"]
   [lein-ancient   "0.7.0"]
   [lein-codox     "0.10.8"]
   [lein-cljsbuild "1.1.8"]]

  :profiles
  {;; :default [:base :system :user :provided :dev]
   :server-jvm {:jvm-opts ^:replace ["-server"]}
   :provided {:dependencies [[org.clojure/clojurescript "1.11.60"]
                             [org.clojure/clojure       "1.11.1"]]}
   :c1.11    {:dependencies [[org.clojure/clojure       "1.11.1"]]}
   :c1.10    {:dependencies [[org.clojure/clojure       "1.10.3"]]}
   :c1.9     {:dependencies [[org.clojure/clojure       "1.9.0"]]}

   :depr     {:jvm-opts ["-Dtaoensso.elide-deprecated=true"]}
   :dev      [:c1.11 :test :server-jvm :depr]
   :test     {:dependencies [[org.clojure/test.check    "1.1.1"]
                             [org.clojure/core.async    "1.5.648"]]}}

  :test-paths ["src" "test"]

  :cljsbuild
  {:test-commands {"node" ["node" "target/main.js"]}
   :builds
   {:main
    {:source-paths ["src"]
     :compiler
     {:output-to "target/main.js"
      :optimizations :advanced
      :pretty-print false}}}}

  :aliases
  {"start-dev"  ["with-profile" "+dev" "repl" ":headless"]
   "deploy-lib" ["do" ["build-once"] ["deploy" "clojars"] ["install"]]
   "build-once" ["cljsbuild" "once"]

   "test-cljs"  ["do" ["clean"] "with-profile" "+test" "cljsbuild" "test"]
   "test-all"
   ["do" ["clean"]
    "with-profile" "+c1.11:+c1.10:+c1.9" "test,"
    "test-cljs"]}

  :repositories
  {"sonatype-oss-public"
   "https://oss.sonatype.org/content/groups/public/"})
