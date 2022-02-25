(defproject com.taoensso/encore "3.21.0"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Core utils library for Clojure/Script"
  :url "https://github.com/ptaoussanis/encore"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "Same as Clojure"}
  :min-lein-version "2.3.3"
  :global-vars {*warn-on-reflection* true
                *assert*             true
                ;; *unchecked-math*  :warn-on-boxed
                }

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
   :provided {:dependencies [[org.clojure/clojure       "1.7.0"]
                             [org.clojure/clojurescript "1.11.4"]]}
   :1.7      {:dependencies [[org.clojure/clojure       "1.7.0"]]}
   :1.8      {:dependencies [[org.clojure/clojure       "1.8.0"]]}
   :1.9      {:dependencies [[org.clojure/clojure       "1.9.0"]]}
   :1.10     {:dependencies [[org.clojure/clojure       "1.10.3"]]}
   :1.11     {:dependencies [[org.clojure/clojure       "1.11.0-rc1"]]}
   :test     {:dependencies [[org.clojure/test.check    "1.1.1"]
                             [org.clojure/core.async    "1.5.648"]]}
   :depr     {:jvm-opts ["-Dtaoensso.elide-deprecated=true"]}
   :dev      [:1.10 :test :server-jvm :depr]}

  :test-paths ["test" "src"]

  :cljsbuild
  {:builds
   {:main
    {:source-paths ["src" "test"]
     :compiler
     {:output-to "target/main.js"
      :optimizations :advanced
      :pretty-print false}}}

   :test-commands {"node" ["node" "target/main.js"]}}

  :aliases
  {"start-dev"  ["with-profile" "+dev" "repl" ":headless"]
   "deploy-lib" ["do" ["build-once"] ["deploy" "clojars"] ["install"]]
   "build-once" ["cljsbuild" "once"]
   "test-cljs"  ["cljsbuild" "test"]
   "test-all"
   ["do" ["clean"]
    "with-profile" "+1.10:+1.9:+1.8:+1.7" "test,"
    "with-profile" "+test" "cljsbuild" "test"]}

  :repositories
  {"sonatype-oss-public"
   "https://oss.sonatype.org/content/groups/public/"})
