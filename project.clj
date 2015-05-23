(defproject com.taoensso/encore "1.30.0"
  :author "Peter Taoussanis <https://www.taoensso.com>"
  :description "Shared support utils for taoensso.com Clojure/Script libs"
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
  [[org.clojure/clojure      "1.4.0"]
   ;; [org.clojure/clojure   "1.5.1"] ; Soon...
   [org.clojure/tools.reader "0.9.2"]]

  :plugins
  [[lein-pprint                     "1.1.2"]
   [lein-ancient                    "0.6.7"]
   [lein-expectations               "0.0.8"]
   [lein-autoexpect                 "1.4.3"]
   ;; [com.cemerick/austin          "0.1.6"]
   [com.cemerick/clojurescript.test "0.3.3"]
   [codox                           "0.8.12"]]

  :profiles
  {;; :default [:base :system :user :provided :dev]
   :server-jvm {:jvm-opts ^:replace ["-server"]}
   :1.5  {:dependencies [[org.clojure/clojure "1.5.1"]]}
   :1.6  {:dependencies [[org.clojure/clojure "1.6.0"]]}
   :1.7  {:dependencies [[org.clojure/clojure "1.7.0-RC1"]]}
   :test {:dependencies [[expectations              "2.1.0"]
                         [com.cemerick/double-check "0.6.1"]]}
   :dev
   [:1.7 :test
    {:dependencies
     [;; Currently the latest release that doesn't cause Expectations v2.1.0 to
      ;; choke on macros, Ref. http://goo.gl/jJbLCm:
      ;; [org.clojure/clojurescript "0.0-2985"]
      [org.clojure/clojurescript    "0.0-3211"]
      [org.clojure/core.async       "0.1.303.0-886421-alpha"]]

     :plugins
     [;; These must be in :dev, Ref. https://github.com/lynaghk/cljx/issues/47:
      [com.keminglabs/cljx "0.6.0"]
      [lein-cljsbuild      "1.0.5"]]}]}

  ;; :jar-exclusions [#"\.cljx|\.DS_Store"]

  :source-paths ["src" "target/classes"]
  :test-paths   ["src" "test" "target/test-classes"]

  :cljx
  {:builds
   [{:source-paths ["src"]        :rules :clj  :output-path "target/classes"}
    {:source-paths ["src"]        :rules :cljs :output-path "target/classes"}
    {:source-paths ["src" "test"] :rules :clj  :output-path "target/test-classes"}
    {:source-paths ["src" "test"] :rules :cljs :output-path "target/test-classes"}]}

  :cljsbuild
  {:test-commands {"node"       ["node" :node-runner "target/tests.js"]
                   ;; "phantom" ["phantomjs" :runner "target/tests.js"]
                   }
   :builds
   ;; TODO Parallel builds currently cause issues with Expectations v2.1.0,
   ;; Ref. http://goo.gl/8LDHe5
   [{:id "main"
     :source-paths   ["src" "target/classes"]
     ;; :notify-command ["terminal-notifier" "-title" "cljsbuild" "-message"]
     :compiler       {:output-to "target/main.js"
                      :optimizations :advanced
                      :pretty-print false}}
    {:id "tests"
     :source-paths   ["src" "target/classes" "test" "target/test-classes"]
     :notify-command ["node" "target/tests.js"]
     :compiler       {:output-to "target/tests.js"
                      :optimizations :simple ; Necessary for node.js
                      :pretty-print true
                      :target :nodejs
                      :hashbang false ; Ref. http://goo.gl/vrtNDR
                      :main "taoensso.encore.tests"}}]}

  :auto-clean false
  :prep-tasks [["cljx" "once"] "javac" "compile"]

  :codox {:language :clojure ; [:clojure :clojurescript] cljsbuild  ; No support?
          :sources  ["target/classes"]
          :src-linenum-anchor-prefix "L"
          :src-dir-uri "http://github.com/ptaoussanis/encore/blob/master/src/"
          :src-uri-mapping {#"target/classes"
                            #(.replaceFirst (str %) "(.cljs$|.clj$)" ".cljx")}}

  :aliases
  {"test-all"   ["do" "clean," "cljx" "once,"
                 "with-profile" "default:+1.5:+1.6:+1.7" "expectations,"
                 "with-profile" "+test" "cljsbuild" "test"]
   "test-auto"  ["with-profile" "+test" "autoexpect"]
   "build-once" ["do" "clean," "cljx" "once," "cljsbuild" "once" "main"]
   "deploy-lib" ["do" "build-once," "deploy" "clojars," "install"]
   "start-dev"  ["with-profile" "+server-jvm" "repl" ":headless"]}

  :repositories {"sonatype-oss-public"
                 "https://oss.sonatype.org/content/groups/public/"})
