(ns test-shutdown
  "Manual tests related to JVM shutdown, etc."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]))

(do
  (def            ct-sig-filter        nil)
  (def ^:dynamic *rt-sig-filter*       nil)
  (def ^:dynamic *sig-handlers*        nil)
  (def ^:dynamic *auto-stop-handlers?* true)

  (sigs/def-api
    {:purpose "testing"
     :sf-arity 4
     :ct-sig-filter          ct-sig-filter
     :*rt-sig-filter*       *rt-sig-filter*
     :*sig-handlers*        *sig-handlers*
     :*auto-stop-handlers?* *auto-stop-handlers?*})

  (deftype MySignal []
    sigs/IFilterableSignal
    (allow-signal? [_ _] true)
    (signal-value  [_ _] ::dummy-signal-value)))

(defn -main [& args]
  (enc/println "Main starting")

  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [this thread throwable]
        (fn [throwable thread]
          (enc/println (str "Uncaught:" (ex-message throwable)))))))

  (.addShutdownHook (Runtime/getRuntime)
    (Thread. (fn [] (enc/println "Main shutdown hook running"))))

  #_(enc/threaded :user   (Thread/sleep 2000)) ; Does    block JVM shutdown
  #_(enc/threaded :daemon (Thread/sleep 2000)) ; Doesn't block JVM shutdown

  (let [r (enc/runner {})]
    (r
      (fn []
        (enc/println "Runner task starting")
        (Thread/sleep 5000)
        (enc/println "Runner task complete (**success**) <---"))))

  (add-handler! :test-handler
    (fn [x]
      (enc/println "Signal handler running")
      (Thread/sleep 5000)
      (enc/println "Signal handler complete (**success**) <---")))

  (sigs/call-handlers! *sig-handlers* (MySignal.))

  (enc/println "Main ending")
  (enc/println "Should see 2x \"**success**\" in upcoming output <---"))
