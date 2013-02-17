(ns morsepeter.send
  (:import [framboos OutPin]))

(defn send-signals!
  ([signals]
     (send-signals! signals 1))
  ([signals pin-number]
     (with-open [out (OutPin.)]
       (.setValue out false)
       (doseq [[on-or-off millis] signals]
         (.setValue out on-or-off)
         (Thread/sleep millis))
       (.setValue out false))))