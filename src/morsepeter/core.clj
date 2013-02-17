(ns morsepeter.core
  (:require [morsepeter.morse :as m]
            [morsepeter.receive :as r]
            [morsepeter.util :as u]))

(def my-group 6)
(def min-group 1)
(def max-group 14)

(defn invalid-group? [[group _]]
  (or (= group my-group)
      (not (<= min-group group max-group))))

(defn get-wire-input!
  "infinite lazy seq of incoming messages that should be relayed"
  []
  (->> (r/receive-signals!) ;; this starts a thread in the background that does the polling
       r/signals->messages
       (map m/decode-message)
       (u/trace-seq "decoded message")
       (map m/parse-message)
       (remove invalid-group?)))

(defn run! []
  (.start (Thread. #(let [items (get-wire-input!)]
                      (doseq [item items]
                        (println item))))))