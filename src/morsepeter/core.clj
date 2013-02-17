(ns morsepeter.core
  (:require [morsepeter.morse :as m]
            [morsepeter.receive :as r]
            [morsepeter.send :as s]
            [morsepeter.util :as u]))

(def dit-millis 100)
(def my-group 6)
(def min-group 1)
(def max-group 14)
(def msg-start-pattern [1 1 1 0 1 0 1 1 1 0 1 0 1 1 1])
(def msg-end-pattern [1 0 1 1 1 0 1 0 1 1 1 0 1])

(defn invalid-group? [[group _]]
  (or (= group my-group)
      (not (<= min-group group max-group))))

(defn get-wire-messages!
  "infinite lazy seq of incoming messages that should be relayed"
  []
  (->> (r/receive-signals!) ;; this starts a thread in the background that does the polling
       (partial r/signals->messages dit-millis msg-start-pattern msg-end-pattern)
       (map m/decode-message)
       (u/trace-seq "decoded message")
       (map m/parse-message)
       (remove invalid-group?)))

(defn send-messages!
  "starts a thread that takes care of sending the messages"
  [messages]
  (.start (Thread. #(doseq [msg messages]
                      (->> msg
                           m/serialize-message
                           (s/message->signals dit-millis msg-start-pattern msg-end-pattern)
                           s/send-signals!)))))

(defn keyboard-messaging
  "creates infinite lazy seq of keyboard messages that should be sent and a function that takes a message which is then added to that seq"
  []
  (let [[queue put] (u/make-queue)
        send-msg (fn [text]
                   (->> [my-group (.toUpperCase text)]
                        (u/trace "keyboard input")
                        put))]
    [queue send-msg]))

(defn run! []
  (let [[keyboard-messages send-message] (keyboard-messaging)
        wire-messages (get-wire-messages!)                                ;; starts a thread that polls input pin
        all-messages (u/interleave-seqs keyboard-messages wire-messages)] ;; starts two threads that wait for messages from input pin and keyboard
    (send-messages! all-messages)                                         ;; starts a thread that sends the messages
    send-message))

;; (def send (run!))