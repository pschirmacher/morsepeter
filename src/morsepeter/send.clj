(ns morsepeter.send
  (:import [framboos OutPin]))

(defn send-signals!
  "set pin according to given signals, each signal is a vector with a boolean for on/off and the millis, e.g.:
   (send-signals! [[true 300] [false 100]]) -> sets pin to 'on' for 300 millis, then to 'off' for 100 millis
   always sets pin to 'off' after sending the signals"
  ([signals]
     (send-signals! signals 1))
  ([signals pin-number]
     (with-open [out (OutPin. pin-number)]
       (.setValue out false)
       (doseq [[on-or-off millis] signals]
         (.setValue out on-or-off)
         (Thread/sleep millis))
       (.setValue out false))))

(defn to-signals
  "takes a seq of bits and transforms it into signals, e.g.:
   (to-signals 100 [0 0 1 1 0]) -> ([false 200] [true 200] [false 100])"
  [tic-millis bits]
  (->> bits
       (partition-by #(= % 0))
       (map (fn [chunk]
              (let [on (= 1 (first chunk))
                    millis (* tic-millis (count chunk))]
                [on millis])))))

(defn surround-with [prefix postfix coll]
  (concat prefix coll postfix))

(defn to-bit-seq
  "transforms bit-string into seq of bits, e.g.:
   \"1101\" -> (1 1 0 1)"
  [bit-string]
  (map (fn [bit-char]
         (case bit-char
           \0 0
           \1 1))
       bit-string))

(defn message->signals
  "transforms given bit-string into signals that can be sent using 'send-signals!', e.g.:
   (message->signals 100 <start> <end> \"110001\") -> (<start> [true 200] [false 300] [true 100] <end>)"
  [tic-millis start-pattern end-pattern message]
  (->> message
       to-bit-seq
       (surround-with start-pattern end-pattern)
       (to-signals tic-millis)))