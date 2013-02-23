(ns morsepeter.receive
  (:import [framboos InPin])
  (:require [morsepeter.util :as u]))

(defonce stop (atom false))

(defn stop!
  "stop polling"
  []
  (reset! stop true)
  (Thread/sleep 1000)
  (reset! stop false))

(defn poll!
  "starts polling the input pin and calls callback with the last received signal whenever the signal changes - a signal is a
   vector with two elements: the last input (true or false) and the duration of that input in milliseconds
   example signal: [true 300]"
  ([callback]
     (poll! 0 10 callback))
  ([pin-number timeout callback]
     (with-open [in (InPin. pin-number)]
       (loop [last-on (.getValue in)
              last-time (System/currentTimeMillis)]
         (when (not @stop)
           (let [now-on (.getValue in)
                 now-time (System/currentTimeMillis)
                 duration (- now-time last-time)]
             (if (= now-on last-on)
               (do
                 (Thread/sleep timeout)
                 (recur last-on last-time))
               (do
                 (callback [last-on duration])
                 (Thread/sleep timeout)
                 (recur now-on now-time))))))
       (println "stopped polling"))
     (println "closed input pin")))

(defn receive-signals!
  "starts polling and creates an infinite lazy seq of the received signals, e.g.:
   ([false 7583] [true 300] [false 100]...)"
  []
  (let [[signals add-signal] (u/make-queue)]
    (.start (Thread. #(poll! add-signal)))
    signals))

(defn highs-or-lows
  "takes a signal and converts it to a seq of 0s or 1s for each 'tic', e.g.
   [true 300] -> (1 1 1) when tic-millis is 100 milliseconds"
  [tic-millis [high millis]]
  (let [tics (Math/round (/ millis (double tic-millis)))
        high-or-low (if high 1 0)]
    (repeat tics high-or-low)))

(defn drop-before
  "drops everything from the coll before the given prefix, e.g.:
   (drop-before [1 2] [0 1 2 3 4]) -> (1 2 3 4)"
  [prefix coll]
  (->> (partition (count prefix) 1 (take (count prefix) coll) coll)
       (drop-while #(not= prefix %))
       (map first)))

(defn split-at-first
  "splits the collection at the first appearance of the given pattern (inclusive), e.g.:
   (split-at-first [1 2] [0 1 2 3 4 1 2 3]) -> [(0 1 2) (3 4 1 2 3)]"
  [pattern-inclusive coll]
  (let [chunks (partition (count pattern-inclusive) 1 coll)]
    (if (some #{pattern-inclusive} chunks)
      (let [before-pattern (map first (take-while #(not= pattern-inclusive %) chunks))
            with-pattern (concat before-pattern pattern-inclusive)]
        [with-pattern (drop (count with-pattern) coll)])
      [() coll])))

(defn find-chunks
  "finds chunks in coll that start with start-pattern and end with end-pattern, drops everything in between, e.g.:
   (find-chunks [1 2] [4 3] [0 1 2 3 4 3 0 0 1 2 4 3 0]) -> ((1 2 3 4 3) (1 2 4 3))"
  [start-pattern end-pattern coll]
  (lazy-seq
   (let [[first-chunk more] (split-at-first end-pattern (drop-before start-pattern coll))]
     (cond (and (seq first-chunk) (seq more))
           (cons first-chunk (find-chunks start-pattern end-pattern more))
           (seq first-chunk)
           [first-chunk]
           :else
           ()))))

(defn extract-message-body
  "takes a collection of bits that starts with start-pattern and ends with end-pattern and returns the body as a bit-string, e.g.:
   (extract-message-body [<start-pattern> 1 0 1 0 0 1 <end-pattern>] <start-pattern> <end-pattern>) -> \"101001\""
  [start-pattern end-pattern bits]
  (->> bits
       (drop (count start-pattern))
       reverse
       (drop (count end-pattern))
       reverse
       (apply str)))

(defn signals->messages
  "returns messages as bit strings, e.g.:
   (\"111000111\" \"1010101\"...)"
  [tic-millis start-pattern end-pattern signals]
  (->> signals                                                          ;; ([false 400] [true 300] [false 300]...)
       (u/trace-seq "signal")
       (map (partial highs-or-lows tic-millis))                         ;; ((0 0 0 0) (1 1 1) (0 0 0)...)
       u/lazy-flatten                                                   ;; (0 0 0 0 1 1 1 0 0 0...) mapcat is not lazy enough
       (find-chunks start-pattern end-pattern)                          ;; ((<bits of first message>) (<bits of second message>)...)
       (u/trace-seq "bit message")
       (map (partial extract-message-body start-pattern end-pattern)))) ;; ("111000101" "1010111" ...)