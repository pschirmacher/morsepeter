(ns morsepeter.util)

(defn make-queue
  "returns a vector with two elements: an infinite lazy seq and a function that takes
   one argument and appends that argument to the seq"
  []
  (let [queue (java.util.concurrent.LinkedBlockingQueue. 1000) ;; fixed capacity, put-fn blocks when full
        nil-item (Object.)
        put-fn (fn [item]
                 (if (nil? item)
                   (.put queue nil-item)
                   (.put queue item)))
        queue-seq (repeatedly #(let [item (.take queue)]
                                 (if (identical? item nil-item)
                                   nil
                                   item)))]
    [queue-seq put-fn]))

(defn lazy-flatten
  "turns a nested collection (e.g. list of lists) into a flat sequence by removing one nesting level, e.g.:
   (lazy-flatten [[1 2 3] [4 [5]]] -> (1 2 3 4 [5]))
   this is lazier than (mapcat identity coll)"
  ([coll]
     (lazy-seq
      (when (seq coll)
        (lazy-flatten (first coll) (rest coll)))))
  ([head tail]
     (lazy-seq
      (if (seq head)
        (cons (first head)
              (lazy-flatten (rest head) tail))
        (lazy-flatten tail)))))

(defn trace
  ([x]
     (trace nil x))
  ([msg x]
     (if msg (println msg x) (println x))
     x))

(defn trace-seq
  ([coll]
     (trace-seq nil coll))
  ([msg coll]
     (map (partial trace msg) coll)))

(defn interleave-seqs
  "takes several seqs and returns an infinite lazy seq that contains everything from the given seqs;
   the input seqs are consumed in the background (on thread per seq)"
  [& seqs]
  (let [[queue put] (make-queue)]
    (doseq [s seqs]
      (.start (Thread. #(doseq [item s]
                          (put item)))))
    queue))