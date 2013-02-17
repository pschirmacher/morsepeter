(ns morsepeter.morse
  (:require [clojure.string :as s]))

(def sign->code {"A" "10111"
                 "B" "111010101"
                 "C" "11101011101"
                 "D" "1110101"
                 "E" "1"
                 "F" "101011101"
                 "G" "111011101"
                 "H" "1010101"
                 "I" "101"
                 "J" "1011101110111"
                 "K" "111010111"
                 "L" "101110101"
                 "M" "1110111"
                 "N" "11101"
                 "O" "11101110111"
                 "P" "10111011101"
                 "Q" "1110111010111"
                 "R" "1011101"
                 "S" "10101"
                 "T" "111"
                 "U" "1010111"
                 "V" "101010111"
                 "W" "101110111"
                 "X" "11101010111"
                 "Y" "1110101110111"
                 "Z" "11101110101"
                 "0" "1110111011101110111"
                 "1" "10111011101110111"
                 "2" "101011101110111"
                 "3" "1010101110111"
                 "4" "10101010111"
                 "5" "101010101"
                 "6" "11101010101"
                 "7" "1110111010101"
                 "8" "111011101110101"
                 "9" "11101110111011101"
                 "." "10111010111010111"
                 " " "0000000"})

(def code->sign (into {}
                      (for [[sign code] sign->code]
                        [code sign])))

(def between-signs #"000")

(def between-words #"0000000")

                                        ; DECODING

(defn decode-sign [bit-string]
  (if-let [sign (code->sign bit-string)]
    sign
    (do
      (println "unknown sign" bit-string)
      "")))

(defn decode-word [bit-string]
  (->> (s/split bit-string between-signs)
       (map decode-sign)
       s/join))

(defn decode-message
  "decodes a message, e.g.:
   (decode-message \"1110111011101110111000101010101000000010111\") -> \"05 A\""
  [bit-string]
  (let [words (map decode-word
                   (s/split bit-string between-words))]
    (s/join " " (rest words))))

(defn parse-message
  "parses a message, e.g.:
   (parse-message \"05 A\") -> [5 \"A\"]"
  [msg]
  (let [[first-word & more] (s/split msg #" ")]
    [(Integer/valueOf first-word) (s/join " " more)]))

                                        ; ENCODING

(defn encode-word [word]
  (let [codes (map (fn [sign]
                     (get sign->code (str sign) ""))
                   word)]
    (s/join (str between-signs) codes)))

(defn encode-message [msg]
  (let [words (s/split msg between-words)
        word-codes (map encode-word words)]
    (s/join (str between-words) word-codes)))

(defn serialize-message
  "takes a message and returns a string of bits, e.g.:
   (serialize-message [5 \"A\"]) -> \"1110111011101110111000101010101000000000000010111\""
  [[group text]]
  (encode-message (str (if (< group 10)
                         (str "0" group)
                         group)
                       " "
                        text)))