(ns tursas.util
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq]))

(def promotion-chars "rnbq")

(defn split-on
  "Splits given sequence from sep to list of lists."
  [sep coll]
  (loop [result '()
         items coll]
    (cond (empty? items) (reverse result)
          :else (let [[head tail] (split-with #(not (= sep %)) items)]
                  (recur (cons head result)
                         (rest tail))))))

(defn valid-coord?
  "Checks if given string is valid chess board coordinate."
  [s]
  (true?
   (some #(= s %)
         (for [x (range 8) y (range 8)]
           (str (get "abcdefgh" x) (inc y))))))

(defn split-move
  "Partitions chess move given in coordinate notation to pair of coordinates
   and possible promotion character."
  [algebraic]
  (map #(apply str %1)
       (partition 2 2 "" algebraic)))

(defn coordinate-string?
  "Predicate to detect valid move strings in
   coordinate notation."
  [s]
  (let [parts (split-move s)]
    (and (or (== (count parts) 2)
             (== (count parts) 3))
         (valid-coord? (first parts))
         (valid-coord? (second parts))
         (if (== (count parts) 3)
           (.contains promotion-chars (first (nthnext parts 2)))
           true))))

(defn san-string?
  "Predicate to see if given string represents chess move in SAN notation."
  [s]
  false)

(defn move-string?
  "Predicate to see if given string represents a valid chess move."
  [s]
  (or (coordinate-string? s)
      (san-string? s)))

(defn fen->ascii
  "Return picture of board in ASCII from fen string."
  [fen]
  (let [fen-board (first (re-seq #"\S+" fen))]
    (str (apply str (map (fn [[index piece]]
                           (str (- 8 index) "|" piece "\n"))
                         (seq/indexed (->> fen-board
                                           (s/replace-by #"\d" #(str (s/repeat (Integer/parseInt %) \-)))
                                           (s/replace-by #"[\p{Alpha}-]" #(str \space %))
                                           (s/split #"/+")))))
         "------------------\n"
         " | a b c d e f g h\n")))

