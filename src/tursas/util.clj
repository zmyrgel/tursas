(ns tursas.util
  (:require [clojure.contrib.string :as s]))

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

(defn char-digit?
  "Returns true if given character is a digit."
  [chr]
  (< -1 (Character/getNumericValue chr) 10))

(defn expand-digits
  "Expands digits in given character sequence by that many of given chars."
  [item coll]
  (loop [result '()
         items coll]
    (cond (empty? items) (reverse result)
          (char-digit? (first items))
          (recur (concat (repeat (Character/getNumericValue (first items)) item) result)
                 (rest items))
          :else (recur (cons (first items)  result)
                       (rest items)))))

(defn compact-item
  "Compacts items in coll to digits indicating their amount.
   Example:"
  [item coll]
  (loop [result '()
         items coll
         found 0]
    (cond (empty? items) (if (pos? found)
                           (reverse (cons found result))
                           (reverse result))
          (= (first items) item) (recur result (rest items) (inc found))
          :else (if (pos? found)
                  (recur (cons (char (+ 48 found)) result) items 0)
                  (recur (cons (first items) result) (rest items) found)))))

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

(defn- parse-fen-board
  "Parses fen string and returns character sequence for the board part."
  [fen]
  (first (split-on \space fen)))

(defn fen->ascii
  "Return printable string for the board from fen string.

 8| r n b q k b n r
 7| p p p p p p p p
 6| - - - - - - - -
 5| - - - - - - - -
 4| - - - - - - - -
 3| - - - - - - - -
 2| P P P P P P P P
 1| R N B Q K B N R
 -+----------------
  | a b c d e f g h"
  [fen]
  (apply str
         (let [rows (map #(cons (first %) (interpose \space (cons \| (rest %))))
                         (concat (map #(cons %1 %2)
                                      (iterate dec 8)
                                      (->> (parse-fen-board fen)
                                           (expand-digits \-)
                                           (split-on \/)))
                                 (list (cons \space (seq "abcdefgh")))))]
           (mapcat #(concat % '(\newline))
                   (concat (butlast rows) (cons (repeat 18 \-) (list (last rows))))))))



