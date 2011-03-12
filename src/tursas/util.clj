(ns tursas.util
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq]
            [matchure :as m]))

(def promotion-chars "rnbq")

(defn valid-coord?
  "Predicate to check given coord for valid chess coordinate."
  [coord]
  (true?
   (some #(= coord %)
         (for [x (range 8) y (range 8)]
           (str (get "abcdefgh" x) (inc y))))))

(defn fen?
  "Checks if given string is valid FEN string.
   Doesn't do very precise work yet."
  [s]
  (and (string? s)
       (= (count (re-seq #"\S+" s)) 6)
       (= (count (re-seq #"/" s)) 7)))

(defn valid-variant?
  "Checks if given string is valid and supported variant."
  [s]
  (= s "normal"))

(defn result?
  "Checks if given string is game result eg. 1/2-1/2 {Draw} or similar."
  [s]
  (m/cond-match s
                #"1/2-1/2 \{.+\}" true
                #"1-0 \{.+\}" true
                #"0-1 \{.+\}" true
                ? false))

(defn split-move
  "Partitions chess move given in coordinate notation to pair of coordinates
   and possible promotion character."
  [algebraic]
  (filter (complement empty?)
          (s/partition #"[a-h0-8]{2}" algebraic)))

(defn coordinate-string?
  "Predicate to detect valid move strings in
   coordinate notation."
  [s]
  (let [parts (split-move s)]
    (or (= s "0000") ;; null move
        (and (or (== (count parts) 2)
                 (== (count parts) 3))
             (valid-coord? (first parts))
             (valid-coord? (second parts))
             (if (== (count parts) 3)
               (s/substring? (first (nthnext parts 2)) promotion-chars)
               true)))))

(defn san-string?
  "Predicate to see if given string represents chess move in SAN notation."
  [s]
  false)

(defn move-string?
  "Predicate to see if given string represents a valid chess move."
  [s]
  (or (coordinate-string? s)
      (san-string? s)))

(defn print-board
  "Return picture of board in ASCII from fen string."
  [fen]
  (let [fen-board (first (re-seq #"\S+" fen))]
    (str (s/map-str (fn [[index piece]]
                      (str (- 8 index) "|" piece "\n"))
                    (seq/indexed (->> fen-board
                                      (s/replace-by #"\d" #(str (s/repeat (Integer/parseInt %) \-)))
                                      (s/replace-by #"[\p{Alpha}-]" #(str \space %))
                                      (s/split #"/+"))))
         "------------------\n"
         " | a b c d e f g h\n")))

