(ns tursas.hexmove
  (:use (tursas move)))

(defn index->algebraic
  "Converts given index to algebraic representation."
  [index]
  (let [coord (format "%02x" index)
        num (+ (- (int (nth coord 0)) 48) 1)
        alpha (get "abcdefgh" (- (int (nth coord 1)) 48))]
    (str alpha num)))

(defn algebraic->index
  "Converts given algebraic representation to board index value."
  [algebraic]
  (let [file (- (int (nth algebraic 0)) 97)
        rank (- (int (nth algebraic 1)) 48)]
    (+ (* (dec rank) 16) file)))

(defrecord HexMove [from to promotion]
  Move
  (move->algebraic [move]
                  (str (:from move)
                       (:to move)
                       (if (nil? (:promotion move))
                         ""
                         (:promotion move))))
  (from [move]
        (index->algebraic (:from move)))
  (to [move]
      (index->algebraic (:to move)))
  (promotion [move]
             (:promotion move)))

(defn make-move
  "Constructor for moves."
  [from to promotion]
  (cond (and (number? from)
             (number? to))
        (HexMove. from to promotion)
        (and (string? from)
             (string? to))
        (HexMove. (algebraic->index from)
                  (algebraic->index to)
                  promotion)
        :else (println "Invalid move arguments!")))

;;(extend-type HexMove
;;)

(defprotocol Algebraic
  (algebraic->move [algebraic]))

(extend-type String
  Algebraic
  (algebraic->move [algebraic]
                   (let [from (str (get algebraic 0)
                                   (get algebraic 1))
                         to (str (get algebraic 2)
                                 (get algebraic 3))
                         promotion (str (get algebraic 4))]
                     (make-move from to promotion))))
