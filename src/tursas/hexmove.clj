(ns tursas.hexmove
  (:use (tursas move)
        (tursas.state util0x88)))

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
    (str (index->algebraic (:from move))
         (index->algebraic (:to move))
         (piece-name (:promotion move))))
  (from [move]
    (index->algebraic (:from move)))
  (to [move]
    (index->algebraic (:to move)))
  (promotion [move]
    (when-not (zero? (:promotion move))
      (piece-name (:promotion move)))))

(defn make-move
  "Constructor for moves."
  [from to promotion]
  (cond (and (number? from)
             (number? to)
             (number? promotion))
        (HexMove. from to promotion)
        (and (string? from)
             (string? to)
             (string? promotion))
        (HexMove. (algebraic->index from)
                  (algebraic->index to)
                  (piece-value promotion))
        :else (println "Invalid move arguments!")))

(defn algebraic->move
  [algebraic]
  (let [from (str (get algebraic 0)
                  (get algebraic 1))
        to (str (get algebraic 2)
                (get algebraic 3))
        promotion (str (get algebraic 4))]
    (make-move from to promotion)))
