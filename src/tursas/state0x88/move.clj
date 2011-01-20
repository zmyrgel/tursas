(ns tursas.state0x88.move
  (:use (tursas move util)
        [tursas.state0x88.util :only [piece-name
                                      piece-value]]))

(defn index->coord
  "Converts given index to algebraic representation."
  [index]
  (let [coord (format "%02x" index)
        num (+ (- (int (nth coord 0)) 48) 1)
        alpha (get "abcdefgh" (- (int (nth coord 1)) 48))]
    (str alpha num)))

(defn coord->index
  "Converts given string in coordinate
   representation to board index value."
  [s]
  (when (valid-coord? s)
    (let [file (- (int (nth s 0)) 97)
          rank (- (int (nth s 1)) 48)]
      (+ (* (dec rank) 16) file))))

(defrecord HexMove [from to promotion]
  Move
  (move->coord [move]
    (str (index->coord (:from move))
         (index->coord (:to move))
         (let [piece (piece-name (:promotion move))]
           (when-not (= piece \E)
             piece))))
  (from [move]
    (index->coord (:from move)))
  (to [move]
    (index->coord (:to move)))
  (promotion [move]
    (when-not (zero? (:promotion move))
      (piece-name (:promotion move)))))

(defn make-move
  "Constructor for moves."
  [from to promotion]
  (HexMove. from to promotion))

(defn coord->move
  "Make a Move from given string.
   The string should be in coordinate notation."
  [s]
  (when (move-string? s)
    (let [parts (split-move s)]
      (make-move (coord->index (first parts))
                 (coord->index (second parts))
                 (if (== (count parts) 3)
                   (piece-value (get s 4))
                   0)))))
