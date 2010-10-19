(ns tursas.move
  (:use [tursas.game]))

;; from and to are in algebraic format such as e4, h5
;; promotion is lower case letter for the piece such as q or r
(defrecord Move [from to promotion])

(defn- index->algebraic
  "Converts given index to algebraic representation."
  [index]
  (let* [coord (format "%x" index)
         num (+ (- (int (nth coord 0)) 48) 1)
         alpha (get "abcdefgh" (- (int (nth coord 1)) 48))]
        (str alpha num)))

(defn- algebraic->index
  "Converts given algebraic representation to board index value."
  [algebraic]
  (let [file (- (int (nth algebraic 0)) 97)
        rank (- (int (nth algebraic 1)) 48)]
    (+ (* (- 8 rank) 16) file)))

(defn move->algebraic
  "Converts MOVE to algebraic notation to better communicate with others."
  [move]
  (let* [to-part (index->algebraic (:to move))
         from-part (index->algebraic (:from move))]
        (str to-part from-part)))

(defn algebraic->move
  "Converts ALGEBRAIC notation to move
   eg. e2e4 to #(Move :to 84 :from 90 :promotion nil)"
  [algebraic]
  (let [from (algebraic->index (str (get algebraic 0)
                                    (get algebraic 1)))
        to (algebraic->index (str (get algebraic 2)
                                  (get algebraic 3)))
        promotion (str (get algebraic 4))]
    (Move. from to promotion)))

