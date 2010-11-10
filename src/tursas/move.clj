(ns tursas.move)

;;; Move data structure holds information for given move
;;; and is presented in long algebraic notation
;;; The long algebraic notation is used in UCI and XBoard protocols
;;; to describe made chess moves.
;;; Examples:  e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion)

(defrecord Move [from to promotion])

(defn move->algebraic
  "Converts MOVE to long algebraic notation."
  [move]
    (str (:from move)
         (:to move)
         (if (nil? (:promotion move))
           ""
           (:promotion move))))

(defn algebraic->move
  "Converts long ALGEBRAIC notation to a Move structure
   eg. e2e4q to #(Move :to e2 :from e4 :promotion q)"
  [algebraic]
  (let [from  (str (get algebraic 0)
                   (get algebraic 1))
        to (str (get algebraic 2)
                (get algebraic 3))
        promotion (str (get algebraic 4))]
    (Move. from to promotion)))

(defn make-move
  "Constructor for moves."
  [from to promotion]
  (Move. from to promotion))
