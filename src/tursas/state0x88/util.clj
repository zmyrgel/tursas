(ns tursas.state0x88.util
  (:use (tursas.state0x88 common)))

(def board-indexes (mapcat #(map (fn [col] (+ col %)) (range 8))
                           [0x70 0x60 0x50 0x40 0x30 0x20 0x10 0x0]))

(defn abs [n]
  (cond (not (number? n)) (throw (IllegalArgumentException. (str "abs requires a number")))
        (neg? n) (- n)
        :else n))

(defn make-table
  "Utility to make full 0x88 vector out of smaller vector."
  [score-table]
  (into (vector-of :byte)
        (mapcat #(concat % (vec (repeat 8 0)))
                (partition 8 score-table))))

(def board-color
     (make-table [black white black white black white black white
                  white black white black white black white black
                  black white black white black white black white
                  white black white black white black white black
                  black white black white black white black white
                  white black white black white black white black
                  black white black white black white black white
                  white black white black white black white black]))

(defn board-index?
  "Does the given index represent a square on the board?"
  [index]
  (zero? (bit-and index 0x88)))

(defn empty-square?
  "Checks if given index on board is empty."
  [board index]
  (zero? (get board index)))

(defn column
  "Get the board column of the given square index.
   Columns start at 0 and go up to 7."
  [index]
  (bit-and index 7))

(defn row
  "Get the board row of the given square index.
   Rows start at 0 and they go up to 7."
  [index]
  (bit-shift-right index 4))

(defn same-column?
  "Determines if both given square indexes x and x are on the same column."
  [x y]
  (== (column x) (column y)))

(defn same-row?
  "Determines if both given square indexes x and y are on the same row."
  [x y]
  (== (row x) (row y)))

(defn white-piece?
  "Predicate to check if given piece value belongs to white."
  [piece]
  (> piece empty-square))

(defn black-piece?
  "Checks if given piece value belongs to black."
  [piece]
  (< piece empty-square))

(defn square-color
  "Returns the color of given square."
  [sq]
  (get board-color sq))

(defn same-color?
  "Check if two squares are same color."
  [sq1 sq2]
  (== (square-color sq1)
      (square-color sq2)))

(defn board-occupied?
  "Predicate to check if board index is occupied or not."
  [board index]
  (not (empty-square? board index)))

(defn occupied-by?
  "Checks if given board index is occupied by player."
  [board index player]
  (let [piece-color? (if (== player white)
                       white-piece?
                       black-piece?)]
    (and (board-occupied? board index)
         (piece-color? (get board index)))))

(defn opponent
  "Return opponent of given player"
  [player]
  (bit-xor player 1))

(defn promotion?
  "Checks if given move is pawn promotion."
  [piece move]
  (or (and (== piece white-pawn)
           (== (row (:to move)) 7))
      (and (== piece black-pawn)
           (== (row (:to move)) 0))))

(defn init-game-board
  "Generates new 128 element vector of bytes
   and places chess piece representation to it."
  []
  (into (vector-of :byte)
        (vec (replicate 128 empty-square))))

(defn fill-square
  "Return new board with given value added to given board's index."
  [board index value]
  (assoc board index value))

(defn clear-square
  "Clears the given square index on the game board."
  [board index]
  (fill-square board index empty-square))

(defn piece-name
  "Gives piece character representation from its board value."
  [piece]
  (cond (== piece white-king) \K
        (== piece white-queen) \Q
        (== piece white-bishop) \B
        (== piece white-knight) \N
        (== piece white-rook) \R
        (== piece white-pawn) \P
        (== piece black-king) \k
        (== piece black-queen) \q
        (== piece black-bishop) \b
        (== piece black-knight) \n
        (== piece black-rook) \r
        (== piece black-pawn) \p
        :else \E))

(defn piece-value
  "Gives pieces character numerical representation from its char."
  [char]
  (case char
        \P white-pawn
        \p black-pawn
        \R white-rook
        \r black-rook
        \N white-knight
        \n black-knight
        \B white-bishop
        \b black-bishop
        \Q white-queen
        \q black-queen
        \K white-king
        \k black-king
        empty-square))

(defn king-index
  "Gets the kings index in state for side."
  [board player]
  (get board (if (= player white)
               white-king-store
               black-king-store)))

(defn update-king-index
  "Updates index of given player's king in outer board."
  [board index player]
  (fill-square board
               (if (== player white)
                 white-king-store
                 black-king-store)
               index))
