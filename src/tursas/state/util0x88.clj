(ns tursas.state.util0x88
  (:use (tursas.state common0x88)))

(def board-indexes (reduce concat
                           (map #(map (fn [col] (+ col %)) (range 8))
                                [0x70 0x60 0x50 0x40 0x30 0x20 0x10 0x0])))

(def board-color [1 0 1 0 1 0 1 0 2 2 2 2 2 2 2 2
                  0 1 0 1 0 1 0 1 2 2 2 2 2 2 2 2
                  1 0 1 0 1 0 1 0 2 2 2 2 2 2 2 2
                  0 1 0 1 0 1 0 1 2 2 2 2 2 2 2 2
                  1 0 1 0 1 0 1 0 2 2 2 2 2 2 2 2
                  0 1 0 1 0 1 0 1 2 2 2 2 2 2 2 2
                  1 0 1 0 1 0 1 0 2 2 2 2 2 2 2 2
                  0 1 0 1 0 1 0 1 2 2 2 2 2 2 2 2])

(defn any?
  "Predicate to see if any of the items of coll return true on pred."
  [pred coll]
  (not (nil? (some pred coll))))

(defn board-index?
  "Does the given index represent a square on the board?"
  [index]
  (zero? (bit-and index 0x88)))

(defn empty-square?
  "Checks if given index on board is empty."
  [board index]
  (zero? (get board index)))

(defn column
  "Get the board column of the given square index."
  [index]
  (bit-and index 7))

(defn row
  "Get the board row of the given square index."
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
  (> piece EMPTY))

(defn black-piece?
  "Checks if given piece value belongs to black."
  [piece]
  (< piece EMPTY))

(defn same-color?
  "Check if two squares are same color."
  [sq1 sq2]
  (== (get board-color sq1)
      (get board-color sq2)))

(defn board-occupied?
  "Predicate to check if board index is occupied or not."
  [board index]
  (not (empty-square? board index)))

(defn occupied-by?
  "Checks if given board index is occupied by player."
  [board index player]
  (let [piece-color? (if (== player WHITE)
                       white-piece?
                       black-piece?)]
    (and (board-occupied? board index)
         (piece-color? (get board index)))))

(defn opponent
  "Return opponent of given player"
  [player]
  (bit-xor player 1))

(defn init-game-board
  "Generates new 128 element vector of bytes
   and places chess piece representation to it."
  []
  (into (vector-of :byte)
        (vec (replicate 128 EMPTY))))

(defn fill-square
  "Return new board with given value added to given board's index."
  [board index value]
  (assoc board index value))

(defn clear-square
  "Clears the given square index on the game board."
  [board index]
  (fill-square board index EMPTY))

(defn piece-name
  "Gives piece character representation from its board value."
  [piece]
  (cond (== piece WHITE-KING) \K
        (== piece WHITE-QUEEN) \Q
        (== piece WHITE-BISHOP) \B
        (== piece WHITE-KNIGHT) \N
        (== piece WHITE-ROOK) \R
        (== piece WHITE-PAWN) \P
        (== piece BLACK-KING) \k
        (== piece BLACK-QUEEN) \q
        (== piece BLACK-BISHOP) \b
        (== piece BLACK-KNIGHT) \n
        (== piece BLACK-ROOK) \r
        (== piece BLACK-PAWN) \p
        :else \E))

(defn piece-value
  "Gives pieces character numerical representation from its char."
  [char]
  (case char
        \P WHITE-PAWN
        \p BLACK-PAWN
        \R WHITE-ROOK
        \r BLACK-ROOK
        \N WHITE-KNIGHT
        \n BLACK-KNIGHT
        \B WHITE-BISHOP
        \b BLACK-BISHOP
        \Q WHITE-QUEEN
        \q BLACK-QUEEN
        \K WHITE-KING
        \k BLACK-KING
        EMPTY))

(defn king-index
  "Gets the kings index in state for side."
  [state player]
  (get (:board state) (if (= player WHITE)
                        WHITE-KING-STORE
                        BLACK-KING-STORE)))

(defn update-king-index
  "Updates index of given player's king in outer board."
  [board index player]
  (fill-square board
               (if (= player WHITE) WHITE-KING-STORE BLACK-KING-STORE)
               index))



