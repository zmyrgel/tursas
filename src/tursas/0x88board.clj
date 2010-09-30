(ns tursas.0x88board
  (:use [clojure.contrib.math :only [abs]]))

;; direction vectors
(def NORTH 16)
(def NN (+ NORTH NORTH))
(def SOUTH -16)
(def SS (+ SOUTH SOUTH))
(def EAST 1)
(def WEST -1)
(def NE 17)
(def SW -17)
(def NW 15)
(def SE -15)

;; sides
(def WHITE 0)
(def BLACK 1)

;; board contents, add side to piece values
(def EMPTY -1)
(def PAWN 0)
(def ROOK 2)
(def KNIGHT 4)
(def BISHOP 6)
(def QUEEN 8)
(def KING 10)

;; piece material values
;; based on Shannon's work
(def PAWN-VALUE 1)
(def BISHOP-VALUE 3)
(def KNIGHT-VALUE 3)
(def ROOK-VALUE 5)
(def QUEEN-VALUE 9)
(def KING-VALUE 999)

;; sliding pieces
(def rook-directions '(NORTH SOUTH EAST WEST))
(def bishop-directions '(NW SW NE SE))
(def queen-directions (concat rook-directions bishop-directions))

;; moving pieces
(def king-movement queen-directions)
(def black-pawn-movement '(SE SW SOUTH))
(def white-pawn-movement '(NE NW NORTH))
(def knight-movement '(-33 -31 -18 -14 14 18 31 33))

(def start-state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def middle-state "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")

;; New types
(defrecord StateWith0x88 [board turn castling en-passant half-moves full-moves])

(defrecord Move [from to])

;; Predicates
(defn board-index?
  "Does the given INDEX represent a square on the board?"
  [^Byte index]
  (zero? (bit-and index 0x88)))

(defn empty-square?
  "Checks if given INDEX on BOARD is empty."
  [board index]
  (= (get board index) EMPTY))

(defn column
  "Get the board column of the given square INDEX."
  [index]
  (bit-and index 7))

(defn row
  "Get the board row of the given square INDEX."
  [index]
  (bit-shift-right index 4))

(defn same-column?
  "Determines if both given square indexes X and Y are on the same column."
  [x y]
  (= (column x) (column y)))

(defn same-row?
  "Determines if both given square indexes X and Y are on the same row."
  [x y]
  (= (row x) (row y)))

(defn occupied?
  "Checks if given INDEX is occupied on the BOARD."
  [board index]
  (not (= (get board index) EMPTY)))

(defn black?
  "Checks if given BOARD INDEX contains a black piece."
  [board index]
  (and (board-index? index)
       (>= (get board index) 0)
       (= (mod (get board index) 2) BLACK)))

(defn white?
  "Checks if given BOARD INDEX contains a white piece."
  [board index]
  (and (board-index? index)
       (>= (get board index) 0)
       (= (mod (get board index) 2) WHITE)))

(defn move-causes-check?
  "Checks if moving the piece in INDEX causes king to be threatened in STATE.
   The piece movement can cause this if it was blocking a sliding piece."
  [state index]
  (let [side (if (white? (:board state) index) WHITE BLACK)]

    )
  false)

;; Public functions

(defn piece-value->char
  "Gives piece character representation from its board VALUE."
  [value]
  (nth "PpRrNnBbQqKk" value))

(defn piece-char->value
  "Gives pieces character numerical representation from its CHAR."
  [char]
  (case char
        \P (+ PAWN WHITE)
        \p (+ PAWN BLACK)
        \R (+ ROOK WHITE)
        \r (+ ROOK BLACK)
        \N (+ KNIGHT WHITE)
        \n (+ KNIGHT BLACK)
        \B (+ BISHOP WHITE)
        \b (+ BISHOP BLACK)
        \Q (+ QUEEN WHITE)
        \q (+ QUEEN BLACK)
        \K (+ KING WHITE)
        \k (+ KING BLACK)
        EMPTY))

(defn piece-value->material-value
  "Gives material value for PIECE."
  [piece]
  (case piece
        \p (- PAWN-VALUE)
        \n (- KNIGHT-VALUE)
        \b (- BISHOP-VALUE)
        \r (- ROOK-VALUE)
        \q (- QUEEN-VALUE)
        \k (- KING-VALUE)
        \P PAWN-VALUE
        \N KNIGHT-VALUE
        \B BISHOP-VALUE
        \R ROOK-VALUE
        \Q QUEEN-VALUE
        \K KING-VALUE
        0))

;; Private functions
(defn index->algebraic
  "Converts given index to algebraic representation."
  [index]
  (let* [coord (format "%x" index)
         num (+ (- (int (nth coord 0)) 48) 1)
         alpha (get "abcdefgh" (- (int (nth coord 1)) 48))]
        (str alpha num)))

(defn algebraic->index
  "Converts given algebraic representation to board index value."
  [algebraic]
  (let [file (- (int (nth algebraic 0)) 97)
        rank (- (int (nth algebraic 1)) 48)]
    (+ (* (- 8 rank) 16) file)))

(defn- slide-in-direction
  "Returns a set of possible moves by sliding piece
   from INDEX to DIRECTION in given STATE."
  [state index direction]
  (let* [board (:board state)
         friendly? (if (black? board index) black? white?)]
    (loop [target-index (+ index direction)
           moves ()]
      (if (or (not (board-index? target-index))
              (friendly? board target-index))
        moves
        (if (empty-square? board (get board target-index))
          (recur (+ target-index direction)
                 (cons  (Move. index target-index) moves))
          (cons (Move. index target-index) moves))))))

(defn- move-to-place
  "Return set with index of possible move to given PLACE in given STATE."
  [state index place]
  (let* [board (:board state)
        friendly? (if (black? board index) black? white?)]
    (if (and (board-index? place)
             (not (friendly? board place)))
      (list (Move. index place))
      ())))

(defn- list-pawn-moves
  "Returns a set of available pawn moves from INDEX in given STATE."
  [state index]
  ;; XXX: add check for promotion
  (let* [board (:board state)
         side (if (black? board index) BLACK WHITE)
         friendly? (if (= side BLACK) black? white?)
         step (if (= side BLACK) SOUTH NORTH)
         move-index (+ index step)
         move-twice (or (and (= side BLACK) (same-row? index 96))
                        (and (= side WHITE) (same-row? index 16)))

         ;; calculate movement
         moves (if (and (board-index? move-index)
                        (empty-square? board move-index))
                 (if (and move-twice
                          (board-index? (+ move-index step))
                          (empty-square? board (+ move-index step)))
                   (list (Move. index move-index) (Move. index (+ move-index step)))
                   (list (Move. index move-index)))
                 ())

         ;; possible capture
         captures (if (= side BLACK)
                    (list (+ SW index) (+ SE index))
                    (list (+ NW index) (+ NE index)))
         en-passant-index (if (= (:en-passant state) "-")
                            -1
                            (algebraic->index (:en-passant state)))]

        ;; check capture points
        (flatten (conj moves (map #(if (or (and (board-index? %)
                                                (= en-passant-index %))
                                           (and (board-index? %)
                                                (occupied? board %)
                                                (not (friendly? board %))))
                                     (list (Move. index %))
                                     ()) captures)))))

(defn- list-moves-for-piece
  "Generates a set of all available moves for piece at INDEX in given STATE."
  [state index]
  (case (piece-value->char (get (:board state) index)) ;; toLower?
        \r (map #(slide-in-direction state index %) rook-directions)
        \b (map #(slide-in-direction state index %) bishop-directions)
        \q (map #(slide-in-direction state index %) queen-directions)
        \n (map #(move-to-place state index (+ index %)) knight-movement)
        \p (list-pawn-moves state index)
        \k (list-king-moves state index)
        ()))

(defn- all-piece-indexes-for
  "Gets a list of all board indexes containing
   SIDE's pieces in given STATE."
  [board side]
  (if (= side BLACK)
    (filter #(black? board %) (range 128))
    (filter #(white? board %) (range 128))))

(defn- king-index
  "Gets the kings index in STATE for SIDE."
  [state side]
  (let [board (:board state)
        king (if (= side BLACK)
               (piece-char->value \k)
               (piece-char->value \K))]
    (first (filter #(= (get board %) king) (range 128)))))

(defn index-under-threat?
  "Checks if given INDEX in STATE is under threath of enemy."
  [state index]
  (let* [opponent (opponent state)
         moves (map :to (flatten (map #(list-moves-for-piece state %)
                                      (all-piece-indexes-for (:board state) opponent))))]
        (not (nil? (some index moves)))))

(defn in-check?
  "Checks given STATE has king in check."
  [state]
  (let* [side (if (= (:turn state) "w") WHITE BLACK)]
        (index-under-threat? state (king-index state side))))

(defn commit-move
  "Commits given MOVE in STATE and return the new state."
  [state move]
  (let* [side (if (black? (:board state) (:from move)) BLACK WHITE)
         moving-piece (get (:board state) (:from move))
         en-passant (if (and (or (= moving-piece 0)
                                 (= moving-piece 1))
                             (= (abs (- (:to move) (:from move))) 0x20))
                      (index->algebraic (/ (+ (:to move) (:from move)) 2))
                      "-")
         castling (if (and (or (= moving-piece (+ KING WHITE))
                               (= moving-piece (+ KING BLACK)))
                           (= (abs (- (:to move) (:from move)) 0x2)))
                    "castling done"
                    "no castling")
         turn (if (= side BLACK) "w" "b")
         half-moves (+ 1) ;; add half moves
         full-moves (if (= side BLACK)
                      (+ (:full-moves state) 1)
                      (:full-moves state))

         board (fill-square
                (clear-square (:board state) (:from move))
                (:to move))]

        (StateWith0x88. board turn en-passant castling half-moves full-moves)))

(defn all-moves-for
  "Returns a set of all available moves for SIDE in STATE."
  [state side]
  (let [all-moves (flatten (map #(list-moves-for-piece state %)
                                (all-piece-indexes-for (:board state) side)))]
    all-moves))
;;(filter #(in-check? (commit-move state %)) all-moves)

(defn clear-en-passant
  "Makes a new state without an en passant move from given STATE."
  [state]
  (assoc state :en-passant "-"))

(defn init-game-board
  "Generates new 128 element vector of bytes
   and places chess piece representation to it."
  []
  (into (vector-of :byte)
        (vec (replicate 128 -1))))

(defn clear-square
  "Clears the given square INDEX on the game state."
  [board index]
  (assoc board index EMPTY))

(defn fill-square
  "Return new STATE with given PIECE of SIDE added to given STATE's INDEX."
  [board index piece-value]
  (assoc board index piece-value))

(defn fen-board->0x88board
  "Parses board information from FEN-BOARD field."
  [fen-board]
  (loop [board (init-game-board)
         row 7
         col 0
         fen fen-board]
    (if (empty? fen)
      board
      (cond (.contains "KQBNRPkqbnrp" (str (first fen)))
            (recur (fill-square board
                                (+ (* row 16) col)
                                (piece-char->value (first fen)))
                   row
                   (inc col)
                   (rest fen))
            (= \/ (first fen))
            (recur board
                   (dec row)
                   0
                   (rest fen))
            (.contains "12345678" (str (first fen)))
            (recur board
                   row
                   (+ col (- (int (first fen)) 48))
                   (rest fen))))))

(defn fen->0x88
  "Converts FEN string to 0x88 board representation."
  [fen]
  (let [fen-list (re-seq #"\S+" fen)]
    (when (= (count fen-list) 6)
      (StateWith0x88. (fen-board->0x88board (first fen-list))    ;; board
                      (if (= (second fen-list) "w") WHITE BLACK) ;; turn
                      (nth fen-list 2)                           ;; castling
                      (nth fen-list 3)                           ;; en-passant
                      (Integer/parseInt (nth fen-list 4))        ;; half turns
                      (Integer/parseInt (nth fen-list 5))))))    ;; full turns

(defn board->fen-board
  "Convert the given state's BOARD to fen board field."
  [board]
  (loop [index 119 ;; start loop from H8 backwards
         fen ""
         empty 0
         add-slash false]
    (let [last-place (zero? (mod index 16))]
      (cond (= index -1)
            fen
            (not (board-index? index))
            (recur (dec index)
                   fen
                   0
                   true)
            add-slash
            (recur index
                   (str fen "/")
                   0
                   false)
            last-place
            (recur (dec index)
                   (if (= (get board index) EMPTY)
                     (str fen (inc empty))
                     (str fen
                          (if (> empty 0) empty "")
                          (piece-value->char (get board index))))
                   0
                   false)
            (= (get board index) EMPTY)
            (recur (dec index)
                   fen
                   (inc empty)
                   false)
            :else
                 (recur (dec index)
                        (str fen
                             (if (> empty 0) empty "")
                             (piece-value->char (get board index)))
                         0
                         false)))))

(defn state->fen
  "Makes a given STATE to FEN."
  [state]
  (str (board->fen-board (:board state)) " "
       (if (= (:turn state) WHITE) "w" "b") " "
       (:castling state) " "
       (:en-passant state) " "
       (:half-moves state) " "
       (:full-moves state)))

(defn evaluate-state
  "Evaluates given game STATE."
  [state]
  (reduce + (map #(if (board-index? %)
                    (piece-char->value (get (:board state) %))
                    0) (range 128))))
