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

(def KING-SIDE 0)
(def QUEEN-SIDE 1)

;; board contents
(def EMPTY -1)
(def WHITE-PAWN 0)
(def BLACK-PAWN 1)
(def WHITE-ROOK 2)
(def BLACK-ROOK 3)
(def WHITE-KNIGHT 4)
(def BLACK-KNIGHT 5)
(def WHITE-BISHOP 6)
(def BLACK-BISHOP 7)
(def WHITE-QUEEN 8)
(def BLACK-QUEEN 9)
(def WHITE-KING 10)
(def BLACK-KING 11)

;; piece material values
;; based on Shannon's work
(def PAWN-VALUE 10)
(def BISHOP-VALUE 30)
(def KNIGHT-VALUE 30)
(def ROOK-VALUE 50)
(def QUEEN-VALUE 90)
(def KING-VALUE 9999)

(def OPENING-GAME 0)
(def MIDDLE-GAME 1)
(def END-GAME 2)

;; sliding pieces
(def rook-directions (list NORTH SOUTH EAST WEST))
(def bishop-directions (list NW SW NE SE))
(def queen-directions (concat rook-directions bishop-directions))

;; moving pieces
(def king-movement queen-directions)
(def black-pawn-movement (list SE SW SOUTH))
(def white-pawn-movement (list NE NW NORTH))
(def knight-movement (list -33 -31 -18 -14 14 18 31 33))

(def start-state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def middle-state "r3kbnr/pppq1ppp/n3p3/3p1b2/1PN5/B1P2N2/P2PPPPPP/R2QKBR1 b Qkq - 5 5")
(def end-state "4k3/8/8/8/8/8/4P3/4K3 w - - 5 39")

;; New types
(defrecord StateWith0x88 [board turn castling en-passant half-moves full-moves])
(defrecord Move [from to])

(def white-pawn-table (into (vector-of :byte)
                            [0   0   0   0   0   0   0   0
                             50  50  50  50  50  50  50  50
                             10  10  20  30  30  20  10  10
                             5   5   10  27  27  10  5   5
                             0   0   0   25  25  0   0   0
                             5  -5  -10  0   0  -10 -5   5
                             5   10  10 -25 -25  10  10  5
                             0   0   0   0   0   0   0   0]))
(def black-pawn-table (reverse white-pawn-table))

(def knight-table (into (vector-of :byte)
                        [-50 -40 -20 -30 -30 -20 -40 -50
                         -40 -20   0   5   5   0 -20 -40
                         -30   5  10  15  15  10   5 -30
                         -30   0  15  20  20  15   0 -30
                         -30   0  15  20  20  15   0 -30
                         -30   5  10  15  15  10   5 -30
                         -40 -20   0   5   5   0 -20 -40
                         -50 -40 -20 -30 -30 -20 -40 -50]))

(def white-bishop-table (into (vector-of :byte)
                              [-20 -10 -10 -10 -10 -10 -10 -20
                               -10   0   0   0   0   0   0 -10
                               -10   0   5  10  10   5   0 -10
                               -10   5   5  10  10   5   5 -10
                               -10   0  10  10  10  10   0 -10
                               -10  10  10  10  10  10  10 -10
                               -10   5   0   0   0   0   5 -10
                               -20 -10 -40 -10 -10 -40 -10 -20]))
(def black-bishop-table (reverse white-bishop-table))

(def white-king-table (into (vector-of :byte)
                            [-30  -40  -40  -50  -50  -40  -40  -30
                             -30  -40  -40  -50  -50  -40  -40  -30
                             -30  -40  -40  -50  -50  -40  -40  -30
                             -30  -40  -40  -50  -50  -40  -40  -30
                             -20  -30  -30  -40  -40  -30  -30  -20
                             -10  -20  -20  -20  -20  -20  -20  -10
                             20   20    0    0    0    0   20   20
                             20   30   10    0    0   10   30   20]))
(def black-king-table (reverse white-king-table))

(def white-king-table-end-game (into (vector-of :byte)
                                     [-50 -40 -30 -20 -20 -30 -40 -50
                                      -30 -20 -10   0   0 -10 -20 -30
                                      -30 -10  20  30  30  20 -10 -30
                                      -30 -10  30  40  40  30 -10 -30
                                      -30 -10  30  40  40  30 -10 -30
                                      -30 -10  20  30  30  20 -10 -30
                                      -30 -30   0   0   0   0 -30 -30
                                      -50 -30 -30 -30 -30 -30 -30 -50]))
(def black-king-table-end-game (reverse white-king-table-end-game))

;; variables
(def *search-depth* (ref 2))

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

(defn opponent
  "Returns the side of the opponent of the given STATE."
  [state]
  (if (= (:turn state) "w") WHITE BLACK))

;; Public function
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
  "Clears the given square INDEX on the game BOARD."
  [board index]
  (assoc board index EMPTY))

(defn fill-square
  "Return new board with given PIECE-VALUE added to given BOARD's INDEX."
  [board index piece-value]
  (assoc board index piece-value))

(defn piece-value->char
  "Gives piece character representation from its board VALUE."
  [value]
  (nth "PpRrNnBbQqKk" value))

(defn piece-char->value
  "Gives pieces character numerical representation from its CHAR."
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

(defn- commit-castle-move
  "Make castling move on board."
  [board move castling-side]
  (let* [side (if (white? (get board (:from move))) WHITE BLACK)
         rook (if (= side WHITE) WHITE-ROOK BLACK-ROOK)
         king (if (= side WHITE) WHITE-KING BLACK-KING)
         rook-from (if (= castling-side QUEEN-SIDE)
                     (if (= side WHITE) 0 112)
                     (if (= side WHITE) 7 119))
         rook-to (if (= castling-side QUEEN-SIDE)
                   (if (= side WHITE) 3 115)
                   (if (= side WHITE) 5 117))
         temp-board (clear-square board (:from move))
         temp-board (clear-square temp-board rook-from)
         temp-board (fill-square temp-board (:to move) king)]
        (fill-square temp-board temp-board rook-to rook)))

(defn commit-move
  "Commits given MOVE in STATE and return the new state."
  [state move]
  (let* [board (:board state)
         to-index (:to move)
         from-index (:from move)
         side (if (black? board from-index)
                BLACK
                WHITE)

         moving-piece (get board from-index)

         turn (if (= side BLACK) "w" "b")

         ;; pawn moves
         pawn-or-capture-move? (or (or (= moving-piece WHITE-PAWN)
                                       (= moving-piece BLACK-PAWN))
                                   (not (= (get board to-index) EMPTY)))
         en-passant-string (if (and (or (= moving-piece WHITE-PAWN)
                                        (= moving-piece BLACK-PAWN))
                                    (= (abs (- to-index from-index)) 0x20))
                             (index->algebraic (/ (+ to-index from-index) 2))
                             "-")
         promotion? (or (and (= moving-piece WHITE-PAWN)
                             (= (row to-index) 7))
                        (and (= moving-piece BLACK-PAWN)
                             (= (row to-index) 0)))

         ;; castling checks
         side-castling (if (= (:castling state) "-")
                         "-"
                         (if (= side WHITE)
                           (reduce str (re-seq #"\p{Upper}" (:castling state)))
                           (reduce str (re-seq #"\p{Lower}" (:castling state)))))
         castling? (and (or (= moving-piece WHITE-KING)
                            (= moving-piece BLACK-KING))
                        (= (abs (- to-index from-index)) 2))
         castling-string (:castling state)

         half-moves (if pawn-or-capture-move? 0 (+ (:half-moves state) 1))

         full-moves (if (= side BLACK)
                      (+ (:full-moves state) 1)
                      (:full-moves state))

         ;; make changes to board
         new-board (cond
                    promotion? (fill-square (clear-square board from-index)
                                            to-index
                                            (if (= side WHITE)
                                              WHITE-QUEEN
                                              BLACK-QUEEN))
                    castling? (commit-castle-move board
                                                  move
                                                  (if (= column to-index 2)
                                                    QUEEN-SIDE
                                                    KING-SIDE))
                    :else (fill-square (clear-square board from-index)
                                       to-index
                                       moving-piece))]
        (StateWith0x88. new-board
                        turn
                        castling-string
                        en-passant-string
                        half-moves
                        full-moves)))

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
      '())))

(defn- ray-to-pieces?
  "Checks if there's ray to from INDEX to given PIECES."
  [board index inc pieces]
  (cond (not (board-index? index)) false
        (empty-square? board index) (recur board (+ index inc) inc pieces)
        :else (nil? (some #{(get board index)} pieces))))

(defn index-under-threat?
  "Checks if given INDEX in STATE is under threath of enemy."
  [state index opponent]
  (let* [board (:board state)]
        (or
         ;; check if opponent's knight can attack index
         (nil? (some #(= (get board %)
                         (if (= opponent WHITE)
                           WHITE-KNIGHT
                           BLACK-KING)) (map #(+ index %) knight-movement)))
        ;; check if there's ray to opponents queen or
        ;; bishop diagonally from index
        (not (nil? (some true? (map #(ray-to-pieces? board index %
                                                     (if (= opponent WHITE)
                                                       [WHITE-QUEEN WHITE-BISHOP]
                                                       [BLACK-QUEEN BLACK-BISHOP]))
                                    [NE NW SW SE]))))

        ;; check if there's ray to opponents queen or
        ;; rook on the same column or row
        (not (nil? (some true? (map #(ray-to-pieces? board index %
                                                     (if (= opponent WHITE)
                                                       [WHITE-QUEEN WHITE-ROOK]
                                                       [BLACK-QUEEN BLACK-ROOK]))
                                    [NORTH EAST WEST SOUTH]))))

        ;; check pawns
        (if (= opponent WHITE)
          (or (= (get board (+ index SE)) WHITE-PAWN)
              (= (get board (+ index SW)) WHITE-PAWN))
          (or (= (get board (+ index NE)) BLACK-PAWN)
              (= (get board (+ index NW)) BLACK-PAWN)))


        ;; check kings if there's king next to index and
        ;; it can attack index
        (let [side (if (= opponent WHITE) BLACK WHITE)
              own-king (if (= side WHITE) WHITE-KING BLACK-KING)
              enemy-king-index (filter #(= (get board  %) own-king)
                                       (map #(+ index %) king-movement))]
          (if (empty? enemy-king-index)
            false
            (index-under-threat?
             (commit-move state (Move. enemy-king-index index))
             index side))))))

(defn- legal-castling?
  [state index increment]
  (let [side (if (= (get (:board state) index) WHITE) WHITE BLACK)]
    (loop [index (+ index increment)
         king-squares 2]
      (cond (> king-squares 0)
            (if (or (occupied? (:board state) index)
                    (index-under-threat? state index side))
              false
              (recur (+ index increment) (- king-squares 1)))
            :else (if (occupied? index)
                    false
                    (or (= (get (:board state)
                                (+ index increment)) WHITE-ROOK)
                        (= (get (:board state)
                                (+ index increment)) BLACK-ROOK)))))))

(defn- list-king-moves
  "Resolves all available moves for king in given INDEX of STATE."
  [state index]
  (let* [side (if (black? (:board state) index) BLACK WHITE)
         castling (:castling state)
         normal-moves (flatten (map #(move-to-place state index (+ index %))
                                   king-movement))
         castling-king-side (some #{(if (= side BLACK) \k \K)} castling)
         castling-queen-side (some #{(if (= side BLACK) \q \Q)} castling)
         castling-moves-king (if (and (not (nil? castling-king-side))
                                      (legal-castling? state index EAST))
                               (Move. index (* WEST 2))
                               '())
         castling-moves-queen (if (and (not (nil? castling-queen-side))
                                       (legal-castling? state index WEST))
                                (Move. index (* EAST 2))
                                '())]
        (concat normal-moves castling-moves-king castling-moves-queen)))

(defn- list-pawn-moves
  "Returns a set of available pawn moves from INDEX in given STATE."
  [state index]
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
                   (list (Move. index move-index)
                         (Move. index (+ move-index step)))
                   (list (Move. index move-index)))
                 '())

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
                                     '()) captures)))))

(defn- list-moves-for-piece
  "Generates a set of all available moves for piece at INDEX in given STATE."
  [state index]
  (case (Character/toLowerCase (piece-value->char (get (:board state) index)))
        \r (map #(slide-in-direction state index %) rook-directions)
        \b (map #(slide-in-direction state index %) bishop-directions)
        \q (map #(slide-in-direction state index %) queen-directions)
        \n (map #(move-to-place state index (+ index %)) knight-movement)
        \p (list-pawn-moves state index)
        \k (list-king-moves state index)
        '()))

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

(defn in-check?
  "Checks given STATE has king in check."
  [state]
  (let* [side (if (= (:turn state) "w") WHITE BLACK)]
        (index-under-threat? state (king-index state side) side)))

(defn all-moves-for
  "Returns a set of all available moves for SIDE in STATE."
  [state side]
  (let [all-moves (flatten (map #(list-moves-for-piece state %)
                                (all-piece-indexes-for (:board state) side)))]
    (filter #(in-check? (commit-move state %)) all-moves)))

(defn available-states-from
  "Lists all legal child states from given game STATE."
  [state]
  (let [side (if (= (:turn state) "w") WHITE BLACK)]
    (map #(commit-move state %) (all-moves-for state side))))

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
      (StateWith0x88. (fen-board->0x88board (first fen-list))
                      (if (= (second fen-list) "w") WHITE BLACK)
                      (nth fen-list 2)
                      (nth fen-list 3)
                      (Integer/parseInt (nth fen-list 4))
                      (Integer/parseInt (nth fen-list 5))))))

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

(defn piece-index-score
  "Checks piece-specific index score"
  [piece index game-situation]
  (case piece
        WHITE-PAWN (get white-pawn-table index)
        BLACK-PAWN (get black-pawn-table index)
        WHITE-KNIGHT (get knight-table index)
        BLACK-KNIGHT (get knight-table index)
        WHITE-BISHOP (get white-bishop-table index)
        BLACK-BISHOP (get black-bishop-table index)
        WHITE-KING (if (= game-situation END-GAME)
                     (get white-king-table-end-game index)
                     (get white-king-table index))
        BLACK-KING (if (= game-situation END-GAME)
                     (get black-king-table-end-game index)
                     (get black-king-table index))
        0))

(defn evaluate-state
  "Evaluates given game STATE.
   Simply calculates the material balance of the board."
  [state]
  (let* [board (:board state)
         game-situation (cond
                         (< (+ (count (all-piece-indexes-for board BLACK))
                               (count (all-piece-indexes-for board WHITE))) 15)
                         END-GAME
                         (> (:full-moves state) 10)
                         MIDDLE-GAME
                         :else OPENING-GAME)
         state-score (reduce +
                             (map
                              #(if (board-index? %)
                                 (let [piece (get board %)]
                                   (+ (piece-value->material-value piece)
                                      (piece-index-score piece % game-situation)))
                                 0)
                              (range 128)))]
        state-score))

(defn minimax-search
  "Search STATEs with Minimax algorithm until DEPTH and use EVAL to
  evaluate results."
  [state depth eval]
  (if (= depth 0)
    (eval state)
    (let [children (available-states-from state)]
      (loop [states children
             best-state nil
             best-value nil]
        (if (empty? states)
          best-value
          (let [value (- (minimax-search (first states) (- depth 1) eval))]
            (if (or (nil? best-value)
                    (> value best-value))
              (recur (rest states) (first states) value)
              (recur (rest states) best-state best-value))))))))

(defn- move->algebraic
  "Converts MOVE to algebraic notation to better communicate with others."
  [move]
  (let* [to-part (index->algebraic (:to move))
         from-part (index->algebraic (:from move))]
        (str to-part from-part)))

(defn- algebraic->move
  "Converts ALGEBRAIC notation to move."
  [algebraic]
  (let [from (algebraic->index (str (get algebraic 0)
                                    (get algebraic 1)))
        to (algebraic->index (str (get algebraic 2)
                                  (get algebraic 3)))]
    (Move. from to)))

(defn- occured-move
  "Given PREV-STATE and NEXT-STATE, calculate which move occurred to
  cause state change."
  [prev-state next-state])

(defn get-move
  "Let AI to seek its next move from STATE."
  [state]
  (let [depth @*search-depth*]
    (map #(cons (minimax-search % depth evaluate-state) %)
         (available-states-from state))))

