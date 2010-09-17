(ns tursas.0x88board
  (:use [clojure.set :only [union]]))

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
(def ROOK-VALUE 3)
(def KNIGHT-VALUE 3)
(def ROOK-VALUE 5)
(def QUEEN-VALUE 9)
(def KING-VALUE 999)

;; sliding pieces
(def rook-directions #{NORTH SOUTH EAST WEST})
(def bishop-directions #{NW,SW,NE,SE})
(def queen-directions (union rook-directions bishop-directions))

;; moving pieces
(def king-movement queen-directions)
(def black-pawn-movement #{SE,SW,SOUTH})
(def white-pawn-movement #{NE,NW,NORTH})
(def knight-movement #{-33, -31, -18, -14, 14, 18, 31, 33})

(defn board-square?
  "Does the given INDEX represent a square on the board?"
  [^Byte index]
  (zero? (bit-and index 0x88)))

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

(defn piece-value->char
  "Gives piece character representation from its board VALUE."
  [value]
  (nth "pPRrNnBbQqKk" value))

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

(defn intersect-rank-diag
  " return: intersection square index (if any)
            of rank of square index A
            with diagonal of square index B
            -1 if no intersection exists"
  [a b]
  (let [s88 (+ (- (* (bit-shift-right a 3) 17)
                  (bit-shift-right b 3))
               (bit-and b 7))]
    (bit-or (+ (bit-and (bit-shift-right s88 1) 56) (bit-and s88 7))
                  (- (bit-shift-right (bit-and s88 0x88) 31)))))

(defn intersect-file-diag
  " return: intersection square index (if any)
            of file of square index A
            with diagonal of square index B
            -1 if no intersection exists"
  [a b]
  (let [s88 (- (+ (* (bit-shift-right a 7) 17)
                  (* (bit-and b 56) 2))
               (* (bit-and b 7) 16))]
    (bit-or (+ (bit-and (bit-shift-right s88 1) 56)
               (bit-and s88 7))
            (- (bit-shift-right (bit-and s88 0x88) 31)))))

(defn intersect-rank-anti
  " return: intersection square index (if any)
            of rank of square index A
            with anti-diagonal of square index B
            -1 if no intersection exists"
  [a b]
  (let [s88 (+ (- (* (bit-shift-right a 3) 17)
                  (bit-shift-right b 3))
               (bit-and b 7))]
    (bit-or (+ (bit-and (bit-shift-right s88 1) 56)
               (bit-and s88 7))
            (bit-shift-right (- (bit-and s88 0x88)) 31))))

(defn intersect-file-anti
  " return: intersection square index (if any)
            of file of square index A
            with anti-diagonal of square index B
            -1 if no intersection exists"
  [a b]
  (let [s88 (- (+ (* (bit-and b 56) 2)
                  (* (bit-and b 7) 16))
               (* (bit-and a 7) 15))]
    (bit-or (+ (bit-and (bit-shift-right s88 1) 56)
               (bit-and s88 7))
            (bit-and (- (bit-and s88 0x88)) 31))))

(defn move-causes-check?
  "Checks if moving the piece causes king to be threatened."
  [state index]
  false)

(defn black?
  "Checks if piece in given INDEX is black."
  [state index]
  (< (nth state index) 0))

(defn white?
  "Checks if piece in given INDEX is black."
  [state index]
  (> (nth state index) 0))

(defn slide-in-direction
  "Returns list of possible moves by sliding piece
   from INDEX to DIRECTION in given STATE."
  [state index direction]
  (let [friendly? (if (black? state index) black? white?)]
    (loop [target-index (+ index direction)
           moves ()]
      (if (or (not (board-square? target-index))
              (friendly? state target-index))
        moves
        (if (zero? (nth state target-index))
          (recur (+ target-index direction)
                 (cons target-index moves))
          (cons target-index moves))))))

(defn move-to-place
  "Return index of possible move to given PLACE in given STATE."
  [state index place]
  (let [friendly? (if (black? state index) black? white?)]
    (when (and (board-square? place)
               (not (friendly? state place)))
      place)))

(defn list-pawn-moves
  "List available pawn moves from INDEX in given STATE."
  [state index]
  ;; XXX: add check for promotion
  (let* [moves ()
         side (if (black? state index) BLACK WHITE)
         friendly? (if (= side BLACK) black? white?)
         ;; possible capture
         potential-captures (if (= side BLACK)
                              #{(+ SW index) (+ SE index)}
                              #{(+ NW index) (+ NE index)})
         ;; normal movement
         potential-move (+ index (if (= side BLACK)
                                   SOUTH
                                   NORTH))
         ;; check if can move two squares
         move-twice? (or (and (= side BLACK)
                              (same-row? index 96))
                         (and (= side WHITE)
                              (same-row? index 16)))]
        (when (and (board-square? potential-move)
                   (zero? (nth state potential-move)))
            (cons potential-move moves))
        (when (and move-twice?
                 (board-square? (+ potential-move potential-move))
                 (zero? (nth state (+ potential-move potential-move))))
          (cons (+ potential-move potential-move) moves))
        (map #(when (and (board-square? %)
                         (not (friendly? state %))) (cons % moves))
             potential-captures)
        moves))

(defn list-moves-for-piece
  "Generates all available moves for piece at INDEX in given STATE."
  [state index]
      (when (not (move-causes-check? state index))
        (flatten
         (case (piece-value->char (nth state index))
               \r (map #(slide-in-direction state index %) rook-directions)
               \b (map #(slide-in-direction state index %) bishop-directions)
               \q (map #(slide-in-direction state index %) queen-directions)
               \k (map #(move-to-place state index (+ index %)) king-movement) ;; check if king is under threat
               \n (map #(move-to-place state index (+ index %)) knight-movement)
               \p (list-pawn-moves state index)
               nil))))

(defn all-piece-indexes-for
  "Gets a list of all board indexes containing
   SIDE's pieces in given STATE."
  [state side]
  (if (= side BLACK)
    (filter #(black? state %) (range 128))
    (filter #(white? state %) (range 128))))

(defn all-moves-for
  "Returns all available moves for SIDE"
  [state side]
    (if (= side BLACK)
    (map #(list-moves-for-piece state %) (all-piece-indexes-for state BLACK))
    (map #(list-moves-for-piece state %) (all-piece-indexes-for state WHITE))))

;; 39 -> 0x27 -> h3
(defn index->algebraic
  "Converts given index to algebraic representation."
  [index]

  )

(defn algebraic->index
  "Converts given algebraic representation to board index value."
  [algebraic]
  (let [file (- (int (nth algebraic 0)) 97)
        rank (- (int (nth algebraic 1)) 48)]
    (+ (* (- 8 rank) 16) file)))

(defn occupied?
  "Checks if given INDEX is occupied on given STATE."
  [state index]
  (not (= (get state index) 0)))

(defn clear-en-passant
  "Clears the en passant possibility from STATE."
  [state])

(defn init-game-board
  "Generates new 128 element vector of bytes
   and places chess piece representation to it."
  []
  (into (vector-of :byte)
        (vec (replicate 128 -1))))

(defn clear-square
  "Clears the given square INDEX on the game state."
  [board index]
  (assoc board index -1))

(defn fill-square
  "Return new STATE with given PIECE of SIDE added to given STATE's INDEX."
  [board index piece-value]
  (assoc board index piece-value))

(defn parse-fen-board
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

(defrecord StateWith0x88 [board turn castling en-passant half-moves full-moves])
(def default-startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defn fen->0x88
  "Converts FEN string to 0x88 board representation."
  [fen]
  (let [fen-list (re-seq #"\S+" fen)]
    (when (= (count fen-list) 6)
      (StateWith0x88. (parse-fen-board (first fen-list))         ;; board
                      (if (= (second fen-list) "w") WHITE BLACK) ;; turn
                      (nth fen-list 2)                           ;; castling
                      (nth fen-list 3)                           ;; en-passant
                      (Integer/parseInt (nth fen-list 4))        ;; half turns
                      (Integer/parseInt (nth fen-list 5))))))    ;; full turns



