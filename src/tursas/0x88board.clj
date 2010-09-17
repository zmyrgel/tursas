(ns tursas.0x88board
  (:use [tursas.repl])
  (:require clojure.set :as set))

;; row identifiers
(def ROW-1 (bit-shift-righf *A1* 4))
(def ROW-2 (bit-shift-righf *A2* 4))
(def ROW-3 (bit-shift-righf *A3* 4))
(def ROW-4 (bit-shift-righf *A4* 4))
(def ROW-5 (bit-shift-righf *A5* 4))
(def ROW-6 (bit-shift-righf *A6* 4))
(def ROW-7 (bit-shift-righf *A7* 4))
(def ROW-8 (bit-shift-righf *A8* 4))

;; column identifiers
(def COL-A (bit-and *A1* 7))
(def COL-B (bit-and *B1* 7))
(def COL-C (bit-and *C1* 7))
(def COL-D (bit-and *D1* 7))
(def COL-E (bit-and *E1* 7))
(def COL-F (bit-and *F1* 7))
(def COL-G (bit-and *G1* 7))
(def COL-H (bit-and *H1* 7))

;; direction vectors
(def NORTH 16)
(def NN (+ *NORTH* *NORTH*))
(def SOUTH -16)
(def SS (+ *SOUTH* *SOUTH*))
(def EAST 1)
(def WEST -1)
(def NE 17)
(def SW -17)
(def NW 15)
(def SE -15)

;; sliding pieces
(def rook-directions #{NORTH SOUTH EAST WEST})
(def bishop-directions #{NW,SW,NE,SE})
(def queen-directions (set/union rook-directions bishop-directions))

;; moving pieces
(def king-movement queen-directions)
(def black-pawn-movement #{SE,SW,SOUTH})
(def white-pawn-movement #{NE,NW,NORTH})
(def knight-movement #{-33, -31, -18, -14, 14, 18, 31, 33})

(defn board-square?
  "Does the given INDEX represent a square on the board?"
  [^Byte index]
  (if (zero? (bit-and index 0x88))
    true
    false))

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
  (if (= (column x) (column y))
    true
    false))

(defn same-row?
  "Determines if both given square indexes X and Y are on the same row."
  [x y]
  (if (= (row x) (row y))
    true
    false))

(defn piece-value->char
  "Gives piece character representation from its board VALUE."
  [value]
  (case value
        -1 \p
        -2 \r
        -3 \n
        -4 \b
        -5 \q
        -6 \k
        1 \P
        2 \N
        3 \B
        4 \R
        5 \Q
        6 \K
        nil))

(defn piece-char->value
  "Gives pieces character numerical representation from its CHAR."
  [char]
  (case char
        \p -1
        \r -2
        \n -3
        \b -4
        \q -5
        \k -6
        \P 1
        \N 2
        \B 3
        \R 4
        \Q 5
        \K 6
        nil))

(defn piece-material-value
  "Gives material value for PIECE."
  [piece]
  (case piece
        \p -1
        \n -3
        \b -3
        \r -5
        \q -9
        \k -999
        \P 1
        \N 3
        \B 3
        \R 5
        \Q 9
        \K 999
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
  [board index]
  false)

(defn black?
  "Checks if piece in given INDEX is black."
  [board index]
  (if (< (nth board index) 0)
    true
    false))

(defn white?
  "Checks if piece in given INDEX is black."
  [board index]
  (if (< (nth board index) 0)
    true
    false))

(defn all-pieces-for
  "Returns a list of all indexes containing pieces of given SIDE."
  [board side]
  (if (= side "black")
    (filter #(black? board %) (range 128))
    (filter #(white? board %) (range 128))))

(defn slide-in-direction
  "Returns list of possible moves by sliding from BOARD INDEX to DIRECTION."
  [board index direction]
  (let [friendly? (if (black? board index) black? white?)]
    (loop [target-index (+ index direction)
           moves ()]
      (if (or (not (board-square? target-index))
              (friendly? board target-index))
        moves
        (if (zero? (nth board target-index))
          (recur (+ target-index direction)
                 (cons target-index moves))
          (cons target-index moves))))))

(defn move-in-place
  "Moves piece on BOARD INDEX to given PLACE if possible."
  [board index place]
  (let [friendly? (if (black? board index) black? white?)]
    (if (or (not (board-square? place))
            (friendly? board place))
      ()
      place)))

(defn list-pawn-moves
  "List available pawn moves from BOARD INDEX."
  [board index]
  ;; XXX: add check for promotion
  (let* [moves ()
         side (if (black? board index) "b" "w")
         friendly? (if (= side "b") black? white?)
         ;; possible capture
         potential-captures (if (= side "b")
                              #{(+ SW index) (+ SE index)}
                              #{(+ NW index) (+ NE index)})
         ;; normal movement
         potential-move (+ index (if (= side "b")
                                   SOUTH
                                   NORTH))
         ;; check if can move two squares
         move-twice? (or (and (= side "b")
                              (same-row? index 96))
                         (and (= side "w")
                              (same-row? index 16)))]

        (when (and (board-square? potential-move)
                   (zero? (nth board potential-move)))
            (cons potential-move moves))
        (when (and move-twice?
                 (board-square? (+ potential-move potential-move))
                 (zero? (nth board (+ potential-move potential-move))))
          (cons (+ potential-move potential-move) moves))
        (map #(when (and (board-square? %)
                         (not (friendly? board %))) (cons % moves))
             potential-captures)
        moves))

(defn list-moves-for-piece
  "Generates all available moves for rook in INDEX position."
  [board index piece]
      (when (not (move-causes-check? board index))
        (flatten
         (case piece
               \r (map #(slide-in-direction board index %) rook-directions)
               \b (map #(slide-in-direction board index %) bishop-directions)
               \q (map #(slide-in-direction board index %) queen-directions)
               \k (map #(move-in-place board index %) king-movement) ;; check if king is under threat
               \n (map #(move-in-place board index %) knight-movement)
               \p (list-pawn-moves board index)
               nil))))

;; 39 -> h6
(defn index->algebraic
  "Converts given index to algebraic representation."
  [index])

(defn algebraic->index
  "Converts given algebraic representation to board index value."
  [algebraic]
  (let [file (- (int (nth algebraic 0)) 97)
        rank (Character/digit (nth algebraic 1) 10)]
    (+ (* (- 8 rank) 16) file)))

(defn occupied?
  "Checks if given index is occupied on the chessboard."
  [chessboard index]
  (if (= (get chessboard index) 0)
    false
    true))

(defn clear-en-passant
  "Clears the en passant possibility from BOARD."
  [board])

(defn initialize-chessboard
  "Generates new 128 element vector of bytes
   and places chess piece representation to it.
   9's on board represent castling possibilities.
   Half moves since pawn movement or capture are stored
   in index 4A.
   En Passant moves are marked in the outside board to
   indexes in range 0x28 to 0x2F and 0x58 to 0x5F."
  []
  (into (vector-of :byte)
        (vector -2 -3 -4 -5 -6 -4 -3 -2 9 0 0 0 0 0 0 9
                -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0
                2 3 4 5 6 4 3 2 9 0 0 0 0 0 0 9)))
