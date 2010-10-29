(ns tursas.state0x88
  (:use [clojure.contrib.math :only [abs]]
        (tursas move state)))

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

(def QUEEN-SIDE 0)
(def KING-SIDE 1)

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

;; sliding pieces
(def rook-directions (list NORTH SOUTH EAST WEST))
(def bishop-directions (list NW SW NE SE))
(def queen-directions (concat rook-directions bishop-directions))

;; moving pieces
(def king-movement queen-directions)
(def black-pawn-movement (list SE SW SOUTH))
(def white-pawn-movement (list NE NW NORTH))
(def knight-movement (list -33 -31 -18 -14 14 18 31 33))

;; base record type
(defrecord StateWith0x88 [board
                          turn
                          castling
                          en-passant
                          half-moves
                          full-moves
                          prev-move
                          score])

;; Predicates
(defn- board-index?
  "Does the given INDEX represent a square on the board?"
  [^Byte index]
  (zero? (bit-and index 0x88)))

(defn- empty-square?
  "Checks if given INDEX on BOARD is empty."
  [board index]
  (= (get board index) EMPTY))

(defn- column
  "Get the board column of the given square INDEX."
  [index]
  (bit-and index 7))

(defn- row
  "Get the board row of the given square INDEX."
  [index]
  (bit-shift-right index 4))

(defn- same-column?
  "Determines if both given square indexes X and Y are on the same column."
  [x y]
  (= (column x) (column y)))

(defn- same-row?
  "Determines if both given square indexes X and Y are on the same row."
  [x y]
  (= (row x) (row y)))

(defn- clear-en-passant
  "Makes a new state without an en passant move from given STATE."
  [state]
  (assoc state :en-passant "-"))

(defn- occupied?
  "Checks if BOARD INDEX is occupied by piece."
  [board index]
  (and (board-index? index)
       (not (empty-square? board index))))

(defn- occupied-by?
  "Checks if given BOARD INDEX is occupied by PLAYER."
  [board index player]
  (and (occupied? board index)
       (= (mod (get board index) 2) player)))

(defn- init-game-board
  "Generates new 128 element vector of bytes
   and places chess piece representation to it."
  []
  (into (vector-of :byte)
        (vec (replicate 128 -1))))

(defn- clear-square
  "Clears the given square INDEX on the game BOARD."
  [board index]
  (assoc board index EMPTY))

(defn- fill-square
  "Return new board with given PIECE-VALUE added to given BOARD's INDEX."
  [board index piece-value]
  (assoc board index piece-value))

(defn- index->algebraic
  "Converts given index to algebraic representation."
  [index]
  (let [coord (format "%x" index)
        num (+ (- (int (nth coord 0)) 48) 1)
        alpha (get "abcdefgh" (- (int (nth coord 1)) 48))]
    (str alpha num)))

(defn- algebraic->index
  "Converts given algebraic representation to board index value."
  [algebraic]
  (let [file (- (int (nth algebraic 0)) 97)
        rank (- (int (nth algebraic 1)) 48)]
    (+ (* (- 8 rank) 16) file)))

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

(defn- commit-castle-move
  "Make castling move on board."
  [player board move castling-side]
  (let [[rook king from to]
        (if (= player :white)
          [WHITE-ROOK WHITE-KING [0 7] [3 5]]
          [BLACK-ROOK BLACK-KING [112 119] [115 117]])]
    (-> board
        (clear-square (:from move))
        (clear-square (get from castling-side))
        (fill-square (:to move) king)
        (fill-square (get to castling-side) rook))))

(defn- slide-in-direction
  "Returns a set of possible moves by sliding piece
   from INDEX to DIRECTION in given STATE."
  [player board index direction]
  (loop [target-index (+ index direction)
         moves ()]
    (if (or (not (board-index? target-index))
            (occupied-by? board target-index player))
      moves
      (if (not (occupied? board (get board target-index)))
        (recur (+ target-index direction)
               (cons  (Move. index target-index nil) moves))
        (cons (Move. index target-index nil) moves)))))

(defn- move-to-place
  "Return set with index of possible move to given PLACE in given STATE."
  [board index place player]
  (if (or (occupied-by? board place (opponent player))
          (not (occupied? board place)))
    (list (Move. index place nil))
    '()))

(defn- ray-to-pieces?
  "Checks if there's ray to from INDEX to given PIECES."
  [board index inc pieces]
  (cond (not (board-index? index)) false
        (not (occupied? board index) (recur board (+ index inc) inc pieces))
        :else (nil? (some #{(get board index)} pieces))))

(defn- threaten-index?
  "Checks if given INDEX in STATE is under threath of enemy."
  [board index opponent]
  (or
   ;; check if opponent's knight can attack index
   (nil? (some #(= (get board %)
                   (if (= opponent :white)
                     WHITE-KNIGHT BLACK-KING))
               (map #(+ index %) knight-movement)))
   ;; check if there's ray to opponents queen or
   ;; bishop diagonally from index
   (not (nil? (some true? (map #(ray-to-pieces? board index %
                                                (if (= opponent :white)
                                                  [WHITE-QUEEN WHITE-BISHOP]
                                                  [BLACK-QUEEN BLACK-BISHOP]))
                               [NE NW SW SE]))))

   ;; check if there's ray to opponents queen or
   ;; rook on the same column or row
   (not (nil? (some true? (map #(ray-to-pieces? board index %
                                                (if (= opponent :white)
                                                  [WHITE-QUEEN WHITE-ROOK]
                                                  [BLACK-QUEEN BLACK-ROOK]))
                               [NORTH EAST WEST SOUTH]))))

   ;; check pawns
   (if (= opponent :white)
     (or (= (get board (+ index SE)) WHITE-PAWN)
         (= (get board (+ index SW)) WHITE-PAWN))
     (or (= (get board (+ index NE)) BLACK-PAWN)
         (= (get board (+ index NW)) BLACK-PAWN)))


   ;; check kings if there's king next to index and
   ;; it can attack index
   (let [player (if (= opponent :white) :black :white)
         own-king (if (= player :white) WHITE-KING BLACK-KING)
         enemy-king-index (filter #(= (get board  %) own-king)
                                  (map #(+ index %) king-movement))]
     (if (empty? enemy-king-index)
       false
       (-> board
           (update-board (Move. enemy-king-index index nil) player)
           (threaten-index? index player))))))

(defn- king-index
  "Gets the kings index in STATE for SIDE."
  [board player]
  (let [king (if (= player :black)
               BLACK-KING
               WHITE-KING)]
    (first (filter #(= (get board %) king) (range 128)))))

(defn- game-end?
  "Predicate to check if given STATE would indicate the game has ended.
   Mostly it checks if king has been captured, 50 move rule has come in effect
   or if player has no moves left to make."
  [state]
  (or (>= (:half-moves state) 50)
      (nil? (king-index (:board state) (:turn state)))
      (empty? (legal-states state))))

(defn- legal-castling?
  "Predicate to check if castling is possible on the board."
  [player board index increment]
  (loop [index (+ index increment)
         king-squares 2]
    (cond (> king-squares 0)
          (if (or (occupied? board index)
                  (threaten-index? board index player))
            false
            (recur (+ index increment) (dec king-squares)))
          :else (if (occupied? index)
                  false
                  (or (= (get board (+ index increment)) WHITE-ROOK)
                      (= (get board (+ index increment)) BLACK-ROOK))))))

(defn- castle-side?
  "Predicate to check if given piece can do castling."
  [player side castling]
  (let [piece (if (= player :white)
                (if (= side QUEEN-SIDE)
                  \Q \K)
                (if (= side QUEEN-SIDE)
                  \q \k))]
    (not (nil (some #{piece} castling)))))

(defn- list-king-moves
  "Resolves all available moves for king in given INDEX of STATE."
  [player board index castling]
  (let [normal-moves (flatten (map #(move-to-place board index (+ index %) player)
                                   king-movement))

        castling-moves-king (if (and (castle-side? player KING-SIDE castling)
                                     (legal-castling? player board index EAST))
                              (Move. index (* WEST 2) nil)
                              '())

        castling-moves-queen (if (and (castle-side? player QUEEN-SIDE castling)
                                      (legal-castling? player board index WEST))
                               (Move. index (* EAST 2) nil)
                               '())]
    (concat normal-moves castling-moves-king castling-moves-queen)))

(defn- list-pawn-moves
  "Returns a set of available pawn moves from INDEX in given STATE."
  [player board index en-passant]
  (let [step (if (= side :black) SOUTH NORTH)
        move-index (+ index step)
        move-twice? (or (and (= side :black) (same-row? index 96))
                        (and (= side :white) (same-row? index 16)))

        ;; calculate normal movement
        moves (if (not (occupied? board move-index))
                (if (and move-twice?
                         (not (occupied? board (+ move-index step))))
                  (list (Move. index move-index nil)
                        (Move. index (+ move-index step) nil))
                  (list (Move. index move-index nil)))
                '())

        ;; possible capture
        captures (if (= side :white)
                   (list (+ NW index) (+ NE index))
                   (list (+ SW index) (+ SE index)))
        en-passant-index (if (= en-passant "-")
                           -1
                           (algebraic->index en-passant))]

    ;; check capture points
    (flatten (conj moves
                   (map #(if (or (and (board-index? %)
                                      (= en-passant-index %))
                                 (and (occupied? board %)
                                      (not (occupied-by? board % player))))
                           (list (Move. index % nil))
                           '())
                        captures)))))

(defn- list-moves-for-piece
  "Generates a set of all available moves for piece at INDEX in given STATE."
  [state index]
  (let [board (:board state)
        player (if (occupied-by? board index :white) :white :black)]
    (case (Character/toLowerCase (piece-value->char (get board index)))
          \r (map #(slide-in-direction player board index %) rook-directions)
          \b (map #(slide-in-direction player board index %) bishop-directions)
          \q (map #(slide-in-direction player board index %) queen-directions)
          \n (map #(move-to-place board index (+ index %) player) knight-movement)
          \p (list-pawn-moves player board index (:en-passant state))
          \k (list-king-moves board index (:castling state))
        '())))

(defn- all-piece-indexes-for
  "Gets a list of all board indexes containing
   SIDE's pieces in given STATE."
  [board player]
  (filter #(occupied-by? board % player) (range 128)))

(defn- all-moves-for
  "Returns a set of all available moves for SIDE in STATE."
  [state side]
  (filter #(in-check? (apply-move state %))
          (flatten (map #(list-moves-for-piece state %)
                        (all-piece-indexes-for (:board state) side)))))

(defn- fen-board->0x88board
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

(defn- board->fen-board
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
            (recur (dec index) fen 0 true)
            add-slash
            (recur index (str fen "/") 0 false)
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
            (recur (dec index) fen (inc empty) false)
            :else
            (recur (dec index)
                   (str fen
                        (if (> empty 0) empty "")
                        (piece-value->char (get board index)))
                   0
                   false)))))

(defn promotion?
  "Checks if given move is pawn promotion."
  [piece move]
  (or (and (= piece WHITE-PAWN)
           (= (row (:to move)) 7))
      (and (= piece BLACK-PAWN)
           (= (row (:to move)) 0))))

(defn castling?
  "Checks given move is castling move."
  [piece move]
  (and (or (= piece WHITE-KING)
           (= piece BLACK-KING))
       (= (abs (- (:to move) (:from move))) 2)))

(defn- update-board
  "Returns new board after applying MOVE to BOARD."
  [board move player move]
  (let [to-index (:to move)
        from-index (:from move)
        moving-piece (get board from-index)]
    (cond (promotion? moving-piece move)
          (fill-square (clear-square board from-index)
                       to-index
                       (piece-char->value
                        (if (= player :white)
                          (Character/toUpperCase (:promotion move))
                          (Character/toLowerCase (:promotion move)))))
          (castling? moving-piece move)
          (commit-castle-move player board move
                              (if (= column to-index 2)
                                QUEEN-SIDE
                                KING-SIDE))
          :else
          (-> (clear-square board from-index)
              (fill-square to-index moving-piece)))))

(defn- pawn-or-capture-move?
  "Predicate to see if move was pawn move or a capture"
  [piece board move]
  (or (= piece WHITE-PAWN)
      (= piece BLACK-PAWN)
      (not (= (get board (:to move)) EMPTY))))

(defn- update-castling
  "Return new castling string for move"
  [current-castling player move]
  (if (= (current-castling) "-")
    "-"
    (if (= player :white)
      (reduce str (re-seq #"\p{Upper}" (current-castling)))
      (reduce str (re-seq #"\p{Lower}" (current-castling))))))

(defn- update-en-passant
  "Construct en-passant string from PIECE and MOVE."
  [piece move]
  (if (and (or (= piece WHITE-PAWN)
               (= piece BLACK-PAWN))
           (= (abs (- (:to move) (:from move))) 0x20))
    (index->algebraic (/ (+ (:to move) (:from move)) 2))
    "-"))

(defn- update-state
  "Return result of applying given MOVE to STATE."
  [state move]
  (let [to-index (:to move)
        from-index (:from move)
        player (if (occupied-by? board from-index :white)
                 :white :black)

        moving-piece (get (:board state) from-index)

        half-moves (if (pawn-or-capture-move? moving-piece (:board state) move)
                     0
                     (inc (:half-moves state)))

         full-moves (if (= player :black)
                      (inc (:full-moves state))
                      (:full-moves state))]

        (StateWith0x88. (update-board (:board state) move player)
                        (if (= player :white) "b" "w")
                        (update-castling (:castling state) player move)
                        (update-en-passant moving-piece move)
                        half-moves
                        full-moves
                        (move->algebraic move))))

(defn- parse-fen
  "Parse FEN string and buld a state record."
  [fen]
  (let [fen-list (re-seq #"\S+" fen)]
    (when (= (count fen-list) 6)
      (StateWith0x88. (fen-board->0x88board (first fen-list))
                      (if (= (second fen-list) "w") :white :black)
                      (nth fen-list 2)
                      (nth fen-list 3)
                      (Integer/parseInt (nth fen-list 4))
                      (Integer/parseInt (nth fen-list 5))
                      nil))))

(defn- parse-state
  "Returns FEN representation of given STATE."
  [state]
  (str (board->fen-board (:board state)) " "
       (if (= (:turn state) :white) "w" "b") " "
       (:castling state) " "
       (:en-passant state) " "
       (:half-moves state) " "
       (:full-moves state)))

;;;;; TYPE EXTENSION ;;;;;;
(extend-type StateWithHex
  State
  (occupied? [state index]
             (occupied? (:board state) index))
  (black? [state index]
          (occupied-by? (:board state) index :black))
  (white? [state index]
          (occupied-by? (:board state) index :white))
  (opponent [state]
            (if (= (:turn state) :white) :black :white))
  (apply-move [state move]
               (update-state state move))
  (in-check? [state]
             (threaten-index? (:board state)
                              (king-index (:board state) (:turn state))
                              (:turn state)))
  (fen->state [fen]
              (parse-fen fen))
  (state->fen [state]
              (parse-state state))
  (legal-states [state]
                (map #(apply-move state %)
                     (all-moves-for state (:turn state)))))

