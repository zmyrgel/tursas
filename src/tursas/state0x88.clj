(ns tursas.state0x88
  (:use (tursas state move)
        [clojure.contrib.math :only [abs]])
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq]))

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
(defrecord State0x88 [board
                      turn
                      castling
                      en-passant
                      half-moves
                      full-moves
                      prev-move
                      score]
  Comparable
  (compareTo [this other]
             (compare score (:score other))))

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

(defn- white-piece?
  "Predicate to check if given piece value belongs to white."
  [piece]
  (zero? (mod piece 2)))

(defn- black-piece?
  "Checks if given PIECE value belongs to black."
  [piece]
  (not (white-piece? piece)))

(defn- board-occupied?
  "Checks if BOARD INDEX is occupied by piece."
  [board index]
  (and (board-index? index)
       (not (empty-square? board index))))

(defn- occupied-by?
  "Checks if given BOARD INDEX is occupied by PLAYER."
  [board index player]
  (and (board-occupied? board index)
       (if (= player :white)
         (white-piece? (get board index))
         (black-piece? (get board index)))))

(defn- opponent
  "Return opponent of given player"
  [player]
  (if (= player :white)
    :black :white))

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
  (let [coord (format "%02x" index)
        num (+ (- (int (nth coord 0)) 48) 1)
        alpha (get "abcdefgh" (- (int (nth coord 1)) 48))]
    (str alpha num)))

(defn- algebraic->index
  "Converts given algebraic representation to board index value."
  [algebraic]
  (let [file (- (int (nth algebraic 0)) 97)
        rank (- (int (nth algebraic 1)) 48)]
    (+ (* (dec rank) 16) file)))

(defn piece-value->char
  "Gives piece character representation from its board VALUE."
  [value]
  (if (= value EMPTY)
    \E
    (nth "PpRrNnBbQqKk" value)))

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
        (clear-square (algebraic->index (:from move)))
        (clear-square (get from castling-side))
        (fill-square (algebraic->index (:to move)) king)
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
      (if (not (board-occupied? board (get board target-index)))
        (recur (+ target-index direction)
               (cons  (make-move (index->algebraic index)
                                 (index->algebraic target-index)
                                 nil)
                      moves))
        (cons (make-move (index->algebraic index)
                         (index->algebraic target-index)
                         nil)
              moves)))))

(defn- move-to-place
  "Return set with index of possible move to given PLACE in given STATE."
  [board index place player]
  (if (and (board-index? place)
           (or (not (board-occupied? board place))
               (occupied-by? board place (opponent player))))
    (list (make-move (index->algebraic index)
                     (index->algebraic place)
                     nil))
    '()))

(defn- ray-to-pieces?
  "Checks if there's ray to from INDEX to given PIECES."
  [board index inc pieces]
  (cond (not (board-index? index)) false
        (not (board-occupied? board index)) (recur board (+ index inc) inc pieces)
        :else (nil? (some #{(get board index)} pieces))))

(declare update-board)
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
           (update-board (make-move (index->algebraic enemy-king-index)
                                    (index->algebraic index)
                                    nil)
                         player)
           (threaten-index? index player))))))

(defn- king-index
  "Gets the kings index in STATE for SIDE."
  [board player]
  (let [king (if (= player :black)
               BLACK-KING
               WHITE-KING)]
    (first (filter #(= (get board %) king) (range 128)))))

(defn- legal-castling?
  "Predicate to check if castling is possible on the board."
  [player board index increment]
  (loop [index (+ index increment)
         king-squares 2]
    (cond (> king-squares 0)
          (if (or (board-occupied? board index)
                  (threaten-index? board index player))
            false
            (recur (+ index increment) (dec king-squares)))
          :else (if (board-occupied? index)
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
    (not (nil? (some #{piece} castling)))))

(defn- list-king-moves
  "Resolves all available moves for king in given INDEX of STATE."
  [player board index castling]
  (let [normal-moves (flatten (map #(move-to-place board index (+ index %) player)
                                   king-movement))

        castling-moves-king (if (and (castle-side? player KING-SIDE castling)
                                     (legal-castling? player board index EAST))
                              (make-move (index->algebraic index)
                                         (index->algebraic (* WEST 2))
                                         nil)
                              '())

        castling-moves-queen (if (and (castle-side? player QUEEN-SIDE castling)
                                      (legal-castling? player board index WEST))
                               (make-move (index->algebraic index)
                                          (index->algebraic (* EAST 2))
                                          nil)
                               '())]
    (concat normal-moves castling-moves-king castling-moves-queen)))

(defn- list-pawn-moves
  "Returns a set of available pawn moves from INDEX in given STATE."
  [player board index en-passant]
  (let [step (if (= player :black) SOUTH NORTH)
        move-index (+ index step)
        move-twice? (or (and (= player :black) (same-row? index 96))
                        (and (= player :white) (same-row? index 16)))

        ;; calculate normal movement
        moves (if (not (board-occupied? board move-index))
                (if (and move-twice?
                         (not (board-occupied? board (+ move-index step))))
                  (list (make-move (index->algebraic index)
                                   (index->algebraic move-index)
                                   nil)
                        (make-move (index->algebraic index)
                                   (index->algebraic (+ move-index step))
                                   nil))
                  (list (make-move (index->algebraic index)
                                   (index->algebraic move-index)
                                   nil)))
                '())

        ;; possible capture
        captures (if (= player :white)
                   (list (+ NW index) (+ NE index))
                   (list (+ SW index) (+ SE index)))
        en-passant-index (if (= en-passant "-")
                           -1
                           (algebraic->index en-passant))]

    ;; check capture points
    (flatten (conj moves
                   (map #(if (or (and (board-index? %)
                                      (= en-passant-index %))
                                 (and (board-occupied? board %)
                                      (not (occupied-by? board % player))))
                           (list (make-move (index->algebraic index)
                                            (index->algebraic %)
                                            nil))
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
          \k (list-king-moves player board index (:castling state))
          '())))

(defn- all-piece-indexes-for
  "Gets a list of all board indexes containing
   SIDE's pieces in given STATE."
  [board player]
  (filter #(occupied-by? board % player) (range 128)))

(defn- all-moves-for
  "Returns a set of all available moves for SIDE in STATE."
  [state side]
  (flatten (map (partial list-moves-for-piece state)
                (all-piece-indexes-for (:board state) side))))

(defn- fen-board->0x88board
  [fen-board]
  (reduce (fn [board [index piece]]
            (fill-square board index (piece-char->value piece)))
          (init-game-board)
          (seq/indexed (s/map-str #(str % "EEEEEEEE")
                                  (->> fen-board
                                       (s/replace-by #"\d" #(str (s/repeat (Integer/parseInt %) \E)))
                                       (s/split #"/+")
                                       reverse)))))

(defn- make-fen-row
  "Builds single fen row from given BOARD and ROW index."
  [board row]
  (s/map-str #(if (= (get % 0) \E) (count %) %)
             (s/partition #"E+"
                          (s/map-str #(piece-value->char (get board (+ row %)))
                                     (range 8)))))

(defn- board->fen-board
  "Convert the given state's BOARD to fen board field."
  [board]
  (s/join "/" (map #(make-fen-row board %)
                   [0x70 0x60 0x50 0x40 0x30 0x20 0x10 0x0])))

(defn promotion?
  "Checks if given move is pawn promotion."
  [piece move]
  (or (and (= piece WHITE-PAWN)
           (= (row (algebraic->index (:to move))) 7))
      (and (= piece BLACK-PAWN)
           (= (row (algebraic->index (:to move))) 0))))

(defn castling?
  "Checks given move is castling move."
  [piece move]
  (and (or (= piece WHITE-KING)
           (= piece BLACK-KING))
       (= (abs (- (algebraic->index (:to move))
                  (algebraic->index (:from move)))) 2)))

(defn- get-promotion-piece
  "Helper function to return new piece char.
   Gets the char from move or if nil, defaults to queen."
  [player move]
  (if (nil? (:promotion move))
    (if (= player :white) \Q \q)
    (if (= player :white)
      (Character/toUpperCase (:promotion move))
      (Character/toLowerCase (:promotion move)))))

(defn- update-board
  "Returns state with new board after applying MOVE to STATE."
  [move state]
  (let [board (:board state)
        player (:tun state)
        to-index (algebraic->index (:to move))
        from-index (algebraic->index (:from move))
        moving-piece (get board from-index)]
    (assoc state :board
           (cond (promotion? moving-piece move)
                 (-> board
                     (clear-square from-index)
                     (fill-square to-index (piece-char->value
                                            (get-promotion-piece player move))))
                 (castling? moving-piece move)
                 (commit-castle-move player board move
                                     (if (= column to-index 2)
                                       QUEEN-SIDE
                                       KING-SIDE))
                 :else (-> board
                           (clear-square from-index)
                           (fill-square to-index moving-piece))))))

(defn- pawn-or-capture-move?
  "Predicate to see if move was pawn move or a capture"
  [piece board move]
  (or (= piece WHITE-PAWN)
      (= piece BLACK-PAWN)
      (not (= (get board (algebraic->index (:to move))) EMPTY))))

(defn- update-castling
  "Return new castling string for move
   checks for king or rook moves."
  [move state]
  (assoc state :castling
         (if (= (:castling state) "-")
           "-"
           (let [cur-white (reduce str (re-seq #"\p{Upper}" (:castling state)))
                 cur-black (reduce str (re-seq #"\p{Lower}" (:castling state)))
                 add-castling #(if (empty? %) "-" %)]
             (if (= (:turn state) :white)
               (case (:from move)
                     "e1" (add-castling cur-black)
                     "a1" (add-castling (str (s/replace-char \Q "" cur-white) cur-black))
                     "h1" (add-castling (str (s/replace-char \K "" cur-white) cur-black))
                     (str cur-white cur-black))
               (case (:from move)
                     "e8" (add-castling cur-white)
                     "a8" (add-castling (str (s/replace-char \q "" cur-black) cur-white))
                     "h8" (add-castling (str (s/replace-char \k"" "" cur-black) cur-white))
                     (str cur-white cur-black)))))))

(defn- update-turn
  "Updates player turn of STATE"
  [state]
  (assoc state :turn
         (if (= (:turn state) :black)
           :white :black)))

(defn- update-en-passant
  "Associates new en-passant string with given STATE based on the MOVE."
  [move state]
  (let [from-index (algebraic->index (:from move))
        to-index (algebraic->index (:to move))
        piece (get (:board state) from-index)]
    (assoc state :en-passant (if (and (or (= piece WHITE-PAWN)
                                          (= piece BLACK-PAWN))
                                      (= (abs (- to-index from-index)) 0x20))
                               (index->algebraic (/ (+ to-index from-index) 2))
                               "-"))))

(defn- allowed-move?
  "Checks if given MOVE is allowed in STATE."
  [state move]
  (not (nil? (some #(and (= (:from move) (:from %))
                         (= (:to move) (:to %)))
                   (flatten (list-moves-for-piece state
                                                  (algebraic->index (:from move))))))))

(defn- update-half-moves
  "Increases STATE half moves count unless the move
   pawn move or a capture."
  [move state]
  (let [piece (get (:board state) (algebraic->index (:from move)))]
    (assoc state :half-moves
           (if (pawn-or-capture-move? piece (:board state) move)
             0
             (inc (:half-moves state))))))

(defn- update-full-moves
  "Updates full move count of STATE"
  [state]
  (assoc state :full-moves
         (if (= (:turn state) :black)
           (inc (:full-moves state))
           (:full-moves state))))

(defn- update-move
  "Update the previous move of state."
  [move state]
  (assoc state :prev-move (move->algebraic move)))

(defn- parse-state
  "Returns FEN representation of given STATE."
  [state]
  (str (board->fen-board (:board state)) " "
       (if (= (:turn state) :white) "w" "b") " "
       (:castling state) " "
       (:en-passant state) " "
       (:half-moves state) " "
       (:full-moves state)))

(defn- build-piece-map
  "Builds map with algebraic coordinate as keys
   and values are piece chars of given corresponding index."
  [state]
  (loop [coords '()
         pieces '()
         index 0]
    (if (= index 121)
      (zipmap coords pieces)
      (if (board-occupied? (:board state) index)
        (recur (cons (index->algebraic index) coords)
               (cons (piece-value->char (get (:board state) index)) pieces)
               (inc index))
        (recur coords pieces (inc index))))))

;;;;; TYPE EXTENSIONS ;;;;;;
(extend-type State0x88
  State
  (occupied? [state index]
             (board-occupied? (:board state) index))
  (black? [state index]
          (occupied-by? (:board state) index :black))
  (white? [state index]
          (occupied-by? (:board state) index :white))
  (apply-move [state move]
              (when (and (occupied-by? (:board state)
                                       (algebraic->index (:from move))
                                       (:turn state))
                         (not (game-end? state))
                         (allowed-move? state move))
                (->> state
                     (update-board move)
                     update-turn
                     (update-castling move)
                     (update-en-passant move)
                     (update-half-moves move)
                     update-full-moves
                     (update-move move))))
  (in-check? [state]
             (threaten-index? (:board state)
                              (king-index (:board state) (:turn state))
                              (opponent (:turn state))))
  (game-end? [state]
             (or (>= (:half-moves state) 50)
                 (nil? (king-index (:board state) (:turn state)))))
  (state->fen [state]
              (parse-state state))
  (legal-states [state]
                (map (partial apply-move state)
                     (all-moves-for state (:turn state))))
  (get-pieces [state]
              (build-piece-map state)))

(defprotocol Fen
  (fen->state [fen]))

(extend-type String
  Fen
  (fen->state [fen]
              (when-let [fen-list (re-seq #"\S+" fen)]
                (State0x88. (fen-board->0x88board (first fen-list))
                            (if (= (second fen-list) "w") :white :black)
                            (nth fen-list 2)
                            (nth fen-list 3)
                            (Integer/parseInt (nth fen-list 4))
                            (Integer/parseInt (nth fen-list 5))
                            nil
                            nil))))

