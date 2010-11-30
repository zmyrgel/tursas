(ns tursas.state0x88
  (:use (tursas state move hexmove)
        [clojure.contrib.math :only [abs]])
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq]))

(def NORTH 16)
(def NN 32)
(def SOUTH -16)
(def SS -32)
(def EAST 1)
(def WEST -1)
(def NE 17)
(def SW -17)
(def NW 15)
(def SE -15)

(def QUEEN-SIDE 0)
(def KING-SIDE 1)

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

(def rook-directions (list NORTH SOUTH EAST WEST))
(def bishop-directions (list NW SW NE SE))
(def queen-directions (concat rook-directions bishop-directions))

(def king-movement queen-directions)
(def black-pawn-movement (list SE SW SOUTH))
(def white-pawn-movement (list NE NW NORTH))
(def knight-movement (list -33 -31 -18 -14 14 18 31 33))

(defrecord State0x88 [board
                      turn
                      castling
                      en-passant
                      half-moves
                      full-moves
                      prev-move])

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

(defn- castle-side?
  "Predicate to check if given piece can do castling."
  [player side castling]
  (let [piece (if (= player :white)
                (if (= side QUEEN-SIDE) \Q \K)
                (if (= side QUEEN-SIDE) \q \k))]
    (not (nil? (some #{piece} castling)))))

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

(defn- clear-en-passant
  "Makes a new state without an en passant move from given STATE."
  [state]
  (assoc state :en-passant "-"))

(defn- clear-square
  "Clears the given square INDEX on the game BOARD."
  [board index]
  (assoc board index EMPTY))

(defn- fill-square
  "Return new board with given PIECE-VALUE added to given BOARD's INDEX."
  [board index piece-value]
  (assoc board index piece-value))

(defn piece-name
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
      (if (not (board-occupied? board (get board target-index)))
        (recur (+ target-index direction)
               (cons (make-move index target-index nil)
                     moves))
        (cons (make-move index target-index nil)
              moves)))))

(defn- move-to-place
  "Return set with index of possible move to given PLACE in given STATE."
  [board index place player]
  (if (and (board-index? place)
           (or (not (board-occupied? board place))
               (occupied-by? board place (opponent player))))
    (list (make-move index place nil))
    '()))

(defn- ray-to-pieces?
  "Checks if there's ray to from INDEX to given PIECES."
  [board index inc pieces]
  (let [new-index (+ index inc)]
    (cond (not (board-index? new-index)) false
          (not (board-occupied? board new-index)) (recur board new-index inc pieces)
          :else (not (nil? (some #(= (get board new-index) %) pieces))))))

(defn- king-index
  "Gets the kings index in STATE for SIDE."
  [board player]
  (let [king (if (= player :black) BLACK-KING WHITE-KING)]
    (first (filter #(= (get board %) king) (range 128)))))

(defn- threaten-by-knight?
  "Can piece in INDEX of BOARD be captured by OPPONENTs knight."
  [board index opponent]
  (not (nil? (some #(= (get board %)
                       (if (= opponent :white)
                         WHITE-KNIGHT BLACK-KING))
                   (map #(+ index %) knight-movement)))))

(defn- threaten-by-pawn?
  "Can the piece in INDEX of BOARD be captured by OPPONENTs pawn?"
  [board index opponent]
  (if (= opponent :white)
    (or (= (get board (+ index SE)) WHITE-PAWN)
        (= (get board (+ index SW)) WHITE-PAWN))
    (or (= (get board (+ index NE)) BLACK-PAWN)
        (= (get board (+ index NW)) BLACK-PAWN))))

(defn- threaten-by-queen-or-rook?
  "Can the piece in INDEX of BOARD be captured
   by OPPONENTs queen or rook?"
  [board index opponent]
  (not (nil? (some true? (map #(ray-to-pieces? board index %
                                               (if (= opponent :white)
                                                 [WHITE-QUEEN WHITE-ROOK]
                                                 [BLACK-QUEEN BLACK-ROOK]))
                              [NORTH EAST WEST SOUTH])))))

(defn- threaten-by-queen-or-bishop?
  "Can the piece in INDEX on BOARD be captured by
   OPPONENTs queen or bishop?"
  [board index opponent]
  (not (nil? (some true? (map #(ray-to-pieces? board index %
                                               (if (= opponent :white)
                                                 [WHITE-QUEEN WHITE-BISHOP]
                                                 [BLACK-QUEEN BLACK-BISHOP]))
                              [NE NW SW SE])))))
(declare threaten-index?)
(declare update-board)
(defn- threaten-by-king?
  "Can the piece in INDEX on BOARD be captured by OPPONENTs king."
  [board index opponent]
  (let [player (if (= opponent :white) :black :white)
        enemy-king (if (= opponent :black) BLACK-KING WHITE-KING)
        king-move-indexes (map #(+ index %) king-movement)
        enemy-king-index (first (filter #(= (get board %) enemy-king)
                                        king-move-indexes))]
    (if (empty? enemy-king-index)
      false
      (-> (fill-square board index enemy-king)
          (threaten-index? index player)))))

(defn- threaten-index?
  "Checks if given INDEX in STATE is under threath of enemy."
  [board index opponent]
  (or
   (threaten-by-knight? board index opponent)
   (threaten-by-queen-or-bishop? board index opponent)
   (threaten-by-queen-or-rook? board index opponent)
   (threaten-by-pawn? board index opponent)
   (threaten-by-king? board index opponent)))

(defn- legal-castling?
  "Predicate to check if castling is possible on the board."
  [player board index direction]
  (let [king-index-1 (+ index direction)
        king-index-2 (+ king-index-1 direction)
        rook-index (if (= direction WEST)
                     (+ king-index-2 WEST WEST)
                     (+ king-index-2 EAST))
        opponent (opponent player)]
    (and (not (threaten-index? board index opponent))
         (not (board-occupied? board king-index-1))
         (not (threaten-index? board king-index-1 opponent))
         (not (board-occupied? board king-index-2))
         (not (threaten-index? board king-index-2 opponent))
         (if (= direction WEST)
           (not (board-occupied? board (+ king-index-2 direction)))
           true)
         (= (get board rook-index)
            (if (= player :white)
              WHITE-ROOK BLACK-ROOK)))))

(defn- list-king-moves
  "Resolves all available moves for king in given INDEX of STATE."
  [player board index castling]
  (let [normal-moves (flatten (map #(move-to-place board index (+ index %) player)
                                   king-movement))
        castling-moves-king (if (and (castle-side? player KING-SIDE castling)
                                     (legal-castling? player board index WEST))
                              (make-move index (+ WEST WEST) nil)
                              '())
        castling-moves-queen (if (and (castle-side? player QUEEN-SIDE castling)
                                      (legal-castling? player board index EAST))
                               (make-move index (+ EAST EAST) nil)
                               '())]
    (concat normal-moves castling-moves-king castling-moves-queen)))

(defn- list-pawn-normal-moves
  "Lists available moves for PLAYER's pawn in BOARD INDEX."
  [player board index]
  (let [direction (if (= player :white) NORTH SOUTH)
        move-index (+ index direction)]
    (if (not (board-occupied? board move-index))
      (if (and (not (board-occupied? board (+ move-index direction)))
               (or (and (= player :white) (same-row? index 0x10))
                   (and (= player :black) (same-row? index 0x60))))
        (list (make-move index move-index nil)
              (make-move index (+ move-index direction) nil))
        (list (make-move index move-index nil)))
      '())))

(defn- list-pawn-capture-moves
  "List of possible capture moves of pawn."
  [player board index en-passant]
  (let [direction (if (= player :white) NORTH SOUTH)
        move-index (+ index direction)
        captures (if (= player :white)
                   (list (+ NW index) (+ NE index))
                   (list (+ SW index) (+ SE index)))
        en-passant-index (if (= en-passant "-")
                           -1
                           (algebraic->index en-passant))]
    (map #(if (or (and (board-index? %)
                       (= en-passant-index %))
                  (and (board-occupied? board %)
                       (not (occupied-by? board % player))))
            (list (make-move index % nil))
            '())
         captures)))

(defn- list-pawn-moves
  "Returns a set of available pawn moves from INDEX in given STATE."
  [player board index en-passant]
  (flatten (conj (list-pawn-normal-moves player board index)
                 (list-pawn-capture-moves player board index en-passant))))

(defn- list-moves-for-piece
  "Generates a set of all available moves for piece at INDEX in given STATE."
  [state index]
  (let [board (:board state)
        player (if (occupied-by? board index :white) :white :black)]
    (case (Character/toLowerCase (char (piece-name (get board index))))
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
  [state]
  (flatten (map #(list-moves-for-piece state %)
                (all-piece-indexes-for (:board state) (:turn state)))))

(defn- all-states-for
  "Returns all states attainable by applying move."
  [state moves]
  (map #(apply-move state %) moves))

(defn- fen-board->0x88board
  "Converts given FEN board representation
   to 0x88 board representation."
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
                          (s/map-str #(piece-name (get board (+ row %)))
                                     (range 8)))))

(defn- board->fen-board
  "Convert the given state's BOARD to fen board field."
  [board]
  (s/join "/" (map #(make-fen-row board %)
                   [0x70 0x60 0x50 0x40 0x30 0x20 0x10 0x0])))

(defn- get-promotion-piece
  "Helper function to return new piece char.
    Gets the char from move or if nil, defaults to queen."
  [player move]
  (if (nil? (:promotion move))
    (if (= player :white) \Q \q)
    (if (= player :white)
      (Character/toUpperCase (char (:promotion move)))
      (Character/toLowerCase (char (:promotion move))))))

(defn- update-board
  "Returns state with new board after applying MOVE to STATE."
  [move state]
  (assoc state :board
         (let [board (:board state)
               player (:tun state)
               moving-piece (get board (:from move))]
           (cond (promotion? moving-piece move)
                 (-> board
                     (clear-square (:from move))
                     (fill-square (:to move)
                                  (piece-char->value (get-promotion-piece player move))))
                 (castling? moving-piece move)
                 (commit-castle-move player board move
                                     (if (= column (:to move) 2)
                                       QUEEN-SIDE
                                       KING-SIDE))
                 :else (-> board
                           (clear-square (:from move))
                           (fill-square (:to move) moving-piece))))))

(defn- pawn-or-capture-move?
  "Predicate to see if move was pawn move or a capture"
  [piece board move]
  (or (= piece WHITE-PAWN)
      (= piece BLACK-PAWN)
      (not (= (get board (:to move)) EMPTY))))

(defn- update-castling
  "Return new castling string for move
    checks for king or rook moves."
  [move state]
  (assoc state :castling
         (if (= (:castling state) "-")
           "-"
           (let [player (:turn state)
                 [king queen king-sq rook-q-sq rook-k-sq]
                 (if (= player :white)
                   ["K" "Q" 0x05 0x00 0x07]
                   ["k" "q" 0x75 0x70 0x77])
                 check-str (fn [x] (if (empty? x) "-" x))
                 player-str (reduce str (re-seq (if (= player :white)
                                                  #"\p{Upper}" #"\p{Lower}")
                                                (:castling state)))
                 opponent-str (s/replace-str player-str "" (:castling state))]
             (reduce str (sort
                          (cond (= (:from move) king-sq)
                                (check-str opponent-str)
                                (= (:from move) rook-q-sq)
                                (check-str (str (s/replace-str queen "" player-str)
                                                opponent-str))
                                (= (:from move) rook-k-sq)
                                (check-str (str (s/replace-str king "" player-str)
                                                opponent-str))
                                :else (str player-str opponent-str))))))))

(defn- update-turn
  "Updates player turn of STATE"
  [state]
  (assoc state :turn
         (if (= (:turn state) :black)
           :white :black)))

(defn- update-en-passant
  "Associates new en-passant string with given STATE based on the MOVE."
  [move state]
  (assoc state :en-passant
         (let [piece (get (:board state) (:from move))]
           (if (and (or (= piece WHITE-PAWN)
                        (= piece BLACK-PAWN))
                    (= (abs (- (:to move) (:from move))) 0x20))
             (index->algebraic (/ (+ (:to move) (:from move)) 2))
             "-"))))

(defn- allowed-move?
  "Checks if given MOVE is allowed in STATE."
  [state move]
  (not (nil? (some #(and (= (:from move) (:from %))
                         (= (:to move) (:to %)))
                   (flatten (list-moves-for-piece state (:from move)))))))

(defn- update-half-moves
  "Increases STATE half moves count unless the move
    pawn move or a capture."
  [move state]
  (assoc state :half-moves
         (let [piece (get (:board state) (:from move))]
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
        (recur (cons index coords)
               (cons (piece-name (get (:board state) index)) pieces)
               (inc index))
        (recur coords pieces (inc index))))))

(defn- commit-move
  "Commit move in state if legal move.
   On legal move function returns new state after move has been
  applied to previous state.
  On illegal move this function will return nil value."
 [state move]
 (when (and (occupied-by? (:board state) (:from move) (:turn state))
            (not (game-end? state))
            (allowed-move? state move))
   (let [new-state (->> state
                        (update-board move)
                        update-turn
                        (update-castling move)
                        (update-en-passant move)
                        (update-half-moves move)
                        update-full-moves
                        (update-move move))]
     (when (not (in-check? new-state))
       new-state))))

(extend-type State0x88
  State
  (occupied? [state index]
             (board-occupied? (:board state) index))
  (black? [state index]
          (occupied-by? (:board state) index :black))
  (white? [state index]
          (occupied-by? (:board state) index :white))
  (apply-move [state move]
              (commit-move state move))
  (in-check? [state]
             (threaten-index? (:board state)
                              (king-index (:board state) (opponent (:turn state)))
                              (:turn state)))
  (game-end? [state]
             (or (>= (:half-moves state) 50)
                 (nil? (king-index (:board state) (:turn state)))))
  (state->fen [state]
              (parse-state state))
  (legal-states [state]
                (filter #(not (or
                               (nil? %)
                               (nil? (king-index (:board state) (:turn state)))
                               (in-check? %)
                               (game-end? %)))
                        (all-states-for state (all-moves-for state))))
  (get-pieces [state]
              (build-piece-map state))
  (perft [state depth]
    (if (zero? depth)
      1
      (let [states (legal-states state)]
        (reduce + (map #(perft % (dec depth)) states))))))

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
                            nil))))
