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

(def WHITE-KING-SIDE 8)
(def WHITE-QUEEN-SIDE 4)
(def BLACK-KING-SIDE 2)
(def BLACK-QUEEN-SIDE 1)
(def KING-SIDE 1)
(def QUEEN-SIDE 0)

(def WHITE 0)
(def BLACK 1)

(def WHITE-KING-STORE 0x0c)
(def BLACK-KING-STORE 0x7c)
(def TURN-STORE 0x38)
(def HALF-MOVE-STORE 0x39)
(def FULL-MOVE-STORE 0x3a)
(def CASTLING-STORE 0x58)
(def EN-PASSANT-STORE 0x59)
(def LAST-MOVE-FROM 0x7a)
(def LAST-MOVE-TO 0x7b)
(def DYNAMIC-STORE 0x5a)

(def BLACK-QUEEN -6)
(def BLACK-ROOK -5)
(def BLACK-BISHOP -4)
(def BLACK-KING -3)
(def BLACK-KNIGHT -2)
(def BLACK-PAWN -1)
(def EMPTY 0)
(def WHITE-PAWN 1)
(def WHITE-KNIGHT 2)
(def WHITE-KING 3)
(def WHITE-BISHOP 4)
(def WHITE-ROOK 5)
(def WHITE-QUEEN 6)

(def rook-directions (list NORTH SOUTH EAST WEST))
(def bishop-directions (list NW SW NE SE))
(def queen-directions (concat rook-directions bishop-directions))

(def king-movement queen-directions)
(def black-pawn-movement (list SE SW SOUTH))
(def white-pawn-movement (list NE NW NORTH))
(def knight-movement (list -33 -31 -18 -14 14 18 31 33))

(def board-indexes (reduce concat
                           (map #(map (fn [col] (+ col %)) (range 8))
                                [0x70 0x60 0x50 0x40 0x30 0x20 0x10 0x0])))

(defrecord State0x88 [board black-pieces white-pieces])

(defn- board-index?
  "Does the given INDEX represent a square on the board?"
  [index]
  (zero? (bit-and index 0x88)))

(defn- empty-square?
  "Checks if given INDEX on BOARD is empty."
  [board index]
  (zero? (get board index)))

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
  (> piece EMPTY))

(defn- black-piece?
  "Checks if given PIECE value belongs to black."
  [piece]
  (< piece EMPTY))

(defn- board-occupied?
  "Checks if BOARD INDEX is occupied by piece."
  [board index]
  (not (empty-square? board index)))

(defn- occupied-by?
  "Checks if given BOARD INDEX is occupied by PLAYER."
  [board index player]
  (and (board-occupied? board index)
       (if (= player WHITE)
         (white-piece? (get board index))
         (black-piece? (get board index)))))

(defn promotion?
  "Checks if given move is pawn promotion."
  [piece move]
  (or (and (= piece WHITE-PAWN)
           (= (row (:to move)) 0x07))
      (and (= piece BLACK-PAWN)
           (= (row (:to move)) 0x00))))

(defn castling?
  "Checks given move is castling move."
  [piece move]
  (and (or (= piece WHITE-KING)
           (= piece BLACK-KING))
       (= (abs (- (:to move) (:from move))) 2)))

(defn- castle-side?
  "Predicate to check if given piece can do castling."
  [player side castling]
  (let [value (if (= player WHITE)
                (if (= side KING-SIDE) 8 4)
                (if (= side KING-SIDE) 2 1))]
    (pos? (bit-and value castling))))

(defn- castling-str
  "Converts internal castling representation to string."
  [board]
  (let [castling (get board CASTLING-STORE)
        add-char (fn [castling value letter]
                   (when (pos? (bit-and castling value)) letter))]
    (str (add-char castling 8 \K)
         (add-char castling 4 \Q)
         (add-char castling 2 \k)
         (add-char castling 1 \q))))

(defn- castling-value
  "Convers castling string to value."
  [castling]
  (letfn [(convert [letter value result]
                   (if (some #(= % letter) castling)
                     (+ result value)
                     result))]
    (->> 0
         (convert \K 8)
         (convert \Q 4)
         (convert \k 2)
         (convert \q 1))))

(defn- opponent
  "Return opponent of given player"
  [player]
  (bit-xor player 1))

(defn- init-game-board
  "Generates new 128 element vector of bytes
   and places chess piece representation to it."
  []
  (into (vector-of :byte)
        (vec (replicate 128 EMPTY))))

(defn- fill-square
  "Return new board with given VALUE added to given BOARD's INDEX."
  [board index value]
  (assoc board index value))

(defn- clear-square
  "Clears the given square INDEX on the game BOARD."
  [board index]
  (fill-square board index EMPTY))

(defn- update-king-index
  "Updates INDEX of given PLAYER's king in outer board."
  [board index player]
  (if (= player WHITE)
    (fill-square board WHITE-KING-STORE index)
    (fill-square board BLACK-KING-STORE index)))

(defn- get-king-index
  "Returns the king index on the board."
  [player board]
  (if (= player WHITE)
    (get board WHITE-KING-STORE)
    (get board BLACK-KING-STORE)))

(defn- pmap-add
  "Add piece to player piece-map store on the board."
  [state player index piece]
  (if (= player WHITE)
    (assoc state :white-pieces
           (assoc (:white-pieces state) index piece))
    (assoc state :black-pieces
           (assoc (:black-pieces state) index piece))))

(defn- pmap-remove
  "Remove piece from player piece-map store on the board."
  [state player index]
  (if (= player WHITE)
    (assoc state :white-pieces
           (dissoc (:white-pieces state) index))
    (assoc state :black-pieces
           (dissoc (:black-pieces state) index))))

(defn- pmap-get
  "Returns the players piece-map from board."
  [state player]
  (if (= player WHITE)
    (:white-pieces state)
    (:black-pieces state)))

(defn- set-dynamic
  "Sets the states dynamic value, now only set on captures."
  [state value]
  (assoc state :board
         (fill-square (:board state) DYNAMIC-STORE value)))

(defn- add-piece
  "Adds given piece to board in state"
  [state index piece]
  (let [player (if (white-piece? piece) WHITE BLACK)
        new-board (if (or (= piece BLACK-KING)
                          (= piece WHITE-KING))
                    (-> (:board state)
                        (update-king-index index player)
                        (fill-square index piece))
                    (fill-square (:board state) index piece))
        new-state (assoc state :board new-board)]
    (pmap-add new-state index piece)))

(defn- remove-piece
  "Removes piece from board and updates maps accordingly."
  [state index]
  (let [player (if (white-piece? (get (:board state) index))
                 WHITE BLACK)
        cleared-board (clear-square (:board state) index)
        new-state (assoc state :board cleared-board)]
    (pmap-remove new-state player index)))

(defn- move-piece
  "Moves piece in the board."
  [state move]
  (let [piece (get (:board state) (:from move))
        occupant (get (:board state) (:to move))
        player (if (white-piece? piece) WHITE BLACK)]
    (if (= occupant EMPTY)
      (-> state
          (remove-piece (:from move))
          (add-piece (:to move) piece)
          (set-dynamic 0))
      (-> state
          (remove-piece (:to move))
          (remove-piece (:from move))
          (add-piece (:to move) piece)
          (set-dynamic 1)))))

(defn- promote-piece
  "Promotes piece in INDEX to VALUE."
  [state index new-piece]
  (-> state
      (remove-piece index)
      (add-piece index new-piece)))

(defn piece-name
  "Gives piece character representation from its board VALUE."
  [piece]
  (cond (= piece WHITE-KING) \K
        (= piece WHITE-QUEEN) \Q
        (= piece WHITE-BISHOP) \B
        (= piece WHITE-KNIGHT) \N
        (= piece WHITE-ROOK) \R
        (= piece WHITE-PAWN) \P
        (= piece BLACK-KING) \k
        (= piece BLACK-QUEEN) \q
        (= piece BLACK-BISHOP) \b
        (= piece BLACK-KNIGHT) \n
        (= piece BLACK-ROOK) \r
        (= piece BLACK-PAWN) \p
        :else \E))

(defn piece-value
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
  [player state move castling-side]
  (let [[rook king from to]
        (if (= player WHITE)
          [WHITE-ROOK WHITE-KING [0x00 0x07] [0x03 0x05]]
          [BLACK-ROOK BLACK-KING [0x70 0x77] [0x73 0x75]])]
    (-> state
        (remove-piece (:from move))
        (remove-piece (get from castling-side))
        (add-piece (:to move) king)
        (add-piece (get to castling-side) rook))))

(defn- slide-in-direction
  "Returns a set of possible moves by sliding piece
   from INDEX to DIRECTION in given STATE."
  [player board index direction]
  (loop [new-index (+ index direction)
         moves ()]
    (if (or (not (board-index? new-index))
            (occupied-by? board new-index player))
      moves
      (if (not (board-occupied? board (get board new-index)))
        (recur (+ new-index direction)
               (cons (make-move index new-index nil)
                     moves))
        (cons (make-move index new-index nil)
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
  (get board (if (= player WHITE)
               WHITE-KING-STORE BLACK-KING-STORE)))

(defn- threaten-by-knight?
  "Can piece in INDEX of BOARD be captured by OPPONENTs knight."
  [board index opponent]
  (not (nil? (some #(= (get board %)
                       (if (= opponent WHITE)
                         WHITE-KNIGHT BLACK-KING))
                   (map #(+ index %) knight-movement)))))

(defn- threaten-by-pawn?
  "Can the piece in INDEX of BOARD be captured by OPPONENTs pawn?"
  [board index opponent]
  (if (= opponent WHITE)
    (or (= (get board (+ index SE)) WHITE-PAWN)
        (= (get board (+ index SW)) WHITE-PAWN))
    (or (= (get board (+ index NE)) BLACK-PAWN)
        (= (get board (+ index NW)) BLACK-PAWN))))

(defn- threaten-by-queen-or-rook?
  "Can the piece in INDEX of BOARD be captured
   by OPPONENTs queen or rook?"
  [board index opponent]
  (not (nil? (some true? (map #(ray-to-pieces? board index %
                                               (if (= opponent WHITE)
                                                 [WHITE-QUEEN WHITE-ROOK]
                                                 [BLACK-QUEEN BLACK-ROOK]))
                              [NORTH EAST WEST SOUTH])))))

(defn- threaten-by-queen-or-bishop?
  "Can the piece in INDEX on BOARD be captured by
   OPPONENTs queen or bishop?"
  [board index opponent]
  (not (nil? (some true? (map #(ray-to-pieces? board index %
                                               (if (= opponent WHITE)
                                                 [WHITE-QUEEN WHITE-BISHOP]
                                                 [BLACK-QUEEN BLACK-BISHOP]))
                              [NE NW SW SE])))))
(declare threaten-index?)
(declare update-board)
(defn- threaten-by-king?
  "Can the piece in INDEX on BOARD be captured by OPPONENTs king."
  [board index opponent]
  (let [player (if (= opponent WHITE) BLACK WHITE)
        enemy-king (if (= opponent BLACK) BLACK-KING WHITE-KING)
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

(defn- empty-and-safe?
  "Predicate to see if INDEX is empty and unthreatened."
  [board index opponent]
  (and (not (board-occupied? board index))
       (not (threaten-index? board index opponent))))

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
         (empty-and-safe? board king-index-1 opponent)
         (empty-and-safe? board king-index-2 opponent)
         (if (= direction WEST)
           (not (board-occupied? board (+ king-index-2 direction)))
           true)
         (= (get board rook-index)
            (if (= player WHITE)
              WHITE-ROOK BLACK-ROOK)))))

(defn- list-king-moves
  "Resolves all available moves for king in given INDEX of STATE."
  [player board index]
  (let [castling (get board CASTLING-STORE)
        normal-moves (flatten (map #(move-to-place board index (+ index %) player)
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
  (let [direction (if (= player WHITE) NORTH SOUTH)
        move-index (+ index direction)]
    (if (and (board-index? move-index)
             (not (board-occupied? board move-index)))
      (if (and (not (board-occupied? board (+ move-index direction)))
               (or (and (= player WHITE) (same-row? index 0x10))
                   (and (= player BLACK) (same-row? index 0x60))))
        (list (make-move index move-index nil)
              (make-move index (+ move-index direction) nil))
        (list (make-move index move-index nil)))
      '())))

(defn- list-pawn-capture-moves
  "List of possible capture moves of pawn."
  [player board index]
  (let [direction (if (= player WHITE) NORTH SOUTH)
        move-index (+ index direction)
        captures (if (= player WHITE)
                   [(+ NW index) (+ NE index)]
                   [(+ SW index) (+ SE index)])
        en-passant-index (get board EN-PASSANT-STORE)]
    (map #(if (or (and (board-index? %)
                       (= en-passant-index %))
                  (and (board-index? %)
                       (board-occupied? board %)
                       (not (occupied-by? board % player))))
            (list (make-move index % nil))
            '())
         captures)))

(defn- list-pawn-moves
  "Returns a set of available pawn moves from INDEX in given STATE."
  [player board index]
  (flatten (conj (list-pawn-normal-moves player board index)
                 (list-pawn-capture-moves player board index))))

(defn- list-bishop-moves
  "Returns a list of bishop moves"
  [player board index]
  (map #(slide-in-direction player board index %)
       bishop-directions))

(defn- list-queen-moves
  "Returns a list of queen moves."
  [player board index]
  (map #(slide-in-direction player board index %)
       queen-directions))

(defn- list-rook-moves
  "Returns a list of rook moves."
  [player board index]
  (map #(slide-in-direction player board index %)
       rook-directions))

(defn- list-knight-moves
  "Returns a list of knight moves."
  [player board index]
  (map #(move-to-place board index (+ index %) player)
       knight-movement))

(defn- pseudo-moves-for
  "Generates a set of all available moves for piece at INDEX in given STATE."
  [state index]
  (let [board (:board state)
        player (if (occupied-by? board index WHITE) WHITE BLACK)]
    (case (Character/toLowerCase (char (piece-name (get board index))))
          \p (list-pawn-moves player board index)
          \b (list-bishop-moves player board index)
          \n (list-knight-moves player board index)
          \r (list-rook-moves player board index)
          \q (list-queen-moves player board index)
          \k (list-king-moves player board index)
          '())))

(defn- piece-indexes
  "Gets a list of all board indexes containing
   player's pieces in given board."
  [board player]
  (keys (pmap-get board player)))

(defn- all-moves-for
  "Returns a set of all available moves for SIDE in STATE."
  [state]
  (flatten (map #(pseudo-moves-for state %)
                (piece-indexes (:board state) (turn state)))))

(defn- all-states-for
  "Returns all states attainable by applying move."
  [state moves]
  (map #(apply-move state %) moves))

(defn- fen-board->0x88board
  "Converts given FEN board representation
   to 0x88 board representation."
  [fen-board]
  (reduce (fn [board [index piece]]
            (fill-square board index (piece-value piece)))
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
    (if (= player WHITE) \Q \q)
    (if (= player WHITE)
      (Character/toUpperCase (char (:promotion move)))
      (Character/toLowerCase (char (:promotion move))))))

(defn- update-board
  "Returns state with new board after applying MOVE to STATE."
  [move state]
  (assoc state :board
         (let [board (:board state)
               player (turn state)
               moving-piece (get board (:from move))]
           (cond (promotion? moving-piece move)
                 (promote-piece board (:to move)
                                (piece-value (get-promotion-piece player move)))
                 (castling? moving-piece move)
                 (commit-castle-move player board move
                                     (if (= column (:to move) 2)
                                       QUEEN-SIDE
                                       KING-SIDE))
                 :else (move-piece board move)))))

(defn- pawn-or-capture-move?
  "Predicate to see if move was pawn move or a capture"
  [board move]
  (or (= (get board (:from move)) WHITE-PAWN)
      (= (get board (:from move)) BLACK-PAWN)
      (not (= (get board (:to move)) EMPTY))))

(defn- update-castling
  "Return new castling string for move
    checks for king or rook moves."
  [move state]
  (assoc state :board
         (let [board (:board state)
               castling (get board CASTLING-STORE)]
           (if (zero? castling)
             0
             (let [player (get board TURN-STORE)
                   [king queen king-sq rook-q-sq rook-k-sq]
                   (if (= player WHITE)
                     ["K" "Q" 0x05 0x00 0x07]
                     ["k" "q" 0x75 0x70 0x77])
                   check-str (fn [x] (if (empty? x) "-" x))
                   player-str (reduce str (re-seq (if (= player WHITE)
                                                    #"\p{Upper}" #"\p{Lower}")
                                                  castling))
                   opponent-str (s/replace-str player-str "" castling)]
               (reduce str (sort
                            (cond (= (:from move) king-sq)
                                  (check-str opponent-str)
                                  (= (:from move) rook-q-sq)
                                  (check-str (str (s/replace-str queen "" player-str)
                                                  opponent-str))
                                  (= (:from move) rook-k-sq)
                                  (check-str (str (s/replace-str king "" player-str)
                                                  opponent-str))
                                  :else (str player-str opponent-str)))))))))

(defn- update-turn
  "Updates player turn value on board."
  [state]
  (assoc state :board
         (fill-square (:board state) TURN-STORE
                      (opponent (turn state)))))

(defn- update-en-passant
  "Associates new en-passant string with given STATE based on the MOVE."
  [move state]
  (assoc state :board
         (fill-square (:board state) EN-PASSANT-STORE
                      (let [piece (get (:board state) (:from move))]
                        (if (and (or (= piece WHITE-PAWN)
                                     (= piece BLACK-PAWN))
                                 (= (abs (- (:to move) (:from move))) 0x20))
                          (/ (+ (:to move) (:from move)) 2)
                          EMPTY)))))

(defn- allowed-move?
  "Checks if given MOVE is allowed in STATE."
  [state move]
  (not (nil? (some #(and (= (:from move) (:from %))
                         (= (:to move) (:to %)))
                   (flatten (pseudo-moves-for state (:from move)))))))

(defn- update-half-moves
  "Increases half move count on board unless the move
   was pawn or a capture move."
  [move state]
  (assoc state :board
         (fill-square (:board state) HALF-MOVE-STORE
                      (if (pawn-or-capture-move? (:board state) move)
                        0
                        (inc (get (:board state) HALF-MOVE-STORE))))))

(defn- update-full-moves
  "Updates full move count on board."
  [state]
  (if (= (turn state) BLACK)
    (assoc state :board
           (-> (:board state)
               (fill-square (:board state) FULL-MOVE-STORE
                            (inc (get (:board state) FULL-MOVE-STORE)))))
    state))

(defn- update-move
  "Update the previous move of board."
  [move state]
  (assoc state :board
         (-> (:board state)
             (fill-square LAST-MOVE-FROM (:from move))
             (fill-square LAST-MOVE-TO (:to move)))))

(defn- parse-state
  "Returns FEN representation of given STATE."
  [state]
  (let [board (:board state)]
    (str (board->fen-board board) " "
         (if (= (turn state) WHITE) "w" "b") " "
         (castling-str board) " "
         (index->algebraic (get board EN-PASSANT-STORE)) " "
         (get board HALF-MOVE-STORE) " "
         (get board FULL-MOVE-STORE))))

(defn- commit-move
  "Commit move in state if legal move.
   On legal move function returns new state after move has been
  applied to previous state.
  On illegal move this function will return nil value."
  [state move]
  (when (and (occupied-by? (:board state)
                           (:from move)
                           (turn state))
             (allowed-move? state move))
    (let [new-state (->> state
                         (update-board move)
                         update-turn
                         (update-castling move)
                         (update-en-passant move)
                         (update-half-moves move)
                         update-full-moves
                         (update-move move))]
      (when (and (not (check? new-state))
                 (not (draw? new-state)))
        new-state))))

(defn- fifty-move-rule?
  "Checks if state is draw according to 50-move rule."
  [state]
  (>= (get (:board state) HALF-MOVE-STORE) 50))

(defn- stalemate?
  "Check if given state is in stalemate."
  [state]
  (and (empty? (legal-states state))
       (check? state)))

(defn- fide-draw?
  "Checks if state is draw according to FIDE rules:
   - Both sides have only king piece.
   - One side has king and bishop or knight vs. others king
   - One sides king and two knights agains others bare king
   - Both sides have only bishop of same color besides kings"
  [state]
  false
  ;; (let [piece-count (count (keys (get-pieces state)))]
  ;;   (and (<= piece-count 4)
  ;;        (or (= piece-count 2)
  ;;            (and (= piece-count 3)
  ;;                 (or (not (nil? (some #(= \n ) ))))
  ;;                 (or (not (nil? (some #(= \n ))))))
  ;;            (and (= piece-count 4)
  ;;                 )
  ;;            )))
  )

(extend-type State0x88
  State
  (occupied? [state index]
             (board-occupied? (:board state) index))
  (black? [state index]
          (occupied-by? (:board state) index BLACK))
  (white? [state index]
          (occupied-by? (:board state) index WHITE))
  (check? [state]
          (threaten-index? (:board state)
                           (king-index (:board state) (opponent (turn state)))
                           (turn state)))
  (mate? [state]
         false)
  (draw? [state]
         (or (fifty-move-rule? state)
             (fide-draw? state)
             (stalemate? state)))
  (state->fen [state]
              (parse-state state))
  (apply-move [state move]
              (commit-move state move))
  (legal-states [state]
                (filter #(not (or (nil? %)
                                  (nil? (king-index (:board state) (turn state)))
                                  (check? %)))
                        (all-states-for state (all-moves-for state))))
  (get-pieces [state]
              (merge (:white-pieces state)
                     (:black-pieces state)))
  (turn [state]
        (get (:board state) TURN-STORE))
  (perft [state depth]
         (if (zero? depth)
           1
           (reduce + (map #(perft % (dec depth))
                          (legal-states state)))))
  (dynamic? [state]
            (= (get (:board state) DYNAMIC-STORE) 1))
  (full-moves [state]
              (get (:board state) FULL-MOVE-STORE)))

(defn- add-pieces
  "Adds all pieces from board to piece-map."
  [state]
  (loop [index 0x77
         blacks {}
         whites {}]
    (if (= index -1)
      (-> state
          (assoc :white-pieces whites)
          (assoc :black-pieces blacks))
      (recur (dec index)
             (if (black-piece? (get (:board state) index))
               (assoc blacks index (get (:board state) index))
               blacks)
             (if (white-piece? (get (:board state) index))
               (assoc whites index (get (:board state) index))
               whites)))))

(defprotocol Fen
  (fen->state [fen]))

(extend-type String
  Fen
  (fen->state [fen]
              (when-let [fen-list (re-seq #"\S+" fen)]
                (-> (State0x88.
                     (-> (fen-board->0x88board (first fen-list))
                         (fill-square TURN-STORE (if (= (second fen-list) "w")
                                                   WHITE BLACK))
                         (fill-square CASTLING-STORE (castling-value
                                                      (nth fen-list 2)))
                         (fill-square EN-PASSANT-STORE (if (= (nth fen-list 3) "-")
                                                         EN-PASSANT-STORE
                                                         (algebraic->index (nth fen-list 3))))
                         (fill-square HALF-MOVE-STORE (Integer/parseInt (nth fen-list 4)))
                         (fill-square FULL-MOVE-STORE (Integer/parseInt (nth fen-list 5))))
                     nil
                     nil)
                    (add-pieces)))))
