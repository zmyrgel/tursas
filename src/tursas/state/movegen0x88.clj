(ns tursas.state.movegen0x88
  (:use (tursas state move hexmove)
        (tursas.state common0x88 util0x88)
        [clojure.contrib.math :only [abs]]))

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

(def rook-directions (list NORTH SOUTH EAST WEST))
(def bishop-directions (list NW SW NE SE))
(def queen-directions (concat rook-directions bishop-directions))

(def king-movement queen-directions)
(def black-pawn-movement (list SE SW SOUTH))
(def white-pawn-movement (list NE NW NORTH))
(def knight-movement (list -33 -31 -18 -14 14 18 31 33))

(defn promotion?
  "Checks if given move is pawn promotion."
  [piece move]
  (or (and (= piece WHITE-PAWN)
           (= (row (:to move)) 0x07))
      (and (= piece BLACK-PAWN)
           (= (row (:to move)) 0x00))))

(defn- pmap-add
  "Add piece to player piece-map store on the board."
  [state player index piece]
  (if (= player WHITE)
    (assoc state :white-pieces
           (assoc (:white-pieces state) index piece))
    (assoc state :black-pieces
           (assoc (:black-pieces state) index piece))))

(defn pmap-remove
  "Remove piece from player piece-map store on the board."
  [state player index]
  (if (= player WHITE)
    (assoc state :white-pieces
           (dissoc (:white-pieces state) index))
    (assoc state :black-pieces
           (dissoc (:black-pieces state) index))))

(defn pmap-get
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

(defn add-piece
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
    (pmap-add new-state player index piece)))

(defn remove-piece
  "Removes piece from board and updates maps accordingly."
  [state index]
  (let [player (if (white-piece? (get (:board state) index))
                 WHITE BLACK)
        cleared-board (clear-square (:board state) index)
        new-state (assoc state :board cleared-board)]
    (pmap-remove new-state player index)))

(defn move-piece
  "Moves piece in the board."
  [state move]
  (let [piece (get (:board state) (:from move))
        occupant (get (:board state) (:to move))
        player (if (white-piece? piece) WHITE BLACK)]
    (-> state
        (remove-piece (:from move))
        (add-piece (:to move) piece)
        (set-dynamic (if (= occupant EMPTY) 0 1)))))

(defn promote-piece
  "Promotes piece in index to new-piece value."
  [state index new-piece]
  (-> state
      (remove-piece index)
      (add-piece index new-piece)))

(defn- piece-indexes
  "Gets a list of all board indexes containing
   player's pieces in given board."
  [state player]
  (keys (pmap-get state player)))

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

(defn- slide-in-direction
  "Returns a list of possible moves by sliding piece
   from index to given direction on the board.
   Sliding will continue until it hits piece or board edge."
  [player board index direction]
  (loop [new-index (+ index direction)
         moves '()]
    (if (or (not (board-index? new-index))
            (occupied-by? board new-index player))
      moves
      (if (not (board-occupied? board new-index))
        (recur (+ new-index direction)
               (cons (make-move index new-index 0)
                     moves))
        (cons (make-move index new-index 0)
              moves)))))

(defn- move-to-place
  "Return list of moves for given piece."
  [player board index place]
  (let [new-place (+ index place)]
    (when (and (board-index? new-place)
               (or (not (board-occupied? board new-place))
                   (occupied-by? board new-place (opponent player))))
      (list (make-move index new-place 0)))))

(defn- ray-to-pieces?
  "Checks if there's ray to from INDEX to given PIECES."
  [board index inc pieces]
  (let [new-index (+ index inc)]
    (cond (not (board-index? new-index)) false
          (not (board-occupied? board new-index)) (recur board new-index inc pieces)
          :else (not (nil? (some #(= (get board new-index) %) pieces))))))

(defn- threaten-by-piece?
  "Can piece in index be captured by opponents pieces."
  [board index opponent piece placements]
  (not (nil? (some #(= (get board %) piece)
                   placements))))

(defn- threaten-by-slider?
  "Can the piece in INDEX of BOARD be captured
   by OPPONENTs queen or rook?"
  [board index opponent pieces directions]
  (not (nil? (some true? (map #(ray-to-pieces? board index % pieces)
                              directions)))))

(declare threaten-index?)
(defn- threaten-by-king?
  "Can the piece in INDEX on BOARD be captured by OPPONENTs king."
  [board index opponent]
  (let [player (if (= opponent WHITE) BLACK WHITE)
        enemy-king (if (= opponent BLACK) BLACK-KING WHITE-KING)
        king-move-indexes (map #(+ index %) king-movement)
        enemy-king-index (first (filter #(= (get board %) enemy-king)
                                        king-move-indexes))]
    (if (nil? enemy-king-index)
      false
      (-> (fill-square board index enemy-king)
          (threaten-index? index player)))))

(defn threaten-index?
  "Checks if given INDEX in STATE is under threath of enemy.
   Inf loop with king-index check"
  [board index opponent]
  (let [[qb qr knight pawn pawn-places]
        (if (= opponent WHITE)
          [[WHITE-QUEEN WHITE-BISHOP]
           [WHITE-QUEEN WHITE-ROOK]
           WHITE-KNIGHT WHITE-PAWN [SE SW]]
          [[BLACK-QUEEN BLACK-BISHOP]
           [BLACK-QUEEN BLACK-ROOK]
           BLACK-KNIGHT BLACK-PAWN [NE NW]])]
    (or (threaten-by-piece? board index opponent knight knight-movement)
        (threaten-by-slider? board index opponent qr rook-directions)
        (threaten-by-slider? board index opponent qb bishop-directions)
        (threaten-by-piece? board index opponent pawn pawn-places)
        (threaten-by-king? board index opponent))))

(defn- empty-and-safe?
  "Predicate to see if given index is empty
   and unthreatened by opponent."
  [board index opponent]
  (and (not (board-occupied? board index))
       (not (threaten-index? board index opponent))))

(defn- legal-castling?
  "Predicate to check if castling is possible on the board.
   XXX: incorrect index given when calculating score from position: startpos
        gives index as 2 but castle-side? should return false in that case.
        related to fact that castling won't get updated if rooks are captured?"
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
           true))))

(defn- list-moves
  "Make a list of moves generated for piece in index of board.
   Moves are generated by provided gen-fn function."
  [player board index gen-fn movement]
  (reduce (fn [moves arg]
            (concat moves (gen-fn player board index arg)))
          '() movement))

(defn- list-king-moves
  "Returns a list of  all available moves for players king
   in given index on the board."
  [player board index]
  (let [castling (get board CASTLING-STORE)
        castling-move (fn [side direction]
                        (when (and (= (column index) 4)
                                   (castle-side? player side castling)
                                   (legal-castling? player board index direction))
                          (make-move index (+ direction direction) 0)))]
    (concat
     (reduce (fn [moves diff]
               (concat moves (move-to-place player board index (+ index diff))))
             '() king-movement)
     (castling-move KING-SIDE WEST)
     (castling-move QUEEN-SIDE EAST))))

(defn- list-pawn-normal-moves
  "Returns lists of normail pawn moves available
   for player in board index."
  [player board index]
  (let [direction (if (= player WHITE) NORTH SOUTH)
        move-index (+ index direction)]
    (when (and (board-index? move-index)
               (not (board-occupied? board move-index)))
      (if (and (board-index? (+ move-index direction))
               (not (board-occupied? board (+ move-index direction)))
               (or (and (= player WHITE) (same-row? index 0x10))
                   (and (= player BLACK) (same-row? index 0x60))))
        (list (make-move index move-index 0)
              (make-move index (+ move-index direction) 0))
        (list (make-move index move-index 0))))))

(defn- pawn-capture
  "Function to generate pawn capture moves.
   If pawn of index can capture piece in place, generate the move
   otherwise return nil."
  [player board index place]
  (when (or (and (board-index? place)
                 (= (get board EN-PASSANT-STORE) place))
            (and (board-index? place)
                 (board-occupied? board place)
                 (not (occupied-by? board place player))))
    (make-move index place 0)))

(defn- list-pawn-moves
  "Returns a set of available pawn moves from INDEX in given STATE."
  [player board index]
  (concat (list-pawn-normal-moves player board index)
          (list-moves player board index pawn-capture
                      (if (= player WHITE)
                        [(+ NW index) (+ NE index)]
                        [(+ SW index) (+ SE index)]))))

(defn- piece-moves
  "List of moves for piece in board index."
  [board player index piece]
  (cond (or (= piece WHITE-PAWN)
            (= piece BLACK-PAWN))
        (list-pawn-moves player board index)
        (or (= piece WHITE-BISHOP)
            (= piece BLACK-BISHOP))
        (list-moves player board index
                    slide-in-direction bishop-directions)
        (or (= piece WHITE-KNIGHT)
            (= piece BLACK-KNIGHT))
        (list-moves player board index
                    move-to-place knight-movement)
        (or (= piece WHITE-ROOK)
            (= piece BLACK-ROOK))
        (list-moves player board index
                    slide-in-direction rook-directions)
        (or (= piece WHITE-QUEEN)
            (= piece BLACK-QUEEN))
        (list-moves player board index
                    slide-in-direction queen-directions)
        (or (= piece WHITE-KING)
            (= piece BLACK-KING))
        (list-king-moves player board index)
        :else nil))

(defn- pseudo-moves
  "Lists all pseudo-moves for player in state.
   Still doesn't live up to its name as all moves returned are also
   legal.  Later the move generation should be separated to pseudo
   moves and legal moves."
  [state player]
  (let [board (:board state)
        pieces (seq (pmap-get state player))]
    (reduce (fn [moves [index piece]]
              (concat moves (piece-moves board player index piece)))
            '() pieces)))

(defn moves
  "Returns a set of all available moves for SIDE in STATE."
  [state]
  (pseudo-moves state (get (:board state) TURN-STORE)))

(defn states
  "Returns all legal states attainable by applying move."
  [state moves]
  (let [states (reduce (fn [states move]
                         (if-let [new-state (apply-move state move)]
                           (cons new-state states)
                           states))
                       '() moves)]
    (filter #(not (or (nil? (king-index state (get (:board state) TURN-STORE)))
                      (check? %))) states)))

(defn allowed-move?
  "Checks if given MOVE is allowed in STATE."
  [state move]
  (let [board (:board state)
        player (get (:board state) TURN-STORE)
        from (:from move)
        to (:to move)
        piece (get board from)]
    (and (occupied-by? board from player)
         (not (nil? (some #(and (= from (:from %))
                                (= to (:to %)))
                          (piece-moves board player from piece)))))))
