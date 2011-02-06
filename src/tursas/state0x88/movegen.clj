(ns tursas.state0x88.movegen
  (:use (tursas state move util)
        (tursas.state0x88 common util move)
        [clojure.contrib.math :only [abs]]))

(def rook-directions (list NORTH SOUTH EAST WEST))
(def bishop-directions (list NW SW NE SE))
(def queen-directions (concat rook-directions bishop-directions))

(def king-movement queen-directions)
(def black-pawn-movement (list SE SW SOUTH))
(def white-pawn-movement (list NE NW NORTH))
(def knight-movement (list -33 -31 -18 -14 14 18 31 33))

(defn- pmap-add
  "Add piece to player piece-map store on the board."
  [state player index piece]
  (let [[key piece-map] (if (== player WHITE)
                          [:white-pieces (:white-pieces state)]
                          [:black-pieces (:black-pieces state)])]
    (assoc state key (assoc piece-map index piece))))

(defn pmap-remove
  "Remove piece from player piece-map store on the board."
  [state player index]
  (let [[key piece-map] (if (== player WHITE)
                          [:white-pieces (:white-pieces state)]
                          [:black-pieces (:black-pieces state)])]
    (assoc state key (dissoc piece-map index))))

(defn pmap-get
  "Returns the players piece-map from board."
  [state player]
  (if (== player WHITE)
    (:white-pieces state)
    (:black-pieces state)))

(defn- set-dynamic
  "Sets the states dynamic value, now only set on captures."
  [state value]
  (assoc state :board
         (fill-square (:board state) DYNAMIC-STORE value)))

(defn add-piece
  "Associates given piece to states board."
  [state index piece]
  (let [[player king] (if (white-piece? piece)
                        [WHITE WHITE-KING]
                        [BLACK BLACK-KING])]
    (-> state
        (assoc :board (if (== piece king)
                        (-> (:board state)
                            (update-king-index index player)
                            (fill-square index piece))
                        (fill-square (:board state) index piece)))
        (pmap-add player index piece))))

(defn remove-piece
  "Removes piece from board and updates maps accordingly."
  [state index]
  (let [player (if (white-piece? (get (:board state) index))
                 WHITE BLACK)]
    (-> (assoc state :board (clear-square (:board state) index))
        (pmap-remove player index))))

(defn move-piece
  "Moves piece in the board."
  [state move]
  (let [piece (int (get (:board state) (:from move)))
        occupant (int (get (:board state) (:to move)))
        player (if (white-piece? piece) WHITE BLACK)]
    (-> (if (== occupant EMPTY)
          state
          (remove-piece state (:to move)))
        (remove-piece (:from move))
        (add-piece (:to move) piece)
        (set-dynamic (if (== occupant EMPTY) 0 1)))))

(defn move-castling-pieces
  "Helper function for update-board to make castling move on board.
   Mainly it moves the king piece and the participating rook piece."
  [player state move castling-side]
  (let [[rook king from to]
        (if (== player WHITE)
          [WHITE-ROOK WHITE-KING [0x00 0x07] [0x03 0x05]]
          [BLACK-ROOK BLACK-KING [0x70 0x77] [0x73 0x75]])]
    (-> state
        (remove-piece (:from move))
        (remove-piece (get from castling-side))
        (add-piece (:to move) king)
        (add-piece (get to castling-side) rook))))

(defn- piece-indexes
  "Gets a list of all board indexes containing
   player's pieces in given board."
  [state player]
  (keys (pmap-get state player)))

(defn castling?
  "Checks given move is castling move."
  [piece move]
  (and (or (== piece WHITE-KING)
           (== piece BLACK-KING))
       (== 2 (abs (- (:to move)
                     (:from move))))))

(defn en-passant?
  "Is the given move an en-passant move"
  [board piece move]
  (let [movement (- (:to move) (:from move))]
    (and (or (== piece WHITE-PAWN)
             (== piece BLACK-PAWN))
         (any? #(= % movement) [SW SE NE NW])
         (empty-square? board (:to move)))))

(defn- slide-in-dir
  "Returns a list of possible moves by sliding piece
   from index to given direction on the board.
   Sliding will continue until it hits piece or board edge."
  [player board index dir]
  (loop [new-index (int (+ index dir))
         moves '()]
    (if (or (not (board-index? new-index))
            (occupied-by? board new-index player))
      moves
      (if (empty-square? board new-index)
        (recur (int (+ new-index dir))
               (cons (make-move index new-index 0)
                     moves))
        (cons (make-move index new-index 0)
              moves)))))

(defn- move-to-place
  "Return list of moves for given piece."
  [player board index place]
  (let [new-place (int (+ index place))]
    (when (and (board-index? new-place)
               (or (empty-square? board new-place)
                   (occupied-by? board new-place (opponent player))))
      (list (make-move index new-place 0)))))

(defn- ray-to-pieces?
  "Checks if there's ray from index to given pieces."
  [board index dir pieces]
  (let [new-index (int (+ index dir))]
    (cond (not (board-index? new-index)) false
          (empty-square? board new-index) (recur board new-index dir pieces)
          :else (any? #(= (get board new-index) %) pieces))))

(defn- threaten-by-piece?
  "Can piece in index be captured by opponents pieces."
  [board index opponent piece places]
  (any? #(= (get board (+ index %)) piece)
        places))

(defn- threaten-by-slider?
  "Can the piece in index of board be captured
   by opponents queen or rook?"
  [board index opponent pieces directions]
  (any? true? (map #(ray-to-pieces? board index % pieces)
                   directions)))

(declare threatened?)
(defn- threaten-by-king?
  "Can the piece in index on board be captured by opponents king.
   Checks this by looking for a king within next squares and then
   checking if it can move to index and not be threatened instead."
  [board index opponent]
  (let [[player opp-king] (if (== opponent WHITE)
                            [BLACK WHITE-KING]
                            [WHITE BLACK-KING])]
    (if (empty? (filter #(= (get board %) opp-king)
                      (map #(+ index %) king-movement)))
      false
      (not (-> board
               (fill-square index opp-king)
               (threatened? index player))))))

(defn- threaten-by-white?
  "Checks if given index is threatened by white player."
  [board index]
  (let [threaten-by-p? (partial threaten-by-piece? board index WHITE)
        threaten-by-s? (partial threaten-by-slider? board index WHITE)]
    (or (threaten-by-p? WHITE-KNIGHT knight-movement)
        (threaten-by-s? [WHITE-QUEEN WHITE-ROOK] rook-directions)
        (threaten-by-s? [WHITE-QUEEN WHITE-BISHOP] bishop-directions)
        (threaten-by-p? WHITE-PAWN [SE SW])
        (threaten-by-king? board index WHITE))))

(defn- threaten-by-black?
  "Checks if given index is threatened by black player."
  [board index]
  (let [threaten-by-p? (partial threaten-by-piece? board index BLACK)
        threaten-by-s? (partial threaten-by-slider? board index BLACK)]
    (or (threaten-by-p? BLACK-KNIGHT knight-movement)
        (threaten-by-s? [BLACK-QUEEN BLACK-ROOK] rook-directions)
        (threaten-by-s? [BLACK-QUEEN BLACK-BISHOP] bishop-directions)
        (threaten-by-p? BLACK-PAWN [NE NW])
        (threaten-by-king? board index BLACK))))

(defn threatened?
  "Checks if given index on board is threatened by opponent."
  [board index opponent]
  (if (== opponent WHITE)
    (threaten-by-white? board index)
    (threaten-by-black? board index)))

(defn- legal-castling?
  "Predicate to check if castling is possible on the board."
  [player board index dir]
  (let [king-sq-1 (int (+ index dir))
        king-sq-2 (int (+ king-sq-1 dir))
        opponent (opponent player)
        safe-index? #(and (empty-square? board %)
                          (not (threatened? board % opponent)))]
    (and (not (threatened? board index opponent))
         (safe-index? king-sq-1)
         (safe-index? king-sq-2)
         (if (== dir WEST)
           (empty-square? board (+ king-sq-2 dir))
           true))))

(defn- list-king-moves
  "Returns a list of available moves for players king
   in given index on the board."
  [player board index]
  (let [castling (byte (get board CASTLING-STORE))
        castle-side? (fn [side castling]
                       (let [value (if (== player WHITE)
                                     (if (== side KING-SIDE) 8 4)
                                     (if (== side KING-SIDE) 2 1))]
                         (pos? (bit-and value castling))))
        castling-move (fn [side dir]
                        (when (and (castle-side? side castling)
                                   (legal-castling? player board index dir))
                          (list (make-move index (+ index dir dir) 0))))]
    (concat
     (mapcat (partial move-to-place player board index) king-movement)
     (castling-move KING-SIDE EAST)
     (castling-move QUEEN-SIDE WEST))))

(defn- make-pawn-move
  "Utility function to create pawn moves.
   Needed to handle promotions."
  [player from to]
  (make-move from to
             (cond (and (== player WHITE)
                        (== (row to) 0x70)) WHITE-QUEEN
                   (and (== player BLACK)
                        (== (row to) 0x00)) BLACK-QUEEN
                   :else 0)))

(defn- list-pawn-normal-moves
  "Returns a list of normal pawn moves available
   for player in board index."
  [player board index]
  (let [dir (if (== player WHITE) NORTH SOUTH)
        move-index (int (+ index dir))]
    (when (and (board-index? move-index)
               (empty-square? board move-index))
      (if (and (board-index? (+ move-index dir))
               (empty-square? board (+ move-index dir))
               (or (and (== player WHITE)
                        (same-row? index 0x10))
                   (and (== player BLACK)
                        (same-row? index 0x60))))
        (list (make-pawn-move player index move-index)
              (make-pawn-move player index (+ move-index dir)))
        (list (make-pawn-move player index move-index))))))

(defn- pawn-capture
  "Utility function to generate pawn capture moves.
   If pawn of index can capture piece in place, generate the move
   otherwise return nil."
  [player board index place]
  (when (or (and (board-index? place)
                 (== (get board EN-PASSANT-STORE) place))
            (and (board-index? place)
                 (board-occupied? board place)
                 (not (occupied-by? board place player))))
    (list (make-pawn-move player index place))))

(defn- list-pawn-moves
  "Returns a list of available pawn moves
   for player's pawn in board index."
  [player board index]
  (concat (list-pawn-normal-moves player board index)
          (mapcat (partial pawn-capture player board index)
                  (if (== player WHITE)
                    (list (+ NW index) (+ NE index))
                    (list (+ SW index) (+ SE index))))))

(defn- piece-moves
  "Returns a list of possible piece moves in board index."
  [board player index piece]
  (lazy-seq
   (let [slider (fn [directions]
                  (mapcat (partial slide-in-dir player board index) directions))
         mover (fn [movement]
                  (mapcat (partial move-to-place player board index) movement))]
     (cond (or (== piece WHITE-PAWN)
               (== piece BLACK-PAWN)) (list-pawn-moves player board index)
           (or (== piece WHITE-BISHOP)
               (== piece BLACK-BISHOP)) (slider bishop-directions)
           (or (== piece WHITE-KNIGHT)
               (== piece BLACK-KNIGHT)) (mover knight-movement)
           (or (== piece WHITE-ROOK)
               (== piece BLACK-ROOK)) (slider rook-directions)
           (or (== piece WHITE-QUEEN)
               (== piece BLACK-QUEEN)) (slider queen-directions)
           (or (== piece WHITE-KING)
               (== piece BLACK-KING)) (list-king-moves player board index)))))

(defn pseudo-moves
  "Lists all pseudo-moves for player in state.
   Note: moves generated can leave player in check, hence pseudo-moves."
  [player state]
  (mapcat (fn [[index piece]]
            (piece-moves (:board state) player index piece))
          (seq (pmap-get state player))))

(defn allowed-move?
  "Checks if given move is allowed in state.
   Prevents players from moving each others pieces."
  [state move]
  (let [player (int (get (:board state) TURN-STORE))
        piece (int (get (:board state) (int (:from move))))]
    (and (occupied-by? (:board state) (int (:from move)) player)
         (any? #(and (== (:from move) (:from %))
                     (== (:to move) (:to %)))
               (piece-moves (:board state) player (:from move) piece)))))
