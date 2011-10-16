(ns tursas.state0x88.movegen
  (:use (tursas state move util)
        (tursas.state0x88 common util move)))

(def rook-directions (list north south east west))
(def bishop-directions (list nw sw ne se))
(def queen-directions (concat rook-directions bishop-directions))

(def king-movement queen-directions)
(def black-pawn-movement (list se sw south))
(def white-pawn-movement (list ne nw north))
(def knight-movement (list -33 -31 -18 -14 14 18 31 33))

(defn pmap-get
  "Returns the players piece-map from board."
  [state player]
  (if (== player white)
    (:white-pieces state)
    (:black-pieces state)))

(defn- pmap-add
  "Add piece to player piece-map store on the board."
  [state player index piece]
  (assoc state (if (== player white) :white-pieces :black-pieces)
         (assoc (pmap-get state player) index piece)))

(defn pmap-remove
  "Remove piece from player piece-map store on the board."
  [state player index]
  (assoc state (if (== player white) :white-pieces :black-pieces)
         (dissoc (pmap-get state player) index)))

(defn- set-dynamic
  "Sets the states dynamic value, now only set on captures."
  [state value]
  (assoc state :board
         (fill-square (:board state) dynamic-store value)))

(defn add-piece
  "Associates given piece to states board."
  [state index piece]
  (let [[player king] (if (white-piece? piece)
                        [white white-king]
                        [black black-king])]
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
                 white black)]
    (-> (assoc state :board (clear-square (:board state) index))
        (pmap-remove player index))))

(defn move-piece
  "Moves piece in the board."
  [state move]
  (let [piece (int (get (:board state) (:from move)))
        occupant (int (get (:board state) (:to move)))
        player (if (white-piece? piece) white black)]
    (-> (if (== occupant empty-square)
          state
          (remove-piece state (:to move)))
        (remove-piece (:from move))
        (add-piece (:to move) piece)
        (set-dynamic (if (== occupant empty-square) 0 1)))))

(defn move-castling-pieces
  "Helper function for update-board to make castling move on board.
   Mainly it moves the king piece and the participating rook piece."
  [player state move castling-side]
  (let [[rook king from to]
        (if (== player white)
          [white-rook white-king [0x00 0x07] [0x03 0x05]]
          [black-rook black-king [0x70 0x77] [0x73 0x75]])]
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
  (and (or (== piece white-king)
           (== piece black-king))
       (== 2 (abs (- (:to move)
                     (:from move))))))

(defn en-passant?
  "Is the given move an en-passant move"
  [board piece move]
  (let [movement (- (:to move) (:from move))]
    (and (or (== piece white-pawn)
             (== piece black-pawn))
         (true? (some #(== % movement) [sw se ne nw]))
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
          :else (true? (some #(== (get board new-index) %)
                             pieces)))))

(defn- threaten-by-piece?
  "Can piece in index be captured by opponents pieces."
  [board index opponent piece places]
  (true? (some #(= (get board %) piece)
               (filter board-index?
                       (map #(+ index %) places)))))

(defn- threaten-by-slider?
  "Can the piece in index of board be captured
   by opponents queen or rook?"
  [board index opponent pieces directions]
  (true? (some true? (map #(ray-to-pieces? board index % pieces)
                          directions))))

(declare threatened?)
(defn- threaten-by-king?
  "Can the piece in index on board be captured by opponents king.
   Checks this by looking for a king within next squares and then
   checking if it can move to index and not be threatened instead."
  [board index opp]
  (let [[player king opp-king] (if (== opp white)
                                 [black black-king white-king]
                                 [white white-king black-king])
        idx (int index)
        opp-king-idx (king-index board opp)]
    (if-not (true? (some #(== opp-king-idx %)
                         (map #(+ idx %) king-movement)))
      false
      (if (== (get board idx) king)
        true
        (not (-> board
                 (clear-square (int opp-king-idx))
                 (fill-square idx opp-king)
                 (threatened? idx player)))))))

(defn- threaten-by-white?
  "Checks if given index is threatened by white player."
  [board index]
  (let [threaten-by-p? (partial threaten-by-piece? board index white)
        threaten-by-s? (partial threaten-by-slider? board index white)]
    (or (threaten-by-p? white-knight knight-movement)
        (threaten-by-s? [white-queen white-rook] rook-directions)
        (threaten-by-s? [white-queen white-bishop] bishop-directions)
        (threaten-by-p? white-pawn [se sw])
        (threaten-by-king? board index white))))

(defn- threaten-by-black?
  "Checks if given index is threatened by black player."
  [board index]
  (let [threaten-by-p? (partial threaten-by-piece? board index black)
        threaten-by-s? (partial threaten-by-slider? board index black)]
    (or (threaten-by-p? black-knight knight-movement)
        (threaten-by-s? [black-queen black-rook] rook-directions)
        (threaten-by-s? [black-queen black-bishop] bishop-directions)
        (threaten-by-p? black-pawn [ne nw])
        (threaten-by-king? board index black))))

(defn threatened?
  "Checks if given index on board is threatened by opponent."
  [board index opponent]
  (if (== opponent white)
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
         (if (== dir west)
           (empty-square? board (+ king-sq-2 dir))
           true))))

(defn- list-king-moves
  "Returns a list of available moves for players king
   in given index on the board."
  [player board index]
  (let [castling (byte (get board castling-store))
        castle-side? (fn [side castling]
                       (let [value (if (== player white)
                                     (if (== side king-side) 8 4)
                                     (if (== side king-side) 2 1))]
                         (pos? (bit-and value castling))))
        castling-move (fn [side dir]
                        (when (and (castle-side? side castling)
                                   (legal-castling? player board index dir))
                          (list (make-move index (+ index dir dir) 0))))]
    (concat
     (mapcat (partial move-to-place player board index) king-movement)
     (castling-move king-side east)
     (castling-move queen-side west))))

(defn- make-pawn-move
  "Utility function to create pawn moves.
   Needed to handle promotions."
  [player from to]
  (make-move from to
             (cond (and (== player white)
                        (== (row to) 0x70)) white-queen
                   (and (== player black)
                        (== (row to) 0x00)) black-queen
                   :else 0)))

(defn- list-pawn-normal-moves
  "Returns a list of normal pawn moves available
   for player in board index."
  [player board index]
  (let [dir (if (== player white) north south)
        move-index (int (+ index dir))]
    (when (and (board-index? move-index)
               (empty-square? board move-index))
      (if (and (board-index? (+ move-index dir))
               (empty-square? board (+ move-index dir))
               (or (and (== player white)
                        (same-row? index 0x10))
                   (and (== player black)
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
                 (== (get board en-passant-store) place))
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
                  (if (== player white)
                    (list (+ nw index) (+ ne index))
                    (list (+ sw index) (+ se index))))))

(defn- piece-moves
  "Returns a list of possible piece moves in board index."
  [board player index piece]
  (lazy-seq
   (let [slider (fn [directions]
                  (mapcat (partial slide-in-dir player board index) directions))
         mover (fn [movement]
                  (mapcat (partial move-to-place player board index) movement))]
     (cond (or (== piece white-pawn)
               (== piece black-pawn)) (list-pawn-moves player board index)
           (or (== piece white-bishop)
               (== piece black-bishop)) (slider bishop-directions)
           (or (== piece white-knight)
               (== piece black-knight)) (mover knight-movement)
           (or (== piece white-rook)
               (== piece black-rook)) (slider rook-directions)
           (or (== piece white-queen)
               (== piece black-queen)) (slider queen-directions)
           (or (== piece white-king)
               (== piece black-king)) (list-king-moves player board index)))))

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
  (let [player (int (get (:board state) turn-store))
        piece (int (get (:board state) (int (:from move))))]
    (and (occupied-by? (:board state) (int (:from move)) player)
         (true? (some #(and (== (:from move) (:from %))
                            (== (:to move) (:to %)))
                      (piece-moves (:board state) player (:from move) piece))))))
