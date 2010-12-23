(ns tursas.state.state0x88
  (:use (tursas state  move hexmove)
        (tursas.state eval0x88 movegen0x88 util0x88 common0x88 fen0x88)
        [clojure.contrib.math :only [abs]]))

(defn- fifty-move-rule?
  "Checks if state is draw according to 50-move rule."
  [state]
  (>= (get (:board state) HALF-MOVE-STORE) 50))

(defn- stalemate?
  "Check if given state is in stalemate."
  [state]
  (and (not (check? state))
       (empty? (legal-states state))))

(defn- fide-draw?
  "Checks if state is draw according to FIDE rules:
   - Both sides have only king piece.
   - One side has king and bishop or knight vs. others king
   - One sides king and two knights agains others bare king
   - Both sides have only bishop of same color besides kings"
  [state]
  (let [pieces (merge (:white-pieces state)
                      (:black-pieces state))
        keys (keys pieces)
        vals (vals pieces)
        piece-count (count keys)]
    (and (<= piece-count 4)
         (or (= piece-count 2)
             (and (= piece-count 3)
                  (not (nil? (some
                              #(or (= BLACK-KNIGHT %)
                                   (= BLACK-BISHOP %)
                                   (= WHITE-KNIGHT %)
                                   (= WHITE-BISHOP %))
                              vals))))
             (and (= piece-count 4)
                  (or (= (count (filter #(= BLACK-KNIGHT %) vals)) 2)
                      (= (count (filter #(= WHITE-KNIGHT %) vals)) 2)
                      (let [bishops (filter #(or (= BLACK-BISHOP (get pieces %))
                                                 (= WHITE-BISHOP (get pieces %)))
                                            keys)]
                        (cond (< (count bishops) 2) false
                              :else (same-color? (first (keys bishops))
                                                 (second (keys bishops)))))))))))
(defn- get-promotion-piece
  "Helper function to return promotion piece value.
    Reads the char from move or if nil, defaults to queen."
  [player move]
  (if-let [piece (:promotion move)]
    (if (= player WHITE) (Character/toUpperCase piece) piece)
    (if (= player WHITE) WHITE-QUEEN BLACK-QUEEN)))

(defn- move-castling-pieces
  "Helper function for update-board to make castling move on board.
   Mainly it moves the king piece and the participating rook piece."
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

(defn- update-board
  "Returns state with new board after applying MOVE to STATE."
  [move state]
  (when (not (nil? state))
    (let [board (:board state)
          player (get board TURN-STORE)
          moving-piece (get board (:from move))]
      (cond (promotion? moving-piece move)
            (promote-piece state (:to move)
                           (get-promotion-piece player move))
            (castling? moving-piece move)
            (move-castling-pieces player board move
                                  (if (= column (:to move) 2)
                                    QUEEN-SIDE
                                    KING-SIDE))
            :else (move-piece state move)))))

(defn- update-castling
  "Updates states castling value for move
    checks for king or rook moves."
  [move state]
  (when (not (nil? state))
    (assoc state :board
           (fill-square (:board state) CASTLING-STORE
                        (let [castling (get (:board state) CASTLING-STORE)]
                          (if (zero? castling)
                            0
                            (let [[k-mask qr-mask kr-mask king-sq rook-q-sq
                                   rook-k-sq opp-rkq-mask opp-rkk-mask opp-rk-q-sq opp-rk-k-sq]
                                  (if (= (get (:board state) TURN-STORE) WHITE)
                                    [ 3 11  7 0x04 0x00 0x07 14 13 0x70 0x77]
                                    [12 14 13 0x74 0x70 0x77 11 7 0x00 0x07])]
                              (cond (= (:from move) king-sq) (bit-and castling k-mask)
                                    (= (:from move) rook-q-sq) (bit-and castling qr-mask)
                                    (= (:from move) rook-k-sq) (bit-and castling kr-mask)
                                    (= (:to move) opp-rk-q-sq) (bit-and castling opp-rkq-mask)
                                    (= (:to move) opp-rk-k-sq) (bit-and castling opp-rkk-mask)
                                    :else castling))))))))

(defn- update-turn
  "Updates player turn value on board."
  [state]
  (when (not (nil? state))
    (assoc state :board
           (fill-square (:board state) TURN-STORE
                        (opponent (get (:board state) TURN-STORE))))))

(defn- update-en-passant
  "Associates new en-passant string with given STATE based on the MOVE."
  [move state]
  (when (not (nil? state))
    (assoc state :board
           (fill-square (:board state) EN-PASSANT-STORE
                        (let [piece (get (:board state) (:from move))]
                          (if (and (or (= piece WHITE-PAWN)
                                       (= piece BLACK-PAWN))
                                   (= (abs (- (:to move) (:from move))) 0x20))
                            (/ (+ (:to move) (:from move)) 2)
                            EMPTY))))))

(defn- pawn-or-capture-move?
  "Predicate to see if move was pawn move or a capture"
  [board move]
  (or (= (get board (:from move)) WHITE-PAWN)
      (= (get board (:from move)) BLACK-PAWN)
      (not (= (get board (:to move)) EMPTY))))

(defn- update-half-moves
  "Increases half move count on board unless the move
   was pawn or a capture move."
  [move state]
  (when (not (nil? state))
    (assoc state :board
           (fill-square (:board state) HALF-MOVE-STORE
                        (if (pawn-or-capture-move? (:board state) move)
                          0
                          (inc (get (:board state) HALF-MOVE-STORE)))))))

(defn- update-full-moves
  "Updates full move count on board.
   XXX: Byte can only handle 128 moves, add second square to hold
        a multiplier for full-moves like 3 x 125 for move 375."
  [state]
  (when (not (nil? state))
    (if (= (get (:board state) TURN-STORE) BLACK)
      (assoc state :board
             (fill-square (:board state) FULL-MOVE-STORE
                          (inc (get (:board state) FULL-MOVE-STORE))))
      state)))

(defn- update-move
  "Update the previous move of board.
   Stores previous move to 'off-board' locations"
  [move state]
  (when (not (nil? state))
    (assoc state :board
           (-> (:board state)
               (fill-square PREV-MOVE-FROM (:from move))
               (fill-square PREV-MOVE-TO (:to move))
               (fill-square PREV-PIECE (get (:board state) (:from move)))))))

(defn- update-check
  "Updates CHECK status bit on the state."
  [state]
  (when (not (nil? state))
    (assoc state :board
           (let [board (:board state)
                 player (get board TURN-STORE)]
             (fill-square board GAME-STATUS-STORE
                          (if (threaten-index? board
                                               (king-index state (opponent player))
                                               player)
                            CHECK-BIT
                            0))))))

(defn- update-state
  "Updates game state to reflect changes from move.
   If game state is not legal, will return a nil value."
  [old-state move]
  (->> old-state
       (update-move move)
       (update-board move)
       update-check
       update-turn
       (update-castling move)
       (update-en-passant move)
       (update-half-moves move)
       update-full-moves
       update-check))

(defrecord State0x88 [board black-pieces white-pieces]
  State
  (occupied? [state index]
    (board-occupied? (:board state) index))
  (black? [state index]
    (occupied-by? (:board state) index BLACK))
  (white? [state index]
    (occupied-by? (:board state) index WHITE))
  (check? [state]
    (= (get (:board state) GAME-STATUS-STORE) CHECK-BIT))
  (mate? [state]
    (and (check? state)
         (empty? (legal-states state))))
  (draw? [state]
    (or (fifty-move-rule? state)
        (fide-draw? state)
        (stalemate? state)))
  (state->fen [state]
    (parse-state state))
  (apply-move [state move]
    (when-let [new-state (update-state state move)]
      (when (not (check? new-state))
        new-state)))
  (legal-states [state]
    (->> state
         (pseudo-moves (get (:board state) TURN-STORE))
         (states state)))
  (turn [state]
    (if (= (get (:board state) TURN-STORE) WHITE) :white :black))
  (last-move [state]
    (let [board (:board state)
          prev-piece (get board PREV-PIECE)
          piece (get board (get board PREV-MOVE-TO))]
      (make-move (get board PREV-MOVE-FROM)
                 (get board PREV-MOVE-TO)
                 (if-not (= prev-piece piece)
                   piece
                   0))))
  (perft [state depth]
    (if (zero? depth)
      1
      (reduce + (map #(perft % (dec depth))
                     (legal-states state)))))
  (dynamic? [state]
    (= (get (:board state) DYNAMIC-STORE) 1))
  (evaluate [state]
    (heuristic-value state))
  (full-moves [state]
    (get (:board state) FULL-MOVE-STORE)))

(defn make-state
  [board blacks whites]
  (State0x88. board blacks whites))

(defn fen->state
  "Convert given FEN to state representation."
  [fen]
  (update-check (parse-fen fen (State0x88. nil nil nil))))
