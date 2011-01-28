(ns tursas.state0x88.core
  (:use (tursas state move util)
        (tursas.state0x88 eval movegen util common fen move)
        [clojure.contrib.math :only [abs]]))

(defn- fifty-move-rule?
  "Checks if state is draw according to 50-move rule."
  [state]
  (>= (get (:board state) HALF-MOVE-STORE) 50))

(defn- stalemate?
  "Check if given state is in stalemate."
  [state]
  (and (not (check? state))
       (empty? (legal-moves state))))

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
         (or (== piece-count 2)
             (and (== piece-count 3)
                  (any? #(or (== BLACK-KNIGHT %)
                             (== BLACK-BISHOP %)
                             (== WHITE-KNIGHT %)
                             (== WHITE-BISHOP %))
                        vals))
             (and (== piece-count 4)
                  (or (== (count (filter #(= BLACK-KNIGHT %) vals)) 2)
                      (== (count (filter #(= WHITE-KNIGHT %) vals)) 2)
                      (let [bishops (filter #(or (= BLACK-BISHOP (get pieces %))
                                                 (= WHITE-BISHOP (get pieces %)))
                                            keys)]
                        (cond (< (count bishops) 2) false
                              :else (same-color? (first (keys bishops))
                                                 (second (keys bishops)))))))))))

(defn- repetition?
  "Predicate to see if game is draw by repetition.
   Stub function to be filled later."
  [state]
  false)

(defn- update-board
  "Returns state with new board after applying move to state."
  [state move]
  (when-not (nil? state)
    (let [player (int (get (:board state) TURN-STORE))
          moving-piece (int (get (:board state) (:from move)))]
      (cond (promotion? moving-piece move) (-> (move-piece state move)
                                               (promote-piece player move))
            (castling? moving-piece move) (move-castling-pieces player state move
                                                                (if (== (column (:to move)) 2)
                                                                  QUEEN-SIDE KING-SIDE))
            :else (move-piece state move)))))

(defn- update-castling
  "Updates states castling value for move
   checks for king or rook moves."
  [state move]
  (when-not (nil? state)
    (let [castling (byte (get (:board state) CASTLING-STORE))]
      (if (zero? castling)
        state
        (assoc state :board
               (fill-square (:board state) CASTLING-STORE
                            (let [[k-mask qr-mask kr-mask king-sq rook-q-sq
                                   rook-k-sq opp-rkq-mask opp-rkk-mask opp-rk-q-sq opp-rk-k-sq]
                                  (if (== (get (:board state) TURN-STORE) WHITE)
                                    [ 3 11  7 0x04 0x00 0x07 14 13 0x70 0x77]
                                    [12 14 13 0x74 0x70 0x77 11 7 0x00 0x07])]
                              (cond (== (:from move) king-sq) (bit-and castling k-mask)
                                    (== (:from move) rook-q-sq) (bit-and castling qr-mask)
                                    (== (:from move) rook-k-sq) (bit-and castling kr-mask)
                                    (== (:to move) opp-rk-q-sq) (bit-and castling opp-rkq-mask)
                                    (== (:to move) opp-rk-k-sq) (bit-and castling opp-rkk-mask)
                                    :else castling))))))))

(defn- update-turn
  "Updates player turn value on board."
  [state]
  (when-not (nil? state)
    (assoc state :board
           (fill-square (:board state) TURN-STORE
                        (opponent (get (:board state) TURN-STORE))))))

(defn- update-en-passant
  "Associates new en-passant value with given state based on the move.
   If pawn moves two steps next to opponents pawn, place en-passant
   value as board index just behind moved pawn, otherwise -1."
  [state move]
  (when-not (nil? state)
    (assoc state :board
           (fill-square (:board state) EN-PASSANT-STORE
                        (let [piece (int (get (:board state) (:from move)))
                              opp-pawn (if (== (get (:board state) TURN-STORE) WHITE)
                                         BLACK-PAWN
                                         WHITE-PAWN)]
                          (if (and (or (== piece WHITE-PAWN)
                                       (== piece BLACK-PAWN))
                                   (== (abs (- (:to move) (:from move))) 0x20)
                                   (or (== opp-pawn (int (get (:board state)
                                                              (+ (:to move) WEST))))
                                       (== opp-pawn (int (get (:board state)
                                                              (+ (:to move) EAST))))))
                            (/ (+ (:to move)
                                  (:from move))
                               2)
                            -1))))))

(defn- pawn-or-capture-move?
  "Predicate to see if move was pawn move or a capture"
  [board move]
  (or (== (get board (:from move)) WHITE-PAWN)
      (== (get board (:from move)) BLACK-PAWN)
      (not= (get board (:to move)) EMPTY)))

(defn- update-half-moves
  "Increases half move count on board unless the move
   was pawn or a capture move."
  [state move]
  (when-not (nil? state)
    (assoc state :board
           (fill-square (:board state) HALF-MOVE-STORE
                        (if (pawn-or-capture-move? (:board state) move)
                          0
                          (inc (get (:board state) HALF-MOVE-STORE)))))))

(defn- update-full-moves
  "Updates full move count on board."
  [state]
  (when-not (nil? state)
    (if (== (get (:board state) TURN-STORE) BLACK)
      (assoc state :board
             (let [moves (get (:board state) FULL-MOVE-STORE)
                   n-moves (get (:board state) FULL-MOVE-N-STORE)]
               (if (== moves 127)
                 (-> (:board state)
                     (fill-square FULL-MOVE-N-STORE (inc n-moves))
                     (fill-square FULL-MOVE-STORE 0))
                 (fill-square (:board state) FULL-MOVE-STORE
                              (inc moves)))))
      state)))

(defn- update-move
  "Update the previous move of board.
   Stores previous move to 'off-board' locations"
  [state move]
  (when-not (nil? state)
    (assoc state :board
           (-> (:board state)
               (fill-square PREV-MOVE-FROM (:from move))
               (fill-square PREV-MOVE-TO (:to move))
               (fill-square PREV-PIECE (get (:board state) (:from move)))))))

(defn- update-check
  "Updates check status bit on the state.
   Check for check condition by testing if players king
   is threatened by the last player."
  [state]
  (when-not (nil? state)
    (let [player (get (:board state) TURN-STORE)
          check-store (if (== player WHITE)
                        WHITE-CHECK-STORE
                        BLACK-CHECK-STORE)
          prev-check (int (get (:board state) check-store))
          in-check? (threatened? (:board state)
                                 (king-index state player)
                                 (opponent player))]
      (when-not (and (== prev-check 1) in-check?)
        (assoc state :board
               (fill-square (:board state) check-store
                            (if in-check?
                              1
                              0)))))))

(defn- update-state
  "Updates game state to reflect changes from move.
   If game state is not legal, will return a nil value."
  [state move]
  (when-not (nil? state)
    (-> state
        (update-move move)
        (update-half-moves move)
        (update-board move)
        (update-castling move)
        (update-en-passant move)
        update-full-moves
        update-check
        update-turn)))

(defrecord State0x88 [board black-pieces white-pieces]
  State
  (occupied? [state index]
    (board-occupied? (:board state) index))
  (black? [state index]
    (occupied-by? (:board state) index BLACK))
  (white? [state index]
    (occupied-by? (:board state) index WHITE))
  (check? [state]
    (let [store (if (== (int (get (:board state) TURN-STORE)) WHITE)
                  WHITE-CHECK-STORE BLACK-CHECK-STORE)]
      (== (get (:board state) store) 1)))
  (mate? [state]
    (and (check? state)
         (empty? (legal-moves state))))
  (draw? [state]
    (or (fifty-move-rule? state)
        (fide-draw? state)
        (stalemate? state)
        (repetition? state)))
  (result [state]
    (cond (fifty-move-rule? state) "1/2-1/2 {50-move rule}"
          (fide-draw? state) "1/2-1/2 {Draw per FIDE rules}"
          (stalemate? state) "1/2-1/2 {Stalemate}"
          (repetition? state) "1/2-1/2 {Draw by repetition}"
          (mate? state) (if (== (int (get (:board state) TURN-STORE)) WHITE)
                          "0-1 {Black mates}"
                          "1-1 {White mates}")
          :else nil))
  (state->fen [state]
    (parse-state state))
  (apply-move [state move]
    (when (allowed-move? state move)
      (when-let [new-state (update-state state move)]
        new-state)))
  (legal-states [state]
    (filter #(not (nil? %))
            (map (partial apply-move state)
                 (pseudo-moves (get (:board state) TURN-STORE) state))))
  (legal-moves [state]
    (map last-move (legal-states state)))
  (turn [state]
    (if (== (get (:board state) TURN-STORE) WHITE)
      :white
      :black))
  (last-move [state]
    (let [board (:board state)
          prev-piece (int (get board PREV-PIECE))
          piece (int (get board (int (get board PREV-MOVE-TO))))]
      (make-move (get board PREV-MOVE-FROM)
                 (get board PREV-MOVE-TO)
                 (if-not (== prev-piece piece)
                   piece
                   0))))
  (perft [state depth]
    (if (zero? depth)
      1
      (apply + (map #(perft % (dec depth))
                    (legal-states state)))))
  (dynamic? [state]
    (== (get (:board state) DYNAMIC-STORE) 1))
  (evaluate [state]
    (heuristic-value state))
  (full-moves [state]
    (int (get (:board state) FULL-MOVE-STORE)))
  (game-end? [state]
    (or (draw? state)
        (mate? state)))
  (game-score [state]
    (end-score state)))

(defn fen->state
  "Convert given FEN to state representation."
  [fen]
  (update-check (parse-fen fen (State0x88. nil nil nil))))
