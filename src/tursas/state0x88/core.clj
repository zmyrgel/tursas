(ns tursas.state0x88.core
  (:use (tursas state move util)
        (tursas.state0x88 eval movegen util common fen move)
        [clojure.contrib.math :only [abs]]))

(defn- calculate-white-castling
  "Utility to calculate new castling value for white player.
   Castling value is updated if either our king or rook moves
   or opponents rook gets captured.
   Castling value is kept as single digit and is operated at bit level.
   Castling value is composesd as: K = 8, Q = 4, k = 2, q = 1"
  [castling move]
  (cond (== (:from move) 0x04) (bit-and castling 3)
        (== (:from move) 0x00) (bit-and castling 11)
        (== (:from move) 0x07) (bit-and castling 7)
        (== (:to move) 0x70) (bit-and castling 14)
        (== (:to move) 0x77) (bit-and castling 13)
        :else castling))

(defn- calculate-black-castling
  "Utility to calculate new castling value for black player.
   Castling value is updated if either our king or rook moves
   or opponents rook gets captured.
   Castling value is kept as single digit and is operated at bit level.
   Castling value is composesd as: K = 8, Q = 4, k = 2, q = 1"
  [castling move]
  (cond (== (:from move) 0x74) (bit-and castling 12)
        (== (:from move) 0x70) (bit-and castling 14)
        (== (:from move) 0x77) (bit-and castling 13)
        (== (:to move) 0x00) (bit-and castling 11)
        (== (:to move) 0x07) (bit-and castling 7)
        :else castling))

(defn- calculate-en-passant
  "Utility to calculate new en-passant value.
   If pawn moves two steps next to opponents pawn, place en-passant
   value as board index just behind moved pawn, otherwise -1."
  [player piece west-piece east-piece move]
  (let [opp-pawn (if (== player WHITE) BLACK-PAWN WHITE-PAWN)]
    (if (and (or (== piece WHITE-PAWN)
                 (== piece BLACK-PAWN))
             (== (abs (- (:to move) (:from move))) 0x20)
             (or (== opp-pawn west-piece)
                 (== opp-pawn east-piece)))
      (/ (+ (:to move) (:from move)) 2)
      -1)))

(defn- inc-full-moves
  "Utility to increase full moves on the board.
   Uses two vector indexes because of the limitation of byte value.
   If full moves get to 127 increase multiplier store and reduce full move
   store to 0. This gets full move count to get high enough."
  [board]
  (let [moves (get board FULL-MOVE-STORE)
        n-moves (get board FULL-MOVE-N-STORE)]
    (if (== moves 127)
      (-> board
          (fill-square FULL-MOVE-N-STORE (inc n-moves))
          (fill-square FULL-MOVE-STORE 0))
      (fill-square board FULL-MOVE-STORE (inc moves)))))

(defn- promotion-piece
  "Helper function to return promotion piece value.
    Reads the promotion piece value from move, defaults to queen."
  [player move]
  (let [piece (:promotion move)]
    (if (zero? piece)
      (if (== player WHITE)
        WHITE-QUEEN BLACK-QUEEN)
      (if (== player WHITE)
        (- piece) piece))))

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

(defn- update-board
  "Returns state with new board after applying move to state."
  [state move]
  (when-not (nil? state)
    (let [player (int (get (:board state) TURN-STORE))
          moving-piece (int (get (:board state) (:from move)))]
      (cond (promotion? moving-piece move) (-> (remove-piece state (:from move))
                                               (add-piece (:to move) (promotion-piece player move)))
            (castling? moving-piece move) (move-castling-pieces player state move
                                                                (if (== (column (:to move)) 2)
                                                                  QUEEN-SIDE KING-SIDE))
            (en-passant? (:board state) moving-piece move) (-> (move-piece state move)
                                                               (remove-piece (+ (:to move)
                                                                                (if (== player WHITE)
                                                                                  SOUTH NORTH))))
            :else (move-piece state move)))))

(defn- update-player-check
  "Checks that players move won't leave the players king in check."
  [state]
  (when-not (nil? state)
    (let [board (:board state)
          player (get board TURN-STORE)]
      (when-not (threatened? board (king-index board player) (opponent player))
        state))))

(defn- update-castling
  "Updates states castling value by checking move with current castling value."
  [state move]
  (when-not (nil? state)
    (let [castling (int (get (:board state) CASTLING-STORE))]
      (if (zero? castling)
        state
        (assoc state :board
               (fill-square (:board state) CASTLING-STORE
                            (if (== (get (:board state) TURN-STORE) WHITE)
                              (calculate-white-castling castling move)
                              (calculate-black-castling castling move))))))))

(defn- update-en-passant
  "Associates new en-passant value with given state based on the move."
  [state move]
  (when-not (nil? state)
    (assoc state :board
           (fill-square (:board state) EN-PASSANT-STORE
                        (calculate-en-passant (get (:board state) TURN-STORE)
                                              (get (:board state) (:to move) 0)
                                              (get (:board state) (+ (:to move) WEST) 0)
                                              (get (:board state) (+ (:to move) EAST) 0)
                                              move)))))

(defn- update-full-moves
  "Updates full move count on board."
  [state]
  (when-not (nil? state)
    (if (== (get (:board state) TURN-STORE) BLACK)
      (assoc state :board (inc-full-moves (:board state)))
      state)))

(defn- update-opponent-check
  "Updates opponents check status bit on the state.
   Enables check bit in state if opponents king is threatened."
  [state]
  (when-not (nil? state)
    (let [board (:board state)
          player (get board TURN-STORE)]
      (assoc state :board
             (fill-square board CHECK-STORE
                          (if (threatened? board
                                           (king-index board (opponent player))
                                           player)
                            1
                            0))))))

(defn- update-turn
  "Updates player turn value on board."
  [state]
  (when-not (nil? state)
    (assoc state :board
           (fill-square (:board state) TURN-STORE
                        (opponent (get (:board state) TURN-STORE))))))

(defn- update-state
  "Updates game state to reflect changes from move.
   If game state is not legal, will return a nil value."
  [state move]
  (when-not (nil? state)
    (-> state
        (update-move move)
        (update-half-moves move)
        (update-board move)
        update-player-check
        (update-castling move)
        (update-en-passant move)
        update-full-moves
        update-opponent-check
        update-turn)))

(defn- check-situation
  "Checks which situation, opening, middle or end-game the game is."
  [state]
  (let [pieces (merge (:white-pieces state)
                      (:black-pieces state))]
    (cond (< (count (keys pieces)) 15) END-GAME
          (> (get (:board state) FULL-MOVE-STORE) 10) MIDDLE-GAME
          :else OPENING-GAME)))

(defrecord State0x88 [board black-pieces white-pieces]
  State
  (occupied? [state index]
    (board-occupied? (:board state) index))
  (black? [state index]
    (occupied-by? (:board state) index BLACK))
  (white? [state index]
    (occupied-by? (:board state) index WHITE))
  (check? [state]
    (== (get (:board state) CHECK-STORE) 1))
  (mate? [state]
    (and (check? state)
         (empty? (legal-states state))))
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
                          "1-0 {White mates}")))
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
    (heuristic-value (get (:board state) TURN-STORE)
                     (:white-pieces state)
                     (:black-pieces state)
                     (check-situation state)))
  (full-moves [state]
    (get (:board state) FULL-MOVE-STORE))
  (game-end? [state]
    (or (draw? state)
        (mate? state)))
  (game-score [state]
    (end-score state)))

(defn fen->state
  "Convert given FEN to state representation."
  [fen]
  (update-opponent-check (parse-fen fen (State0x88. nil nil nil))))
