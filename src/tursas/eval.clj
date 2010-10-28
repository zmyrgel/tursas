(ns tursas.eval
  (:use (tursas state)))

;; piece material values
(def PAWN-VALUE 10)
(def BISHOP-VALUE 30)
(def KNIGHT-VALUE 30)
(def ROOK-VALUE 50)
(def QUEEN-VALUE 90)
(def KING-VALUE 9999)

(def white-pawn-table [0   0   0   0   0   0   0   0
                       50  50  50  50  50  50  50  50
                       10  10  20  30  30  20  10  10
                       5   5   10  27  27  10  5   5
                       0   0   0   25  25  0   0   0
                       5  -5  -10  0   0  -10 -5   5
                       5   10  10 -25 -25  10  10  5
                       0   0   0   0   0   0   0   0])
(def black-pawn-table (reverse white-pawn-table))

(def knight-table [-50 -40 -20 -30 -30 -20 -40 -50
                   -40 -20   0   5   5   0 -20 -40
                   -30   5  10  15  15  10   5 -30
                   -30   0  15  20  20  15   0 -30
                   -30   0  15  20  20  15   0 -30
                   -30   5  10  15  15  10   5 -30
                   -40 -20   0   5   5   0 -20 -40
                   -50 -40 -20 -30 -30 -20 -40 -50])

(def white-bishop-table [-20 -10 -10 -10 -10 -10 -10 -20
                         -10   0   0   0   0   0   0 -10
                         -10   0   5  10  10   5   0 -10
                         -10   5   5  10  10   5   5 -10
                         -10   0  10  10  10  10   0 -10
                         -10  10  10  10  10  10  10 -10
                         -10   5   0   0   0   0   5 -10
                         -20 -10 -40 -10 -10 -40 -10 -20])
(def black-bishop-table (reverse white-bishop-table))

(def white-king-table [-30  -40  -40  -50  -50  -40  -40  -30
                       -30  -40  -40  -50  -50  -40  -40  -30
                       -30  -40  -40  -50  -50  -40  -40  -30
                       -30  -40  -40  -50  -50  -40  -40  -30
                       -20  -30  -30  -40  -40  -30  -30  -20
                       -10  -20  -20  -20  -20  -20  -20  -10
                       20   20    0    0    0    0   20   20
                       20   30   10    0    0   10   30   20])
(def black-king-table (reverse white-king-table))

(def white-king-table-end-game [-50 -40 -30 -20 -20 -30 -40 -50
                                -30 -20 -10   0   0 -10 -20 -30
                                -30 -10  20  30  30  20 -10 -30
                                -30 -10  30  40  40  30 -10 -30
                                -30 -10  30  40  40  30 -10 -30
                                -30 -10  20  30  30  20 -10 -30
                                -30 -30   0   0   0   0 -30 -30
                                -50 -30 -30 -30 -30 -30 -30 -50])
(def black-king-table-end-game (reverse white-king-table-end-game))

(defn piece-value->material-value
  "Gives material value for PIECE."
  [piece]
  (case piece
        \p (- PAWN-VALUE)
        \n (- KNIGHT-VALUE)
        \b (- BISHOP-VALUE)
        \r (- ROOK-VALUE)
        \q (- QUEEN-VALUE)
        \k (- KING-VALUE)
        \P PAWN-VALUE
        \N KNIGHT-VALUE
        \B BISHOP-VALUE
        \R ROOK-VALUE
        \Q QUEEN-VALUE
        \K KING-VALUE
        0))

(defn piece-index-score
  "Checks piece-specific index score"
  [piece index game-situation]
  (case piece
        \P (get white-pawn-table index)
        \p (get black-pawn-table index)
        \N (get knight-table index)
        \n (get knight-table index)
        \B (get white-bishop-table index)
        \b (get black-bishop-table index)
        \K (if (= game-situation :end-game)
             (get white-king-table-end-game index)
             (get white-king-table index))
        \k (if (= game-situation :end-game)
             (get black-king-table-end-game index)
             (get black-king-table index))
        0))

(defn check-situation
  "Checks which situation, opening, middle or end-game the game is."
  [state pieces]
  (cond
   (< (count (get-pieces state)) 15)
   :end-game
   (> (:full-moves state) 10)
   :middle-game
   :else :opening-game))

(defn evaluate-state
  "Evaluates given game STATE.
   Simply calculates the material balance of the board."
  [state]
  (let [pieces (get-pieces state)]
  (reduce #(+ (piece-value->material-value (get pieces %))
              (piece-index-score (get pieces %) % (check-situation state pieces)))
          (keys pieces))))



