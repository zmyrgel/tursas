(ns tursas.state.eval0x88
  (:use (tursas state)
        (tursas.state common0x88)))

(def PAWN-VALUE 100)
(def BISHOP-VALUE 400)
(def KNIGHT-VALUE 400)
(def ROOK-VALUE 600)
(def QUEEN-VALUE 1200)
(def KING-VALUE 10000)

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

(defn- material-value
  "Gives material value for PIECE."
  [piece]
  (cond (= piece WHITE-PAWN) PAWN-VALUE
        (= piece WHITE-KNIGHT) KNIGHT-VALUE
        (= piece WHITE-BISHOP) BISHOP-VALUE
        (= piece WHITE-ROOK) ROOK-VALUE
        (= piece WHITE-QUEEN) QUEEN-VALUE
        (= piece WHITE-KING) KING-VALUE
        (= piece BLACK-PAWN) (- PAWN-VALUE)
        (= piece BLACK-KNIGHT) (- KNIGHT-VALUE)
        (= piece BLACK-BISHOP) (- BISHOP-VALUE)
        (= piece BLACK-ROOK) (- ROOK-VALUE)
        (= piece BLACK-QUEEN) (- QUEEN-VALUE)
        (= piece BLACK-KING) (- KING-VALUE)
        :else 0))

(defn- index-score
  "Checks piece-specific index score"
  [piece index game-situation]
  (cond (= piece WHITE-PAWN) (get white-pawn-table index)
        (= piece BLACK-PAWN) (get black-pawn-table index)
        (or (= piece WHITE-KNIGHT)
            (= piece BLACK-KNIGHT)) (get knight-table index)
            (= piece WHITE-BISHOP) (get white-bishop-table index)
            (= piece BLACK-BISHOP) (get black-bishop-table index)
            (= piece WHITE-KING) (if (= game-situation :end-game)
                                   (get white-king-table-end-game index)
                                   (get white-king-table index))
            (= piece BLACK-KING) (if (= game-situation :end-game)
                                   (get black-king-table-end-game index)
                                   (get black-king-table index))
            :else 0))

(defn- check-situation
  "Checks which situation, opening, middle or end-game the game is."
  [state pieces]
  (cond (< (count (keys pieces)) 15) :end-game
        (> (get (:board state) FULL-MOVE-STORE) 10) :middle-game
        :else :opening-game))

(defn heuristic-value
  "Calculates heuristic value for given state."
  [state]
  (let [pieces (merge (:white-pieces state)
                      (:black-pieces state))]
    (reduce + (map #(material-value %) (vals pieces)))))
