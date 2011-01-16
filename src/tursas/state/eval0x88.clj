(ns tursas.state.eval0x88
  (:use (tursas state)
        (tursas.state common0x88)))

(def PAWN-VALUE 100)
(def BISHOP-VALUE 400)
(def KNIGHT-VALUE 400)
(def ROOK-VALUE 600)
(def QUEEN-VALUE 1200)
(def KING-VALUE 99999)

(def OPENING-GAME 0)
(def MIDDLE-GAME 1)
(def END-GAME 2)

(defn make-table
  "Utility to make full 0x88 vector out of smaller vector."
  [score-table]
  (into (vector-of :byte)
        (reduce concat (map #(concat % (vec (replicate 8 0)))
                            (partition 8 score-table)))))

;; Score tables for each piece type
;; all tables are from white players point of view.
(def pawn-scores [0   0   0   0   0   0   0   0
                  50  50  50  50  50  50  50  50
                  10  10  20  30  30  20  10  10
                  5   5   10  25  25  10  5   5
                  0   0   0   25  25  0   0   0
                  5  -5  -10  0   0  -10 -5   5
                  5   10  10 -25 -25  10  10  5
                  0   0   0   0   0   0   0   0])

(def rook-scores [ 50 -10  30  10  10  30 -10  50
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10   0   0   0   0   0   0 -10
                  -10   0   0   0   0   0   0 -10
                  -10   0   0   0   0   0   0 -10
                  -10   0   0   0   0   0   0 -10
                  -10   0   0   0   0   0   0 -10
                  -20 -10 -10 -10 -10 -10 -10 -20])

(def knight-scores [-50 -40 -20 -30 -30 -20 -40 -50
                    -40 -20   0   5   5   0 -20 -40
                    -30   5  10  15  15  10   5 -30
                    -30   0  15  20  20  15   0 -30
                    -30   0  15  20  20  15   0 -30
                    -30   5  10  15  15  10   5 -30
                    -40 -20   0   5   5   0 -20 -40
                    -50 -40 -20 -30 -30 -20 -40 -50])

(def bishop-scores [-20 -10 -10 -10 -10 -10 -10 -20
                    -10   0   0   0   0   0   0 -10
                    -10   0   5  10  10   5   0 -10
                    -10   5   5  10  10   5   5 -10
                    -10   0  10  10  10  10   0 -10
                    -10  10  10  10  10  10  10 -10
                    -10   5   0   0   0   0   5 -10
                    -20 -10 -40 -10 -10 -40 -10 -20])

(def king-scores [-30 -40 -40 -50 -50 -40 -40 -30
                  -30 -40 -40 -50 -50 -40 -40 -30
                  -30 -40 -40 -50 -50 -40 -40 -30
                  -30 -40 -40 -50 -50 -40 -40 -30
                  -20 -30 -30 -40 -40 -30 -30 -20
                  -10 -20 -20 -20 -20 -20 -20 -10
                  20  20   0   0   0   0  20  20
                  20  30  10   0   0  10  30  20])

(def king-end-scores [-50 -40 -30 -20 -20 -30 -40 -50
                      -30 -20 -10   0   0 -10 -20 -30
                      -30 -10  20  30  30  20 -10 -30
                      -30 -10  30  40  40  30 -10 -30
                      -30 -10  30  40  40  30 -10 -30
                      -30 -10  20  30  30  20 -10 -30
                      -30 -30   0   0   0   0 -30 -30
                      -50 -30 -30 -30 -30 -30 -30 -50])

(def white-pawn-table
     (make-table pawn-scores))

(def black-pawn-table
     (make-table (reverse pawn-scores)))

(def white-knight-table
     (make-table knight-scores))

(def black-knight-table
     (make-table (reverse knight-scores)))

(def white-bishop-table
     (make-table bishop-scores))

(def black-bishop-table
     (make-table (reverse bishop-scores)))

(def white-rook-table
     (make-table rook-scores))

(def black-rook-table
     (make-table (reverse rook-scores)))

(def white-king-table
     (make-table king-scores))

(def black-king-table
     (make-table (reverse king-scores)))

(def white-king-table-end-game
     (make-table king-end-scores))

(def black-king-table-end-game
     (make-table (reverse king-end-scores)))

(defn- material-value
  "Gives material value for PIECE."
  [piece]
  (cond (== piece WHITE-PAWN) PAWN-VALUE
        (== piece WHITE-KNIGHT) KNIGHT-VALUE
        (== piece WHITE-BISHOP) BISHOP-VALUE
        (== piece WHITE-ROOK) ROOK-VALUE
        (== piece WHITE-QUEEN) QUEEN-VALUE
        (== piece WHITE-KING) KING-VALUE
        (== piece BLACK-PAWN) PAWN-VALUE
        (== piece BLACK-KNIGHT) KNIGHT-VALUE
        (== piece BLACK-BISHOP) BISHOP-VALUE
        (== piece BLACK-ROOK) ROOK-VALUE
        (== piece BLACK-QUEEN) QUEEN-VALUE
        (== piece BLACK-KING) KING-VALUE
        :else 0))

(defn- index-score
  "Checks piece-specific index score"
  [piece index game-situation]
  (cond (== piece WHITE-PAWN) (get white-pawn-table index)
        (== piece BLACK-PAWN) (get black-pawn-table index)
        (== piece WHITE-KNIGHT) (get white-knight-table index)
        (== piece BLACK-KNIGHT) (get black-knight-table index)
        (== piece WHITE-BISHOP) (get white-bishop-table index)
        (== piece BLACK-BISHOP) (get black-bishop-table index)
        (== piece WHITE-ROOK) (get white-rook-table index)
        (== piece BLACK-ROOK) (get black-rook-table index)
        (== piece WHITE-KING) (if (= game-situation :end-game)
                                (get white-king-table-end-game index)
                                (get white-king-table index))
        (== piece BLACK-KING) (if (= game-situation :end-game)
                                (get black-king-table-end-game index)
                                (get black-king-table index))
        :else 0))

(defn- check-situation
  "Checks which situation, opening, middle or end-game the game is."
  [state]
  (let [pieces (merge (:white-pieces state)
                      (:black-pieces state))]
    (cond (< (count (keys pieces)) 15) END-GAME
          (> (get (:board state) FULL-MOVE-STORE) 10) MIDDLE-GAME
          :else OPENING-GAME)))

(defn- score
  "Calculates score for side."
  [state pieces situation]
  (reduce (fn [score [index piece]]
            (+ score
               (material-value piece)
               (index-score piece index situation)))
          0 (seq pieces)))

(defn heuristic-value
  "Calculates heuristic value for given state."
  [state]
  (let [situation (check-situation state)
        pieces (if (= (:turn state) WHITE)
                 (list (:white-pieces state) (:black-pieces state))
                 (list (:black-pieces state) (:white-pieces state)))]
    (+ (score state (first pieces) situation)
       (- (score state (second pieces) situation))
       (cond (draw? state) -1000
             (check? state) -2500
             (mate? state) (- KING-VALUE)
             :else 0))))

