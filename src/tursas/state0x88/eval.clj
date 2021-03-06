(ns tursas.state0x88.eval
  (:use (tursas state)
        (tursas.state0x88 common util)))

(def pawn-value 10)
(def bishop-value 30)
(def knight-value 30)
(def rook-value 50)
(def queen-value 90)
(def king-value 99999)

;; Score tables for each piece type
;; all tables are from white players point of view.
(def pawn-scores [0   0   0   0   0   0   0   0
                  5   5   5   0   0   5   5   5
                  0   0   5  15  15   5   0   0
                  5   5  10  15  15  10   5   5
                  0   0   5  10  10   5   0   0
                  0   0   0   0   0   0   0   0
                  0   0   0   0   0   0   0   0
                  0   0   0   0   0   0   0   0])

(def rook-scores [5  0  5  0  0  5  0  5
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0
                  0  0  0  0  0  0  0  0 ])

(def rook-end-scores [0  0  0  0  0  0  0  0
                      0  0  0  0  0  0  0  0
                      0  0  0  0  0  0  0  0
                      0  0  0  0  0  0  0  0
                      0  0  0  0  0  0  0  0
                      0  0  0  0  0  0  0  0
                      0  0  0  0  0  0  0  0
                      0  0  0  0  0  0  0  0 ])

(def knight-scores [-10 -5 -5  -5  -5  -5  -5 -10
                    -5  -5  0   5   5   0  -5 -5
                    -5   5  5   10  10  5   5 -5
                    -5   0  10  10  10  10  0 -5
                    -5   0  10  10  10  10  0 -5
                    -5   5  5   10  10  10  5 -5
                    -5  -5  0   5   5   0  -5 -5
                    -10 -5  -5  -5  -5  -5 -5 -10])

(def bishop-scores [-15 -10 -10 -10 -10 -10 -10 -15
                    -10   0   0   0   0   0   0 -10
                    -10   0   5  10  10   5   0 -10
                    -10   5   5  10  10   5   5 -10
                    -10   0  10  10  10  10   0 -10
                    -10  10  10  10  10  10  10 -10
                    -10   5   0   0   0   0   5 -10
                    -15 -10 -10 -10 -10 -10 -10 -15])

(def king-scores [  0 -5    5   0   0  -5   5   0
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10
                  -10 -10 -10 -10 -10 -10 -10 -10])

(def king-end-scores [-15 -10 -10 -10 -10 -10 -10 -15
                      -10 -10  -5   0   0  -5 -10 -10
                      -10  -5   5  10  10   5  -5 -10
                      -10  -5  10  15  15  10  -5 -10
                      -10  -5  10  15  15  10  -5 -10
                      -10  -5   5  10  10   5  -5 -10
                      -10 -10  -5   0   0  -5 -10 -10
                      -15 -10 -10 -10 -10 -10 -10 -15])

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
  "Gives material value for given piece."
  [piece]
  (cond (== piece white-pawn) pawn-value
        (== piece white-knight) knight-value
        (== piece white-bishop) bishop-value
        (== piece white-rook) rook-value
        (== piece white-queen) queen-value
        (== piece white-king) king-value
        (== piece black-pawn) pawn-value
        (== piece black-knight) knight-value
        (== piece black-bishop) bishop-value
        (== piece black-rook) rook-value
        (== piece black-queen) queen-value
        (== piece black-king) king-value
        :else 0))

(defn- index-score
  "Checks piece-specific index score"
  [piece index game-situation]
  (cond (== piece white-pawn) (get white-pawn-table index)
        (== piece black-pawn) (get black-pawn-table index)
        (== piece white-knight) (get white-knight-table index)
        (== piece black-knight) (get black-knight-table index)
        (== piece white-bishop) (get white-bishop-table index)
        (== piece black-bishop) (get black-bishop-table index)
        (== piece white-rook) (get white-rook-table index)
        (== piece black-rook) (get black-rook-table index)
        (== piece white-king) (if (== game-situation end-game)
                                (get white-king-table-end-game index)
                                (get white-king-table index))
        (== piece black-king) (if (== game-situation end-game)
                                (get black-king-table-end-game index)
                                (get black-king-table index))
        :else 0))

(defn- pawn-shield-bonus
  "Returns pawn-shield bonus to score if king is castled and pawns protect it."
  [state]
  0)

(defn- bishop-pair-bonus
  "Grants small bonus if both bishops are present."
  [state]
  0)

(defn- bishop-mobility-bonus
  "When number of pawns is reduced, grant small bonus for each bishop.
   They get more useful once there's more room to move."
  [state]
  0)

(defn- knight-mobility-penalty
  "When number of pawns decreases, the effectiviness of knights is reduced.
   Reduce the value of knights at this point to reflect the fact."
  [state]
  0)

(defn- early-queen-penalty
  "Give small penalty when if queen is moved early in the game.
   It makes it vurnerable to enemy captures."
  [state]
  0)

(defn- mobility-bonus
  "Give small bonus to pieces if it can move more."
  [state]
  0)

(defn- threat-bonus
  "Give small bonus to each piece which is threatening enemy piece."
  [state]
  0)

(defn- protection-bonus
  "Give small bonus to pieces which protect some other piece."
  [state]
  0)

(defn- pawn-color-penalty
  "Give small penalty if there is a lot of pawns
   in same colored squares as the players bishop."
  [state]
  0)

(defn- pawn-advanced-bonus
  "Give bonus for advancing pawns so they will get promoted to other pieces."
  [state]
  0)

(defn- score
  "Calculates score for side."
  [pieces situation]
  (reduce (fn [score [index piece]]
            (+ score
               (material-value piece)
               (index-score piece index situation)))
          0 (seq pieces)))

(defn heuristic-value
  "Calculates heuristic value for given state."
  [player whites blacks situation]
  (let [pieces (if (== player white)
                 (list whites blacks)
                 (list blacks whites))]
    (+ (score (first pieces) situation)
       (- (score (second pieces) situation)))))

(defn end-score [state]
  (if (mate? state)
    (- king-value)
    0))
