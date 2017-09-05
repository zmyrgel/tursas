(ns tursas.state0x88.fen
  (:use (tursas.state0x88 common util movegen move)
        (tursas util)))

(def castling-values (list [8 \K] [4 \Q] [2 \k] [1 \q]))

(defn- castling->str
  "Converts internal castling representation to string."
  [castling]
  (let [result (keep (fn [[value letter]]
                       (when (pos? (bit-and castling value))
                         letter))
                     castling-values)]
    (if (empty? result)
      "-"
      (apply str result))))

(defn- castling->value
  "Convers string representing castling to
   internal castling value."
  [s]
  (reduce (fn [result [value letter]]
            (if (some #(= % letter) s)
              (+ result value)
              result))
          0 castling-values))

(defn- find-king-index
  "Seeks king's index from piece-map.
   This is only used when generating state from a fen.
   Otherwise the king index can be queried from the board directly."
  [state player]
  (let [piece-map (if (== player white)
                    (:white-pieces state)
                    (:black-pieces state))
        king (if (== player white)
               white-king
               black-king)]
    (loop [pieces (seq piece-map)]
      (cond (empty? pieces) nil
            (== (second (first pieces)) king) (ffirst pieces)
            :else (recur (rest pieces))))))

(defn- fen-board->0x88board
  "Converts FEN notation string to 0x88 board representation."
  [s]
  (reduce (fn [board [index piece]]
            (fill-square board index (piece-value piece)))
          (init-game-board)
          (seq (zipmap (iterate inc 0)
                       (mapcat #(concat % (repeat 8 \E))
                               (->> (expand-digits \E s)
                                    (split-on \/)
                                    reverse))))))

(defn- make-fen-row
  "Builds single fen row from given board and row index."
  [board row]
  (compact-item \E (map #(piece-name (get board (+ row %)))
                        (range 8))))

(defn- board->fen-board
  "Convert the state internal board to fen board representation."
  [board]
  (apply str
         (reduce #(concat %1 '(\/) %2)
                 (map #(make-fen-row board %)
                      (reverse (range 0 0x77 0x10))))))

(defn- add-pieces
  "Adds all pieces from board to piece-map."
  [state]
  (letfn [(add-index-if [pred piece-map index]
                     (if (pred (get (:board state) index))
                       (assoc piece-map index (get (:board state) index))
                       piece-map))]
    (loop [index 0x77
           blacks {}
           whites {}]
      (cond (= index -1) (-> state
                             (assoc :white-pieces whites)
                             (assoc :black-pieces blacks))
            (not (board-index? index)) (recur (dec index) blacks whites)
            :else (recur (dec index)
                         (add-index-if black-piece? blacks index)
                         (add-index-if white-piece? whites index))))))

(defn- add-king-indexes
  "Adds king indexes to state."
  [state]
  (assoc state :board
         (let [black-king (find-king-index state black)
               white-king (find-king-index state white)]
           (-> (:board state)
               (update-king-index black-king black)
               (update-king-index white-king white)))))

(defn- add-full-moves
  "Helper funtion to add full moves to board.
   Needed to workaround the byte limitation of the board."
  [board moves]
  (let [n-moves (int (/ moves 128))]
    (-> board
        (fill-square full-move-n-store n-moves)
        (fill-square full-move-store (- moves (* n-moves 128))))))

(defn parse-fen
  "Parses information from given FEN and applies it to given state."
  [s state]
  (when-let [[board turn cast en-pass half full] (re-seq #"\S+" s)]
    (-> (assoc state :board
               (-> (fen-board->0x88board board)
                   (fill-square turn-store (if (= turn "w")
                                             white black))
                   (fill-square castling-store (castling->value cast))
                   (fill-square en-passant-store (if (= en-pass "-")
                                                   -1
                                                   (coord->index en-pass)))
                   (fill-square half-move-store (Integer/parseInt half))
                   (add-full-moves (Integer/parseInt full))))
        add-pieces
        add-king-indexes)))

(defn parse-state
  "Returns FEN representation of given game state."
  [state]
  (let [board (:board state)]
    (apply str
           (interpose " " (list (board->fen-board board)
                                (if (== (int (get board turn-store)) white) "w" "b")
                                (castling->str (int (get board castling-store)))
                                (let [en-passant (int (get board en-passant-store))]
                                  (if (== en-passant -1)
                                    "-"
                                    (index->coord en-passant)))
                                (get board half-move-store)
                                (+ (* (get board full-move-n-store) 127)
                                   (get board full-move-store)))))))
