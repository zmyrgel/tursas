(ns tursas.state0x88.fen
  (:use (tursas.state0x88 common util movegen move))
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq]))

(def castling-values (list [8 \K] [4 \Q] [2 \k] [1 \q]))

(defn- castling->str
  "Converts internal castling representation to string."
  [castling]
  (let [result (s/map-str (fn [[value letter]]
                            (when (pos? (bit-and castling value))
                              letter))
                          castling-values)]
    (if (empty? result)
      "-"
      result)))

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
  (let [piece-map (if (== player WHITE)
                    (:white-pieces state)
                    (:black-pieces state))
        king (if (== player WHITE)
               WHITE-KING
               BLACK-KING)]
    (loop [pieces (seq piece-map)]
      (cond (empty? pieces) nil
            (= (second (first pieces)) king) (ffirst pieces)
            :else (recur (rest pieces))))))

(defn- fen-board->0x88board
  "Converts string given in FEN notation to 0x88 board representation."
  [s]
  (reduce (fn [board [index piece]]
            (fill-square board index (piece-value piece)))
          (init-game-board)
          (seq/indexed (s/map-str #(str % "EEEEEEEE")
                                  (->> s
                                       (s/replace-by #"\d" #(str (s/repeat (Integer/parseInt %) \E)))
                                       (s/split #"/+")
                                       reverse)))))

(defn- make-fen-row
  "Builds single fen row from given BOARD and ROW index."
  [board row]
  (s/map-str #(if (= (get % 0) \E) (count %) %)
             (s/partition #"E+"
                          (s/map-str #(piece-name (get board (+ row %)))
                                     (range 8)))))

(defn- board->fen-board
  "Convert the given state's BOARD to fen board field."
  [board]
  (s/join "/" (map #(make-fen-row board %)
                   [0x70 0x60 0x50 0x40 0x30 0x20 0x10 0x0])))

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
         (let [black-king (find-king-index state BLACK)
               white-king (find-king-index state WHITE)]
           (-> (:board state)
               (update-king-index black-king BLACK)
               (update-king-index white-king WHITE)))))

(defn- add-full-moves
  "Helper funtion to add full moves to board.
   Needed to workaround the byte limitation of the board."
  [board moves]
  (let [n-moves (int (/ moves 128))]
    (-> board
        (fill-square FULL-MOVE-N-STORE n-moves)
        (fill-square FULL-MOVE-STORE (- moves (* n-moves 128))))))

(defn parse-fen
  "Parses information from given FEN and applies it to given state."
  [s state]
  (when-let [fen-list (re-seq #"\S+" s)]
    (-> (assoc state :board
               (-> (fen-board->0x88board (first fen-list))
                   (fill-square TURN-STORE (if (= (second fen-list) "w")
                                             WHITE BLACK))
                   (fill-square CASTLING-STORE (castling->value (nth fen-list 2)))
                   (fill-square EN-PASSANT-STORE (if (= (nth fen-list 3) "-")
                                                   -1
                                                   (coord->index (nth fen-list 3))))
                   (fill-square HALF-MOVE-STORE (Integer/parseInt (nth fen-list 4)))
                   (add-full-moves (Integer/parseInt (nth fen-list 5)))))
        add-pieces
        add-king-indexes)))

(defn parse-state
  "Returns FEN representation of given game state."
  [state]
  (let [board (:board state)]
    (s/join " " (list (board->fen-board board)
                      (if (== (int (get board TURN-STORE)) WHITE) "w" "b")
                      (castling->str (int (get board CASTLING-STORE)))
                      (let [en-passant (int (get board EN-PASSANT-STORE))]
                        (if (== en-passant -1)
                          "-"
                          (index->coord en-passant)))
                      (get board HALF-MOVE-STORE)
                      (+ (* (get board FULL-MOVE-N-STORE) 127)
                         (get board FULL-MOVE-STORE))))))

