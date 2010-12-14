(ns tursas.state.fen0x88
  (:use (tursas hexmove)
        (tursas.state common0x88 util0x88 movegen0x88))
  (:import (tursas.state common0x88.State0x88))
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq]))

(defn- castling-str
  "Converts internal castling representation to string."
  [board]
  (let [castling (get board CASTLING-STORE)
        to-char (fn [castling value letter]
                   (when (pos? (bit-and castling value)) letter))]
    (str (to-char castling 8 \K)
         (to-char castling 4 \Q)
         (to-char castling 2 \k)
         (to-char castling 1 \q))))

(defn- castling-value
  "Convers castling string to value."
  [castling]
  (letfn [(convert [letter value result]
            (if (some #(= % letter) castling)
              (+ result value)
              result))]
    (->> 0
         (convert \K 8)
         (convert \Q 4)
         (convert \k 2)
         (convert \q 1))))

(defn- find-king-index
  "Seeks king's index from piece-map."
  [state player]
  (let [piece-map (if (= player WHITE)
                    (:white-pieces state)
                    (:black-pieces state))
        king (if (= player WHITE)
               WHITE-KING
               BLACK-KING)]
    (loop [pieces (seq piece-map)]
      (cond (empty? pieces) nil
            (= (second (first pieces)) king) (ffirst pieces)
            :else (recur (rest pieces))))))

(defn- fen-board->0x88board
  "Converts given FEN board representation
   to 0x88 board representation."
  [fen-board]
  (reduce (fn [board [index piece]]
            (fill-square board index (piece-value piece)))
          (init-game-board)
          (seq/indexed (s/map-str #(str % "EEEEEEEE")
                                  (->> fen-board
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

(defn parse-state
  "Returns FEN representation of given STATE."
  [state]
  (let [board (:board state)]
    (str (board->fen-board board) " "
         (if (= (get board TURN-STORE) WHITE) "w" "b") " "
         (castling-str board) " "
         (index->algebraic (get board EN-PASSANT-STORE)) " "
         (get board HALF-MOVE-STORE) " "
         (get board FULL-MOVE-STORE))))

(defn- add-pieces
  "Adds all pieces from board to piece-map."
  [state]
  (loop [index 0x77
         blacks {}
         whites {}]
    (if (= index -1)
      (-> state
          (assoc :white-pieces whites)
          (assoc :black-pieces blacks))
      (recur (dec index)
             (if (black-piece? (get (:board state) index))
               (assoc blacks index (get (:board state) index))
               blacks)
             (if (white-piece? (get (:board state) index))
               (assoc whites index (get (:board state) index))
               whites)))))

(defn- add-king-indexes
  "Adds king indexes to state."
  [state]
  (assoc state :board
         (let [black-king (find-king-index state BLACK)
               white-king (find-king-index state WHITE)]
           (-> (:board state)
               (update-king-index black-king BLACK)
               (update-king-index white-king WHITE)))))

;; XXX: duplicated from state0x88, figure out better way to handle
(defn- update-check
  "Updates CHECK status bit on the state."
  [state]
  (assoc state :board
         (let [board (:board state)
               player (get board TURN-STORE)]
           (fill-square board GAME-STATUS-STORE
                        (if (threaten-index? board
                                             (king-index state (opponent player))
                                             player)
                          CHECK-BIT
                          0)))))

(defprotocol Fen
  (fen->state [fen]))

(extend-type String
  Fen
  (fen->state [fen]
    (when-let [fen-list (re-seq #"\S+" fen)]
      (-> (State0x88.
           (-> (fen-board->0x88board (first fen-list))
               (fill-square TURN-STORE (if (= (second fen-list) "w")
                                         WHITE BLACK))
               (fill-square CASTLING-STORE (castling-value
                                            (nth fen-list 2)))
               (fill-square EN-PASSANT-STORE (if (= (nth fen-list 3) "-")
                                               EN-PASSANT-STORE
                                               (algebraic->index (nth fen-list 3))))
               (fill-square HALF-MOVE-STORE (Integer/parseInt (nth fen-list 4)))
               (fill-square FULL-MOVE-STORE (Integer/parseInt (nth fen-list 5))))
           nil
           nil)
          add-pieces
          add-king-indexes
          update-check))))
