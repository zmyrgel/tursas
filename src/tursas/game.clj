(ns tursas.game
  (:require [clojure.contrib.string :as string])
  (:use (tursas search state eval)))

(def startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def game-state (ref ()))
(def *search-depth* (ref 2))

(defn save-game
  "Saves the current game by writing game-state to file."
  []
  (binding [*out* (java.io.FileWriter. "saved-game.txt")]
    (prn @game-state)))

(defn load-game
  "Loads the game-state from file to resume the previous game."
  []
  (try
   (let [state (read-string (slurp "saved-game.txt"))]
     (dosync (ref-set game-state state)))
   (catch Exception e nil)))

(defn setup-game
  "Sets up the chess game to given state."
  [state])

(defn display-board
  "Displays the given FEN in ASCII."
  [fen]
  (let [fen-list (re-seq #"\S+" fen)]
    (loop [i 8
           pieces (re-seq #"\w+" (first fen-list))
           turn (second fen-list)]
      (if (= i 0)
        (println (str "------------------\n"
                      " | a b c d e f g h\n"
                      (if (= turn "w")
                        "  WHITE"
                        "  BLACK")
                      " TO MOVE"))
        (do
          (println (str i "| "
                        (string/map-str #(if (and (>= (int %) 49)
                                           (<= (int %) 56))
                                    (string/repeat (- (int %) 48)
                                            (str \space \-))
                                    (str \space %))
                                 (first pieces))))
          (recur (dec i)
                 (rest pieces)
                 turn))))))

(defn- expand-row
  "Expands numbers to spaces for given FEN notation ROW."
  [row]
  (string/map-str #(if (and (>= (int %) 49)
                     (<= (int %) 56))
                     (string/repeat (- (int %) 48)
                             \space)
                     %)
           row))

(defn- get-piece
  "Returns letter representing game piece in given LOCATION on the BOARD."
  [board location]
  (let [row (- 8 (get location 1))
        col (- (int (get location 0)) 97)]
    (get (get (map expand-row board) row) col)))

(defn- valid-coordinate-string?
  [coordinate]
  (empty? (filter #(if (= % coordinate) true false)
                  (for [x (range 8) y (range 8)]
                    (str (get "abcdefgh" x) (inc y))))))

<<<<<<< HEAD
(defn xboard-move-now
  "Tells the Engine to stop thinking and pick move immidiately."
  [])

(defn xboard-ping
  "Tells XBoard to wait for all the stuff to complete given before this
and once done, respond with pong"
  [n]
  (println (str "pong " n)))

(defn xboard-result
  "Sets the game result to engine, for learning purposes."
  [result])

(defn xboard-set-board
  "Tells the XBoard to set the board to given FEN string."
  [fen]
  (dosync (ref-set game-state (list fen))))

(defn xboard-hint
  "Tells the engine to provide a hint for good move."
  [])

(defn xboard-undo-move
  "Undo last N moves or just the last one."
  [& n]
  (dosync (ref-set game-state (rest @game-state))))

(defn xboard-bk
  "Tells the XBoard to"
  [])

(defn xboard-send-rating
  "Prompts the Engine to send its rating."
  []
  (io! (println "10")))

(defn xboard-parse-option
  "Wrapper to parse options from string and set them."
  [option]
  (let [pair (string/split #"=" option)]
    (if (= (count pair) 1)
      (xboard-set-option (first pair) true)
      (xboard-set-option (first pair) (second pair)))))

;;;; new ;;;;

(defn get-move
  "Let AI to seek its next move from STATE."
  [state]
  (let [depth @*search-depth*]
    (:leading-move (first (sort (map #(cons (minimax-search % depth evaluate-state) %)
                                     (available-states-from state)))))))

;; (defn piece-value->char
;;   "Gives piece character representation from its board VALUE."
;;   [value]
;;   (nth "PpRrNnBbQqKk" value))

;; (defn piece-char->value
;;   "Gives pieces character numerical representation from its CHAR."
;;   [char]
;;   (case char
;;         \P WHITE-PAWN
;;         \p BLACK-PAWN
;;         \R WHITE-ROOK
;;         \r BLACK-ROOK
;;         \N WHITE-KNIGHT
;;         \n BLACK-KNIGHT
;;         \B WHITE-BISHOP
;;         \b BLACK-BISHOP
;;         \Q WHITE-QUEEN
;;         \q BLACK-QUEEN
;;         \K WHITE-KING
;;         \k BLACK-KING
;;         EMPTY))



