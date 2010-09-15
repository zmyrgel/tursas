(ns tursas.game
  (:require [clojure.contrib.string :as string]))

;; "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
(def game-state (ref ()))
(def uci-engine-options (ref {:debug false}))
(def xboard-engine-options (ref {:debug false :protocol 2}))

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
  "Displays the current chess board state in ASCII."
  []
  (let [fen-list (re-seq #"\S+" (first @game-state))]
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

(defn make-move
  "Make the given move in the active game."
  [move])

(defn undo-move
  "Undo N moves or the just the last move."
  [& n])

(defn xboard-set-option
  "Sets XBoard engine options."
  [option value])

(defn xboard-get-option
  "Returns the current OPTIONs value."
  [option])

(defn xboard-accept-feature
  "Tells the engine that GUI accepts last feature."
  [])

(defn xboard-reject-feature
  "Tells the engine that GUI rejects given feature."
  [])

(defn xboard-make-move
  "Tells the XBoard to make MOVE."
  [move])

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
  [&n]
  (dosync (ref-set game-state (rest @game-state))))

(defn xboard-bk
  "Tells the XBoard to"
  [])

(defn xboard-send-rating
  "Prompts the Engine to send its rating."
  [])

(defn xboard-parse-option
  "Wrapper to parse options from string and set them."
  [options])
;; do stuff NAME=VALUE or NAME for boolean
;; call xboard-set-option
