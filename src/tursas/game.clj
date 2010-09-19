(ns tursas.game
  (:use [tursas.0x88board])
  (:require [clojure.contrib.string :as string]))

(def default-startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
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

;; XXX: NPE when game-state == empty
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

(defn expand-row
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

(defn- available-moves-for
  "Lists all available moves for PIECE"
  [piece from])

(defn captures?
  "Placeholder"
  [to]
  true)

(defn castling?
  "Placeholder"
  [move state]
  true)

(defn en-passant?
  "Placeholder"
  [move]
  true)

(defn check?
  "Placeholder"
  [move state]
  true)

(defn- legal-move?
  "Predicate to see if given MOVE is legal in STATE"
  [move state]
  (let* [from (str (get move 0) (get move 1))
         to (str (get move 2) (get move 3))
         promotion (str (get move 4))
         fen-board (nth state 0)
         side (nth state 1)
         castling (nth state 2)
         en-passant (nth state 3)
         half-moves (nth state 4)
         full-moves (nth state 5)
         piece (get-piece fen-board from)]
        (if (contains? (available-moves-for piece from) to)
          true
          false)))

(defn- make-fen
  "Returns new FEN string after MOVE is applied to given STATE."
  [move state]
    (let* [captures (captures? move)
           castling (castling? move state)
           en-passant (en-passant? move)
           check (check? move state)]
          (make-fen move state captures castling en-passant check)))

(defn xboard-make-move
  "Tells the XBoard to make MOVE."
  [move]
  (let* [state (re-seq #"\S+" (first @game-state))]
        (if (legal-move? move state)
          (make-fen move state)
          nil)))

(defn valid-coordinate-string?
  [coordinate]
  (empty? (filter #(if (= % coordinate) true false)
                  (for [x (range 8) y (range 8)] (str (get "abcdefgh" x) (inc y))))))

(defn fen->0x88
  "Convert FEN to 0x88 vector."
  [fen-board])

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
  [options])
;; do stuff NAME=VALUE or NAME for boolean
;; call xboard-set-option

(defn get-material-diff
  "Calculates material difference from FEN"
  [fen]
  (reduce + (map piece-value fen)))

