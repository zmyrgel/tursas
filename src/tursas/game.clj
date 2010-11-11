(ns tursas.game
  (:require [clojure.contrib.string :as string])
  (:use (tursas search eval state state0x88)))

(def startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(def active-repl (ref :general))
(def game-state (ref ()))
(def *black-clock* (ref 300))
(def *white-clock* (ref 300))
(def game-options (ref {:depth-limit 2
                        :node-limit 5000
                        :time-limit 15
                        :random-mode false
                        :ai-mode false
                        :xboard-protocol-version 1
                        :debug false
                        :ponder false
                        :ponder-output false
                        :movestogo 0
                        :white-increment 0
                        :black-increment 0
                        :move-limit nil
                        :search-time 0}))

(defn get-active-repl
  "Returns currently active repl"
  []
  @active-repl)

(defn set-active-repl
  "Sets the currently active repl"
  [repl]
  (dosync (ref-set active-repl repl)))

(defn quit
  "Function to handle closing the engine."
  []
  (System/exit 0))

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

(defn display-board
  "Displays the given FEN in ASCII."
  []
  (if (empty? @game-state)
    (io! (println "Can't print empty board!"))
    (let [fen-list (re-seq #"\S+" (state->fen (first @game-state)))]
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
            (println (str i "|"
                          (string/map-str #(if (and (>= (int %) 49)
                                                    (<= (int %) 56))
                                             (string/repeat (- (int %) 48)
                                                            (str \space \-))
                                             (str \space %))
                                          (first pieces))))
            (recur (dec i)
                   (rest pieces)
                   turn)))))))

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

(defn choose-move
  "Let AI to choose a move from STATE with given STRATEGY."
  [moves & strategy]
  (case strategy
        :total-random (rand-nth moves)
        (first (sort-by :score > moves))))

(defn- legal-moves
  "Generates all available moves from given STATE."
  [state]
  (map (partial evaluate (:depth-limit @game-options))
       (legal-states state)))

(defn get-move
  "Return a move from current game state."
  []
  (->> @game-state
       first
       legal-moves
       choose-move))

(defn get-hint
  "Evaluates all states and chooses one from top five moves at random."
  []
  (->> @game-state
       first
       legal-moves
       (take 5)
       rand-nth
       :prev-move
       println
       io!))

(defn set-game
  "Sets game to given FEN state."
  [fen]
  (dosync
   (ref-set game-state
            (if (= fen "startpos")
              (list (fen->state startpos))
              (list (fen->state fen))))))

(defn set-clock!
  "Sets PLAYER's clock to TIME."
  [player time]
  (dosync
   (ref-set (if (= player :white)
              *white-clock*
              *black-clock*)
            time)))

(defn set-game-option
  "Sets game option"
  [key value]
  (dosync (alter game-options
                 (assoc @game-options key value))))

(defn get-game-option
  "Returns value of given game option"
  [option]
  (@game-options option))

(defn toggle-game-option
  "Toggles the value of given game option, only for boolean."
  [option]
  (set-game-option option (not (= (get-game-option option) true))))

(defn make-chess-move
  "Make given MOVE in chess game."
  [move]
  (let [new-state (apply-move (first @game-state) move)]
    (if (nil? new-state)
      (io! (println "Invalid move!"))
      (dosync
       (ref-set game-state (cons new-state @game-state))))))

(defn undo-move
  "Undo last move or if N given, N last moves."
  [& n]
  (dosync
   (ref-set game-state
            (if (nil? n)
              (rest @game-state)
              (nthnext @game-state n)))))
