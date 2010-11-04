(ns tursas.game
  (:require [clojure.contrib.string :as string])
  (:use (tursas search state eval)))

(def startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

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
                        :ponder-output false}))

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
    (let [fen-list (re-seq #"\S+" (state->fen @game-state))]
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
  (map (partial evaluate @*depth-limit*)
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
  (:prev-move
   (->> @game-state
        first
        legal-moves
        (take 5)
        rand-nth)))

(defn set-game
  "Sets game to given FEN state."
  [fen]
  (dosync
   (ref-set game-state (if (= fen "startpos")
                         (fen->state startpos)
                         (fen->state fen)))))

(defn set-clock!
  "Sets PLAYER's clock to TIME."
  [player time]
  (dosync (ref-set
           (if (= player :white)
             *white-clock*
             *black-clock*) time)))

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
  (set-game-option option
                   (if (= (get-game-option option) true)
                     false
                     true)))

(defn make-move
  "Apply given MOVE to game."
  [move]
  (dosync
   (ref-set game-state
            (cons (apply-move @game-state move) @game-state))))

(defn undo-move
  "Undo last move or if N given, N last moves."
  [& n]
  (dosync
   (ref-set game-state
            (if (nil? n)
              (rest @game-state)
              (nthnext @game-state n)))))
