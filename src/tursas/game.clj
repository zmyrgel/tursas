(ns tursas.game
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq])
  (:use (tursas search eval state state0x88 hexmove)))

(def startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def check-fen "r1bqkbnr/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")

(def check-fen "r1bqkbnr/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")

(def cast-fen "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")

(def prom-fen "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")

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
  "Displays the current chess board in ASCII."
  []
  (io! (println
        (if (empty? @game-state)
          "Can't print empty board!"
          (let [fen-list (re-seq #"\S+" (state->fen (first @game-state)))
                turn (second fen-list)]
            (str (s/map-str (fn [[index piece]]
                              (str (- 8 index) "|" piece "\n"))
                            (seq/indexed (->> fen-list
                                              first
                                              (s/replace-by #"\d" #(str (s/repeat (Integer/parseInt %) \-)))
                                              (s/replace-by #"[\p{Alpha}-]" #(str \space %))
                                              (s/split #"/+"))))
                 "------------------\n"
                 " | a b c d e f g h\n"
                 (if (= turn "w")
                   "  WHITE"
                   "  BLACK")
                 " TO MOVE"))))))

(defn display-fen
  "Display FEN of currect game state."
  []
  (->> @game-state
       first
       state->fen
       println
       io!))

(defn list-moves
  "List all available moves from currect state."
  []
  (io! (doall (map #(println (:prev-move %))
                   (->> @game-state
                        first
                        legal-states)))))

(defn- valid-coord?
  "Check that given coord is valid on chess board."
  [coord]
  (not (nil? (some #(= coord %)
                   (for [x (range 8) y (range 8)]
                     (str (get "abcdefgh" x) (inc y)))))))

(defn choose-move
  "Let AI to choose a move from STATE with given STRATEGY."
  [states & strategy]
  (:prev-mode
   (case strategy
         :total-random (rand-nth states)
         (last (sort states)))))

(defn- legal-moves
  "Generates all available moves from given STATE."
  [state]
  (map #(evaluate (:depth-limit @game-options) %)
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
  (io! (println
        (if (empty? @game-state)
          "Can't calculate legal moves from empty state!"
          (->> @game-state
               first
               legal-moves
               (take 5)
               rand-nth
               :prev-move)))))

(defn set-game
  "Sets game to given FEN state."
  [fen]
  (dosync
   (ref-set game-state
            (cond (= fen "startpos") (list (fen->state startpos))
                  (= fen "check") (list (fen->state check-fen))
                  :else (list (fen->state fen))))))

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
  [algebraic]
  (let [new-state (apply-move (first @game-state) (algebraic->move algebraic))]
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
