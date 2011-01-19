(ns tursas.game
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq])
  (:use (tursas search state move hexmove util)
        (tursas.state state0x88)))

(def startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def check-fen "r1bqkbnr/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")
(def cast-fen "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")
(def prom-fen "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")

(def inf Integer/MAX_VALUE)
(def -inf (+ Integer/MIN_VALUE 1))

(def active-repl (ref :general))
(def game-state (ref ()))
(def *black-clock* (ref 300))
(def *white-clock* (ref 300))
(def game-options (ref {:depth-limit 4
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

(defn get-repl
  "Returns currently active repl"
  []
  @active-repl)

(defn set-repl!
  "Sets the currently active repl"
  [repl]
  (dosync (ref-set active-repl repl)))

(defn- add-game-state
  "Adds given state to game state."
  [new-state]
  (dosync (ref-set game-state (cons new-state @game-state))))

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

(defn- game-end?
  "Predicate to see if last game state ended the game."
  []
  (let [state (first @game-state)]
    (or (draw? state)
        (mate? state))))

(defn- game-result
  "Function to print game result."
  []
  (println "RESULT "
           (let [state (first @game-state)]
             (or (draw? state) (str "1/2-1/2 {" (draw-type state) "}")
                 (mate? state) (if (= (turn state) :white)
                                 "0-1 {Black mates"
                                 "1-0 {White mates")))))

(defn display-board
  "Displays the current chess board in ASCII."
  []
  (println
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
            " TO MOVE"
            (when (check? (first @game-state))
              (if (= (turn (first @game-state)) :white)
                " BLACK CHECKED!"
                " WHITE CHECKED!")))))))

(defn display-fen
  "Display FEN of currect game state."
  []
  (->> @game-state
       first
       state->fen
       println))

(defn display-perft
  "Display Perft of given depth."
  [depth]
  (println
   (if (empty? @game-state)
     "Can't calculate perft from empty state!"
     (time (-> @game-state
               first
               (perft (Integer/parseInt depth)))))))

(defn list-moves
  "List all available moves from currect state."
  []
  (dorun (map #(println (move->coord (last-move %)))
              (->> @game-state
                   first
                   legal-states))))

(defn- all-moves
  "Generates all available states from given state.
   Returns list of two element vectors. The first member of
   vector is the heuristic value and second is the actual state."
  [state]
  (map (partial alpha-beta (:depth-limit @game-options))
       (legal-states state)))

(defn ai-move
  "Prompt a move from AI and add it to game-state."
  []
  (if (empty? @game-state)
    "Can't calculate score from empty state!"
    (if (game-end?)
      (do (game-result)
          (quit))
      (let [move (second (alpha-beta (first @game-state)
                                     -inf
                                     inf
                                     (:depth-limit @game-options)))]
        (do (add-game-state move)
            (println "move " (move->coord (last-move (first @game-state)))))))))

(defn get-score
  "Calculates state's score by checking child states
   to certain depth using alpha-beta algorithm."
  []
  (println
   (if (empty? @game-state)
     "Can't calculate score from empty state!"
     (first (alpha-beta (first @game-state)
                        -inf
                        inf
                        (:depth-limit @game-options))))))

(defn eval-current-state
  "Evaluates the current state and prints its score."
  []
  (println (if (empty? @game-state)
             "Can't evaluate score from empty game state!"
             (->> @game-state
                  first
                  evaluate))))

(defn get-hint
  "Evaluates all states and chooses one from top five moves at random."
  []
  (println
   (if (empty? @game-state)
     "Can't calculate legal moves from empty state!"
     (->> @game-state
          first
          all-moves
          (take 5)
          rand-nth
          last-move))))

(defn set-game!
  "Sets game to given FEN state."
  [fen]
  (add-game-state
   (cond (= fen "startpos") (fen->state startpos)
         (= fen "check") (fen->state check-fen)
         (= fen "cast") (fen->state cast-fen)
         (= fen "prom") (fen->state prom-fen)
         :else (fen->state fen))))

(defn set-clock!
  "Sets player's clock to given time."
  [player time]
  (dosync
   (ref-set (if (= player :white)
              *white-clock*
              *black-clock*)
            time)))

(defn set-option!
  "Sets game option of given key to value."
  [k v]
  (dosync (alter game-options assoc k v)))

(defn get-option
  "Returns value of given game option"
  [option]
  (@game-options option))

(defn toggle-option!
  "Toggles the value of given boolean game option."
  [option]
  (set-option! option (not (get-option option))))

(defn make-chess-move
  "If given string represents chess move, apply it to current game."
  [s]
  (if (move-string? s)
    (let [state (first @game-state)]
      (if (game-end?)
        (do (game-result)
            (quit))
        (if-let [new-state (apply-move state (coord->move s))]
          (do (add-game-state new-state)
              (ai-move))
          (println "Illegal move: " s))))
    (println "Illegal move: " s)))

(defn undo-move
  "Undo last move or if N given, N last moves."
  [& n]
  (dosync
   (ref-set game-state
            (if (nil? n)
              (rest @game-state)
              (nthnext @game-state n)))))
