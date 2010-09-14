(ns tursas.game
  (:require [clojure.contrib.string :as string]))

;;(def game-state (ref "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
(def game-state (ref ()))
(def *engine-options* (ref {:debug false}))

(defn supported-options
  "Prints the supported options for KACE."
  []
  (map #(str "option name " % "\n")
       '("Hash type spin default 1 min 1 max 32"
         "UCI_EngineAbout type string default Tursas by Timo Myyrä")))

(defn save-game []
  (binding [*out* (java.io.FileWriter. "saved-game.txt")]
    (prn @game-state)))

(defn load-game []
  (try
   (let [state (read-string (slurp "saved-game.txt"))]
     (dosync (ref-set game-state state)))
   (catch Exception e nil)))

(defn setup-game
  "Sets up the chess game to given state."
  [state]
)

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
  "Undo's the last move XXX or given move?."
  []
  (dosync (ref-set game-state (rest @game-state)))
  ;;(update-boards (first @game-state))
  )

