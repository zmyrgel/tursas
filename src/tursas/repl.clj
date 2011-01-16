(ns tursas.repl
  (:use (tursas uci xboard)
        [tursas.game :only [get-repl
                            load-game
                            save-game
                            display-board
                            display-fen
                            list-moves
                            get-score
                            ai-move
                            eval-current-state
                            display-perft
                            set-repl!
                            quit]])
  (:require [clojure.contrib.string :as s]))

(defn- print-usage
  "Prints the available commands of the repl."
  []
  (do
    (s/map-str
     println
     '("Available general commands:"
       "help - display this help"
       "load - load the last saved game from file"
       "save - store the current game to file"
       "bd - display the board on the screen"
       "fd - display current game state in FEN"
       "lm - print a list of all available moves"
       "gs - calculates score for the current game state"
       "es - evaluates current game state"
       "pf n - calculate perft score to depth of n"
       "uci - enable uci mode"
       "xboard - ebable xboard mode"
       "quit - quite the Tursas engine"
       ""))
    (let [repl (get-repl)]
      (cond (= repl :uci) (print-uci-usage)
            (= repl :xboard) (print-xboard-usage)
            :else nil))))

(defn process-command
  "Processes command given by UI."
  [command]
  (loop [words (re-seq #"\w+" command)]
    (case (first words)
          "help" (print-usage)
          "load" (load-game)
          "save" (save-game)
          "bd"   (display-board)
          "fd" (display-fen)
          "lm" (list-moves)
          "gs" (get-score)
          "cp" (do (ai-move)
                   (display-board))
          "es" (eval-current-state)
          "pf" (display-perft (second words))
          "uci"  (set-repl! :uci)
          "xboard" (set-repl! :xboard)
          "quit" (quit)
          (case (get-repl)
                :general (when (not (empty? (rest words)))
                           (recur (rest words)))
                :uci (process-uci-command words)
                :xboard (process-xboard-command words)))))




