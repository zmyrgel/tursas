(ns tursas.repl
  (:use (tursas uci xboard game))
  (:require [clojure.contrib.string :as string]))

(defn- print-usage
  "Prints the available commands of the repl."
  []
  (io! (string/map-str
        println
        '("Available general commands:"
          "help - display this help"
          "load - load the last saved game from file"
          "save - store the current game to file"
          "bd - display the board on the screen"
          "uci - enable uci mode"
          "xboard - ebable xboard mode"
          "quit - quite the Tursas engine"
          ""))
       (when (= (get-active-repl) :uci)
         (print-uci-usage))
       (when (= (get-active-repl) :xboard)
         (print-xboard-usage))))


(defn process-command
  "Processes command given by UI."
  [command]
  (loop [cmd (re-seq #"\w+" command)]
    (case (first cmd)
          "help" (print-usage)
          "load" (load-game (rest cmd))
          "save" (save-game (rest cmd))
          "bd"   (display-board)
          "uci"  (set-active-repl :uci)
          "xboard" (set-active-repl :xboard)
          "quit" (quit)
          (case (get-active-repl)
                :general (when (not (empty? (rest cmd)))
                           (recur (rest cmd)))
                :uci (process-uci-command cmd)
                :xboard (process-xboard-command cmd)))))




