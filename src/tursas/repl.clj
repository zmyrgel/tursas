(ns tursas.repl
  (:use (tursas uci xboard utility game))
  (:require [clojure.contrib.string :as string]))

(def active-repl (ref :general))

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
          "quit" (case (@active-repl)
                       :general (quit)
                       :uci (do (quit-uci-engine)
                                (quit))
                       :xboard (do (quit-xboard-engine)
                                   (quit))
                       :else (quit))
          ""))
       (when (= @active-repl :uci)
         (print-uci-usage))
       (when (= @active-repl :xboard)
         (print-xboard-usage))))


(defn process-command
  "Processes command given by UI."
  [command]
  (loop [cmd (re-seq #"\w+" command)]
    (case (first cmd)
          "help" (print-usage)
          "load" (load-game (rest cmd))
          "save" (save-game (rest cmd))
          "bd" (display-board)
          "uci" (do (dosync (ref-set active-repl :uci))
                    (process-uci-command cmd))
          "xboard" (do (dosync (ref-set active-repl :xboard))
                       (process-xboard-command cmd))
          (case @active-repl
                :general (if (not (empty? (rest cmd)))
                           (recur (rest cmd))
                           nil)
                :uci (process-uci-command cmd)
                :xboard (process-xboard-command cmd)))))




