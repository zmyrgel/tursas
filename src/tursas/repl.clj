(ns tursas.repl
  (:use [tursas.game])
  (:require [clojure.contrib.string :as string]))

(def active-repl (ref "general"))

(defn- write [^String msg]
  (.write *out* (.concat msg "\n"))
  (flush))

(defn- print-usage
  "Prints the available commands of the repl."
  []
  (io! (string/map-str println
                       '("Available UCI commands are:"
                         "uci - Sets engine to UCI mode."
                         "debug [ on | off ] - print debug messages"
                         "isready - Prompts the engine if its ready"
                         "setoption name <id> [value <x>]"
                         "register [later | name <x> | code <x> ] - register values with engine"
                         "ucinewgame - start a new game"
                         "position [fen <fenstring> | startpos ] moves <move1> .... <movei>"
                         "go [searchmoves <move1> .... <movei>]"
                         "   ponder - startpos searching in pondering mode"
                         "   wtime <x> - white has x msec left on the clock"
                         "   btime <x> - black has x msec left on the clock"
                         "   winc <x> - white increment per move in mseconds if x > 0"
                         "   binc <x> - black increment per move in mseconds if x > 0"
                         "   movestogo <x> - there are x moves to next time control"
                         "   depth <x> - search x plies only"
                         "   nodes <x> - search x nodes only"
                         "   movetime <x> - search exactly x mseconds"
                         "   infinite - search until 'stop' command is sent"
                         " stop - stop calculating as soon as possible"
                         " ponderhit - the user has played the expected move."
                         " quit - quit the program as soon as possible"
                         ""
                         "Extra commands:"
                         "help - display this help"
                         "load - load the last saved game from file"
                         "save - store the current game to file"
                         "bd - display the board on the screen"))))

(defn- set-option
  "Sets the option given as command"
  [args] ;; name <id> [value <x>]
  args)

(defn- send-command
  "Sends given command."
  [command]
  (write command))

(defn- quit
  "Function to handle closing the engine."
  []
  (System/exit 0))

(defn process-command
  "Processes command given by UI."
  [command]
  (loop [cmd (re-seq #"\w+" command)]
    (case (first cmd)
          "uci" (do (send-command "id name Tursas 0.1")
                    (send-command "id author Timo Myyrä")
                    (map send-command (supported-options))
                    (send-command "uciok"))

          "debug" (def debug (if (.equal (second cmd) "on") true false))
          "isready" (send-command "readyok")
          "setoption" (set-option (rest cmd))
          "register"
          "ucinewgame"
          "position"
          "go"
          "stop"
          "ponderhit"
          "quit" (quit)
          "help" (print-usage)
          "load" (load-game (rest cmd))
          "save" (save-game (rest cmd))
          "bd" (display-board)
          (if (not (empty? (rest cmd)))
            (recur (rest cmd))
            nil))))
