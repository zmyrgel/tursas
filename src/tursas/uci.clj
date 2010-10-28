(ns tursas.uci
  (:use (tursas utility))
  (:require [clojure.contrib.string :as string]))

(def uci-engine-options (ref {:debug false}))

(defn supported-uci-options
  "Prints the supported options for Tursas."
  []
  (map #(str "option name " % "\n")
       '("Hash type spin default 1 min 1 max 32"
         "UCI_EngineAbout type string default Tursas by Timo Myyrä")))

(defn- uci-set-position
  "Sets game to position [fen <fenstring> | startpos ] moves <move1> .... <movei>"
  [pos]
  (let [operands (re-seq #"\S+" pos)]
    (if (= (first operands) "fen")
      (set-game (second operands))
      (set-game (first operands)))
    (cond (and (not (nil? (nth operands 1 nil)))
               (= (nth operands 1 nil) "moves"))
          (map make-move (nthnext operands 2))
          (and (not (nil? (nth operands 2 nil)))
               (= (nth operands 2 nil) "moves"))
          (map make-move (nthnext operands 3)))))

(defn- register-name
  "Registers given name."
  [name]
  (println (str "Registered name " name)))

(defn- register-value
  "Registers given value."
  [value]
  (println (str "Registered value " value)))

(defn- register
  "Registers values."
  [command]
  (let [values (re-seq #"\S+" command)]
    (cond (= (first values) "register")
          (println "register got")
          (= (first values) "name")
          (register-name (second values))
          (= (first values) "code")
          (register-value (second values))
          :else (println "invalid option!"))))

(defn- go
  "Handles go command"
  [command]
  )

(defn print-uci-usage
  "Prints the available commands of the repl."
  []
  (io! (string/map-str
        println
        '("Available UCI commands are:"
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
          " quit - quit the program as soon as possible"))))

(defn quit-uci-engine
  "Tells UCI engine to quit.
  Not used currently but provided for future expansion."
  [])

(defn process-uci-command
  "Processes command in uci mode."
  [command]
  (case (first command)
        "uci" (do (send-command "id name Tursas 0.1")
                  (send-command "id author Timo Myyrä")
                  (map send-command (supported-uci-options))
                  (send-command "uciok"))
        "debug" (set-option :debug
                            (if (= (second command) "on")
                              true
                              false))
        "isready" (send-command "readyok")
        "setoption" (set-option (rest command))
        "register" (register (rest command))
        "ucinewgame" (set-game "startpos") ;; wrong?
        "position" (uci-set-position (rest command))
        "go" (go (rest command))
        "stop"
        "ponderhit"
        nil))

