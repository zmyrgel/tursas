(ns tursas.uci
  (:require [clojure.contrib.string :as string])
  (:use (tursas utility game)))

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


(defn- move?
  "Predicate to detect valid move strings"
  [item]
  (and (or (count item 4)
           (count item 5))
       (or (= item "0000")
           (and (number? (get item 0))
                (number? (get item 2)))
           (and (char? (get item 1))
                (char? (get item 3))))))

(defn- go-handler
  "Handler for go command"
  [command]
  (case (first values)
        "searchmoves" (do (set-game-option :move-limit (take-while move? values))
                          (recur (drop-while move? values)))
        "ponder" (do
                   (set-game-option :ponder-mode true)
                   (recur (rest values)))
        "wtime" (do (set-clock! :white (second values))
                    (recur (rest values)))
        "btime" (do (set-clock! :black (second values))
                    (recur (rest values)))
        "winc" (do (set-game-option :white-increment (second values))
                   (recur (rest values)))
        "binc" (do (set-game-option :black-increment (second values))
                   (recur (rest values)))
        "movestogo" (do (set-game-option :movestogo (second values))
                        (recur (rest values)))
        "depth" (do (set-game-option :depth-limit (second values))
                    (recur (rest values)))
        "nodes" (do (set-game-option :node-limit (second values))
                    (recur (rest values)))
        "movetime" (do (set-game-option :search-time (second values))
                       (recur (rest values)))
        "infinite" (do (set-game-option :search-time 0)
                       (recur (rest values)))
        (println "BAA!"))

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

(defn- uci-set-option
  "Sets UCI specific game options:
   name <id> [value <x>]"
  [options]
  (let [opts (re-seq #"\S+" options)]
    (case (count opts)
          2 (set-game-option (keyword (second opts)) true)
          4 (set-game-option (keyword (second opts)) (fourth opts))
          (write "Error parsing options!"))))

(defn process-uci-command
  "Processes command in uci mode."
  [command]
  (case (first command)
        "uci" (do (send-command "id name Tursas 0.1")
                  (send-command "id author Timo Myyrä")
                  (map send-command (supported-uci-options))
                  (send-command "uciok"))
        "debug" (set-game-option :debug
                            (if (= (second command) "on")
                              true
                              false))
        "isready" (send-command "readyok")
        "setoption" (uci-set-option (rest command))
        "register" (register (rest command))
        "ucinewgame" (set-game "startpos")
        "position" (uci-set-position (rest command))
        "go" (go-handler (re-seq #"\S+" (rest command)))
        "stop"
        "ponderhit"
        nil))

