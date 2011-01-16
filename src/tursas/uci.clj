(ns tursas.uci
  (:require [clojure.contrib.string :as s])
  (:use (tursas game util)))

(defn supported-uci-options
  "Prints the supported options for Tursas."
  []
  (map #(str "option name " % "\n")
       '("Hash type spin default 1 min 1 max 32"
         "UCI_EngineAbout type string default Tursas by Timo Myyrä")))

(defn- uci-set-position!
  "Sets game to position [fen <fenstring> | startpos ] moves <move1> .... <movei>"
  [pos]
  (let [operands (re-seq #"\S+" pos)]
    (if (= (first operands) "fen")
      (set-game! (second operands))
      (set-game! (first operands)))
    (cond (and (not (nil? (nth operands 1 nil)))
               (= (nth operands 1 nil) "moves"))
          (map make-chess-move (nthnext operands 2))
          (and (not (nil? (nth operands 2 nil)))
               (= (nth operands 2 nil) "moves"))
          (map make-chess-move (nthnext operands 3)))))

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
  (let [words (re-seq #"\S+" command)]
    (cond (= (first words) "register") (println "register got")
          (= (first words) "name") (register-name (second words))
          (= (first words) "code") (register-value (second words))
          :else (println "invalid option!"))))

(defn- go-handler
  "Handler for go command"
  [command]
  (case (first command)
        "searchmoves" (do (set-option! :move-limit (take-while move-string? command))
                          (recur (drop-while move-string? command)))
        "ponder" (do (set-option! :ponder-mode true)
                     (recur (rest command)))
        "wtime" (do (set-clock! :white (second command))
                    (recur (rest command)))
        "btime" (do (set-clock! :black (second command))
                    (recur (rest command)))
        "winc" (do (set-option! :white-increment (second command))
                   (recur (rest command)))
        "binc" (do (set-option! :black-increment (second command))
                   (recur (rest command)))
        "movestogo" (do (set-option! :movestogo (second command))
                        (recur (rest command)))
        "depth" (do (set-option! :depth-limit (second command))
                    (recur (rest command)))
        "nodes" (do (set-option! :node-limit (second command))
                    (recur (rest command)))
        "movetime" (do (set-option! :search-time (second command))
                       (recur (rest command)))
        "infinite" (do (set-option! :search-time 0)
                       (recur (rest command)))
        (println "Unknown uci command!")))

(defn print-uci-usage
  "Prints the available commands of the repl."
  []
  (s/map-str
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
     " ponderhit - the user has played the expected move.")))

(defn- uci-set-option!
  "Sets UCI specific game options:
   name <id> [value <x>]"
  [options]
  (let [words (re-seq #"\S+" options)]
    (case (count words)
          2 (set-option! (keyword (second words)) true)
          4 (set-option! (keyword (second words)) (nth words 4))
          (println "Error parsing options!"))))

(defn process-uci-command
  "Processes command in uci mode."
  [words]
  (case (first words)
        "uci" (do (println "id name Tursas 0.1")
                  (println "id author Timo Myyrä")
                  (map println (supported-uci-options))
                  (println "uciok"))
        "debug" (set-option! :debug
                             (if (= (second words) "on")
                               true
                               false))
        "isready" (println "readyok")
        "setoption" (uci-set-option! (rest words))
        "register" (register (rest words))
        "ucinewgame" (set-game! "startpos")
        "position" (uci-set-position! (rest words))
        "go" (go-handler (re-seq #"\S+" (rest words)))
        "stop"
        "ponderhit"
        nil))

