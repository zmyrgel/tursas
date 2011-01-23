(ns tursas.core
  (:gen-class)
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq])
  (:use (tursas search state move util)
        (tursas.state0x88 core move)))

(def startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def check-fen "r1bqkbnr/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")
(def cast-fen "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")
(def prom-fen "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")

(def inf Integer/MAX_VALUE)
(def -inf (+ Integer/MIN_VALUE 1))

(def xboard-supported-features {:ping 1, :setboard 1, :playother 1, :san 0
                                :usermove 0, :time 0, :draw 1, :sigint 0
                                :sigterm 0, :reuse 0, :analyse 0
                                :myname "\"Tursas 0.1\"", :variants "\"normal\""
                                :colors 0, :ics 0, :name 0, :pause 0, :nps 0
                                :debug 0, :memory 0, :smp 0, :done 1})

(def protocol (ref :general))
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

(defn get-protocol
  "Returns currently active reader protocol"
  []
  @protocol)

(defn set-protocol!
  "Sets the currently active reader protocol"
  [prot]
  (dosync (ref-set protocol prot)))

(defn- add-game-state
  "Adds given state to game state."
  [new-state]
  (dosync (ref-set game-state (cons new-state @game-state))))

(defn- game-result
  "Function to print game result."
  []
  (println "RESULT "
           (let [state (first @game-state)]
             (when (game-end? (first @game-state))
               (result state)))))

(defn display-board
  "Displays the current chess board in ASCII."
  []
  (println
   (if (empty? @game-state)
     "Can't print empty board!"
     (let [fen-list (re-seq #"\S+" (state->fen (first @game-state)))
           side (second fen-list)]
       (str (s/map-str (fn [[index piece]]
                         (str (- 8 index) "|" piece "\n"))
                       (seq/indexed (->> fen-list
                                         first
                                         (s/replace-by #"\d" #(str (s/repeat (Integer/parseInt %) \-)))
                                         (s/replace-by #"[\p{Alpha}-]" #(str \space %))
                                         (s/split #"/+"))))
            "------------------\n"
            " | a b c d e f g h\n"
            (if (= side "w")
              "  WHITE"
              "  BLACK")
            " TO MOVE"
            (when (check? (first @game-state))
              (if (= (turn (first @game-state)) :white)
                ", WHITE IN CHECK!"
                ", BLACK IN CHECK!")))))))

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
  (dorun (map #(println (move->coord %))
              (->> @game-state
                   first
                   legal-moves))))

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
    (if (game-end? (first @game-state))
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
      (if (game-end? state)
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

;; xboard

(defn xboard-print-supported-features
  "Prints the default features of the engine."
  []
  (dorun (map println
              (for [option (keys xboard-supported-features)]
                (format "feature %s=%s"
                        (s/as-str option)
                        (xboard-supported-features option))))))

(defn print-xboard-usage
  "Prints the available commands of the repl."
  []
  (s/map-str
   println
   '("Available XBoard commands are:"
     "protover N - change engine to use protocol version N"
     "accepted - Accept last feature"
     "reject - Reject last feature"
     "new - Sets the board to the chess starting position. Set White on move. Leave force mode and set the engine to play Black."
     "variant VARIANT - change to use VARIANT rules. Only 'normal' supported"
     "random - Tell engine to add little random elements"
     "force - Disable engine AI"
     "go - Enable engine AI"
     ;;"playother - Tell AI to switch sides"
     ;;"white - Tell white to move, engine to play black [obsolete]"
     ;;"black - Tell black to move, engine to play white [obsolete]"
     ;;"level MPS BASE INC - set time controls"
     ;;"st TIME - set time controls"
     "sd DEPTH - set search depth to DEPTH"
     ;;"nps NODE_RATE - search only NODE_RATE nodes"
     ;;"time N - set the engine clock to N centiseconds"
     ;;"otim N - set the opponents clock"
     "usermove MOVE - make given MOVE if legal"
     ;;"? - Tell Engine to stop thinking and make its move now"
     "ping N - Pings the engine for pong reply"
     ;;"draw - offer draw to engine"
     "result RESULT {COMMENTS} - give the game RESULT to engine"
     "setboard FEN - Set the game board to given FEN."
     ;;"edit - enable edit mode [obsolete]"
     ;;". - exit edit mode"
     "hint - prompt move hint from engine"
     ;;"bk - use book"
     "undo - tell engine to undo last move"
     "remove - tell engine to undo last two moves"
     ;;"hard - tell engine to ponder during players turn"
     ;;"easy - tell engine to ponder only during its turn"
     ;;"post - tell engine to send ponder output"
     ;;"nopost - tell engine not to send ponder output"
     ;;"analyse - tell engine to engage analyse mode"
     "name X - tell engine its opponents name"
     "rating - ask engine its rating"
     ;;"ics - tell engine its engaging in ICS game"
     "computer - tell engine that its playing against cpu"
     ;;"pause - pause all actions"
     ;;"resume - resume all paused actions"
     ;;"memory N - specify how much engine can use memory"
     ;;"cores N - tell engine how many cpu cores it can use"
     ;;"egtpath PATH - tell engine to use end-game tables from PATH"
     "option NAME[=VALUE] - tell engine to use new option")))

(defn xboard-accept-feature
  "Tells the engine that GUI accepts last feature."
  [])

(defn xboard-reject-feature
  "Tells the engine that GUI rejects given feature."
  [])

(defn xboard-move-now
  "Tells the Engine to stop thinking and pick move immidiately."
  [])

(defn xboard-ping
  "Tells XBoard to wait for all the stuff to complete given before this
   and once done, respond with pong"
  [n]
  (println (str "pong " n)))

(defn xboard-result
  "Sets the game result to engine, for learning purposes
   discarded by Tursas for now."
  [result])

(defn xboard-bk
  "Tells the XBoard to use Book"
  [])

(defn xboard-send-rating
  "Prompts the Engine to send its rating."
  []
  (println "100"))

(defn xboard-parse-option
  "Wrapper to parse options from string and set them."
  [option]
  (let [pair (s/split #"=" option)]
    (set-option! (keyword (first pair))
                 (if (= (count pair) 1)
                   true
                   (second pair)))))

(defn process-xboard-command
  "Processes command in xboard mode."
  [words]
  (case (first words)
        "protover" (do (set-option! :xboard-protocol-version (second words))
                       (xboard-print-supported-features))
        "accepted" (xboard-accept-feature)
        "rejected" (xboard-reject-feature)
        "new" (do (set-game! "startpos")
                  (set-option! :ai-mode false))
        "variant" (set-option! :variant (second words))
        "quit" (quit)
        "random" (toggle-option! :random-mode)
        "force" (set-option! :ai-mode false)
        "go" (set-option! :ai-mode true)
        ;; set playother=1 to enable
        ;;"playother" (xboard-playother)
        ;; set colors=1 to enable these
        ;;"white" (xboard-white)
        ;;"black" (xboard-black)
        ;;"level" (set-option! :level (rest words))
        ;;"st" (set-option! :time (second words))
        "sd" (set-option! :depth-limit (second words))
        ;;"nps" (set-option! :nps (rest words))
        ;; set time=1 to enable these
        ;;"time" (xboard-set-engine-clock (second words))
        ;;"otim" (xboard-set-opponent-clock (second words))
        "usermove" (make-chess-move (second words))
        ;;"?" (xboard-move-now)
        "ping" (xboard-ping (second words))
        ;; set draw=1 to enable
        ;;"draw" (xboard-draw)
        "result" (xboard-result (rest words))
        ;; setboard=0 to disable setboard and use edit words
        "setboard" (set-game! (second words))
        ;;"edit" (xboard-enter-edit-mode)
        ;;"." (xboard-exit-edit-mode)
        "hint" (get-hint)
        ;;"bk" (xboard-bk)
        "undo" (undo-move)
        "remove" (undo-move 2)
        ;;"hard" (set-option! :ponder true)
        ;;"easy" (set-option! :ponder false)
        ;;"post" (set-option! :ponder-output true)
        ;;"nopost" (set-option! :ponder-output false)
        ;; set analyse=1 to enable
        ;;"analyse" (xboard-analyse-mode)
        "name" (set-option! :opponent-name (second words))
        "rating" (xboard-send-rating)
        ;; set ics=1 to enable
        ;;"ics" (xboard-ics)
        "computer" (set-option! :opponent "cpu")
        ;; set pause=1 to enable
        ;;"pause" (xboard-pause)
        ;;"resume" (xboard-resume)
        ;; set memory=1 to enable
        ;;"memory" (xboard-set-memory (second words))
        ;; set smp=1
        ;;"cores" (xboard-set-cores (second words))
        ;; set egtpath to enable
        ;;"egtpath" (xboard-set-egtpath (second words))
        "option" (xboard-parse-option (second words))
        (when (move-string? (first words))
          (make-chess-move (first words)))))

;; uci
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
  (println "Registered name " name))

(defn- register-value
  "Registers given value."
  [value]
  (println "Registered value " value))

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

;; repl

(defn- print-usage
  "Prints the available commands of the repl."
  []
  (do (s/map-str
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
         ;;"uci - enable uci mode"
         "xboard - ebable xboard mode"
         "quit - quite the Tursas engine"
         ""))
      (let [prot (get-protocol)]
        (cond (= prot :uci) (print-uci-usage)
              (= prot :xboard) (print-xboard-usage)
              :else nil))))

(defn process-command
  "Processes command given by user."
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
          ;;"uci"  (set-repl! :uci)
          "xboard" (set-protocol! :xboard)
          "quit" (quit)
          (case (get-protocol)
                :general (when-not (empty? (rest words))
                           (recur (rest words)))
                :uci (process-uci-command words)
                :xboard (process-xboard-command words)))))

(defn set-player!
  "Sets the given player's repl.
   This determines if the player is AI or human."
  [player repl])

(defn- init-engine
  "Initializes the chess engine."
  []
  (do (println "# Welcome to Tursas Chess Engine!")
      (println "# Type 'help' to get list of supported commands")))

(defn -main
  "Starts the engine repl for input handling."
  [& args]
  (init-engine)
  (loop [input (read-line)]
    (process-command input)
    (recur (read-line))))
