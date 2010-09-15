(ns tursas.repl
  (:use [tursas.game])
  (:require [clojure.contrib.string :as string]))

(def active-repl (ref "general"))
(def xboard-default-features
     {"ping" 1
      "setboard" 1
      "playother" 1
      "san" 0
      "usermove" 1
      "time" 0 ;; once time works, change to 1
      "draw" 1
      "sigint" 0
      "sigterm" 0
      "reuse" 0 ;; test and change to 1
      "analyse" 0
      "myname" "\"Tursas 0.1\""
      "variants" "\"normal\""
      "colors" 0
      "ics" 0
      "name" 0
      "pause" 0
      "nps" 0
      "debug" 0
      "memory" 0
      "smp" 0
      ;;:egt
      "done" 1})

(defn- write
  "Writes given MSG to standard output and flushes it."
  [^String msg]
  (.write *out* (.concat msg "\n"))
  (flush))

(defn- xboard-send-default-features
  "Sends the default features of the engine."
  []
  (io!
   (map println
        (for [option (keys xboard-default-features)]
          (format "feature %s=%s" option (xboard-default-features option))))))

(defn supported-uci-options
  "Prints the supported options for Tursas."
  []
  (map #(str "option name " % "\n")
       '("Hash type spin default 1 min 1 max 32"
         "UCI_EngineAbout type string default Tursas by Timo Myyrä")))

(defn- print-uci-usage
  "Prints the available commands of the repl."
  []
  (io! (string/map-str println
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

(defn- print-xboard-usage
  "Prints the available commands of the repl."
  []
  (io! (string/map-str println
                       '("Available XBoard commands are:"
                         "protover N - change engine to use protocol version N"
                         "accepted - Accept last feature"
                         "reject - Reject last feature"
                         "variant VARIANT - change to use VARIANT rules. Only 'normal' supported"
                         "quit - Quit the engine"
                         "random - Tell engine to add little random elements"
                         "force - Disable engine AI"
                         "go - Enable engine AI"
                         ;;"playother - Tell AI to switch sides"
                         ;;"white - Tell white to move, engine to play black [obsolete]"
                         ;;"black - Tell black to move, engine to play white [obsolete]"
                         "level MPS BASE INC - set time controls"
                         "st TIME - set time controls"
                         "sd DEPTH - set search depth to DEPTH"
                         "nps NODE_RATE - search only NODE_RATE nodes"
                         ;;"time N - set the engine clock to N centiseconds"
                         ;;"otim N - set the opponents clock"
                         "usermove MOVE - make given MOVE if legal"
                         "? - Tell Engine to stop thinking and make its move now"
                         "ping N - Pings the engine for pong reply"
                         ;;"draw - offer draw to engine"
                         "result RESULT {COMMENTS} - give the game RESULT to engine"
                         "setboard FEN - Set the game state to given FEN."
                         ;;"edit - enable edit mode [obsolete]"
                         ;;". - exit edit mode"
                         "hint - prompt move hint from engine"
                         "bk - use book"
                         "undo - tell engine to undo last move"
                         "remove - tell engine to undo last two moves"
                         "hard - tell engine to ponder during players turn"
                         "easy - tell engine to ponder only during its turn"
                         "post - tell engine to send ponder output"
                         "nopost - tell engine not to send ponder output"
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
                         "option NAME[=VALUE] - tell engine to use new option"))))

(defn- print-usage
  "Prints the available commands of the repl."
  []
  (io! (string/map-str println
                       '("Available general commands:"
                         "help - display this help"
                         "load - load the last saved game from file"
                         "save - store the current game to file"
                         "bd - display the board on the screen"
                         "uci - enable uci mode"
                         "xboard - ebable xboard mode"
                         ""))
       (when (= @active-repl "uci")
         (print-uci-usage))
       (when (= @active-repl "xboard")
         (print-xboard-usage))))

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

(defn- process-xboard-command
  "Processes command in xboard mode."
  [command]
  (case command
        "protover" (do (xboard-set-option :protocol-version (second command))
                       (xboard-send-default-features))
        "accepted" (xboard-accept-feature)
        "rejected" (xboard-reject-feature)
        "variant" (xboard-set-option :variant (second command))
        "quit" (quit)
        "random" (if (= (xboard-get-option :random-mode) true)
                   (xboard-set-option :random-mode false)
                   (xboard-set-option :random-mode true))
        "force" (xboard-set-option :ai-mode false)
        "go" (xboard-set-option :ai-mode true)

        ;; set playother=1 to enable
        ;;"playother" (xboard-playother)

        ;; set colors=1 to enable these
        ;;"white" (xboard-white)
        ;;"black" (xboard-black)

        "level" (xboard-set-option :level (rest command))
        "st" (xboard-set-option :time (second command))
        "sd" (xboard-set-option :depth (second command))
        "nps" (xboard-set-option :nps (rest command))

        ;; set time=1 to enable these
        ;;"time" (xboard-set-engine-clock (second command))
        ;;"otim" (xboard-set-opponent-clock (second command))

        "usermove" (xboard-make-move (second command))
        "?" (xboard-move-now)
        "ping" (xboard-ping)

        ;; set draw=1 to enable
        ;;"draw" (xboard-draw)

        "result" (xboard-result (rest command))

        ;; setboard=0 to disable setboard and use edit command
        "setboard" (xboard-set-board (second command))
        ;;"edit" (xboard-enter-edit-mode)
        ;;"." (xboard-exit-edit-mode)

        "hint" (xboard-hint)
        "bk" (xboard-bk)
        "undo" (xboard-undo-move)
        "remove" (do (xboard-undo-move)
                     (xboard-undo-move))
        "hard" (xboard-set-option :ponder true)
        "easy" (xboard-set-option :ponder false)
        "post" (xboard-set-option :ponder-output true)
        "nopost" (xboard-set-option :ponder-output false)

        ;; set analyse=1 to enable
        ;;"analyse" (xboard-analyse-mode)

        "name" (xboard-set-option :opponent-name (second command))
        "rating" (xboard-send-rating)

        ;; set ics=1 to enable
        ;;"ics" (xboard-ics)

        "computer" (xboard-set-option :opponent "cpu")

        ;; set pause=1 to enable
        ;;"pause" (xboard-pause)
        ;;"resume" (xboard-resume)

        ;; set memory=1 to enable
        ;;"memory" (xboard-set-memory (second command))

        ;; set smp=1
        ;;"cores" (xboard-set-cores (second command))

        ;; set egtpath to enable
        ;;"egtpath" (xboard-set-egtpath (second command))

        "option" (xboard-parse-option (second command))
        nil))

(defn- process-uci-command
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
        "register"
        "ucinewgame"
        "position"
        "go"
        "stop"
        "ponderhit"
        "quit" (quit)
        nil))

(defn process-command
  "Processes command given by UI."
  [command]
  (loop [cmd (re-seq #"\w+" command)]
    (case (first cmd)
          "help" (print-usage)
          "load" (load-game (rest cmd))
          "save" (save-game (rest cmd))
          "bd" (display-board)
          "uci" (do (dosync (ref-set active-repl "uci"))
                    (process-uci-command cmd))
          "xboard" (do (dosync (ref-set active-repl "xboard"))
                       (process-xboard-command cmd))
          (case @active-repl
                "general" (if (not (empty? (rest cmd)))
                            (recur (rest cmd))
                            nil)
                "uci" (process-uci-command cmd)
                "xboard" (process-xboard-command cmd)))))




