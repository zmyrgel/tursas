(ns tursas.core
  (:gen-class)
  (:require [matchure :as m])
  (:use (tursas search state move util)
        (tursas.state0x88 core move)))

(def inf Integer/MAX_VALUE)
(def -inf (+ Integer/MIN_VALUE 1))

(def cecp-supported-features {:ping 1 :setboard 1 :playother 1 :san 0
                              :usermove 0 :time 0 :draw 1 :sigint 0
                              :sigterm 0 :reuse 0 :analyse 0
                              :myname "\"Tursas 0.1\"" :variants "\"normal\""
                              :colors 0 :ics 0 :name 0 :pause 0 :nps 0
                              :debug 0 :memory 0 :smp 0 :done 1})

(def ^:dynamic *protocol* (ref :general))
(def ^:dynamic *game-state* (ref ()))
(def ^:dynamic *game-options* (ref {:depth-limit 4 :ai-mode false
                                    :cecp-protocol-version 2 :debug false :ponder false
                                    :ponder-output false}))

(defn- quit
  "Function to handle closing the engine."
  []
  (System/exit 0))

(defn- current-game-state
  "Utility to return current game state or nil."
  []
  (first @*game-state*))

(defn- set-option!
  "Sets game option of given key to value."
  [k v]
  (do (dosync (alter *game-options* assoc k v))
      nil))

(defn- get-option
  "Returns value of given game option"
  [option]
  (@*game-options* option))

(defn- save-game
  "Saves the current game by writing game-state to file."
  []
  (binding [*out* (java.io.FileWriter. "saved-game.txt")]
    (prn @*game-state*)))

(defn- load-game
  "Loads the game-state from file to resume the previous game."
  []
  (try
    (let [state (read-string (slurp "saved-game.txt"))]
      (dosync (ref-set *game-state* state)))
    (catch Exception e nil)))

(defn- get-protocol
  "Returns currently active reader protocol"
  []
  @*protocol*)

(defn- set-protocol!
  "Sets the currently active reader protocol"
  [prot]
  (dosync (ref-set *protocol* prot)))

(def search-fn (partial alpha-beta -inf inf (get-option :depth-limit) evaluate))
(def shallow-search-fn (partial alpha-beta -inf inf 2 evaluate))

(defn- tursas-cmd
  "Wrapper for commands which use current game state."
  [msg f & args]
  (if (empty? (current-game-state))
    (str "Error (" msg ")")
    (apply f (current-game-state) args)))

(defn- add-game-state!
  "Adds given state to game state."
  [new-state]
  (do (dosync (alter *game-state* conj new-state))
      nil))

(defn- display-board
  "Displays the current chess board in ASCII."
  [state]
  (str (fen->ascii (state->fen state))
       (if (= (turn state) :white) "  WHITE" "  BLACK")
       " TO MOVE"
       (cond (mate? state) (if (= (turn state) :white)
                             ", WHITE IN MATE!"
                             ", BLACK IN MATE!")
             (check? state) (if (= (turn state) :white)
                              ", WHITE IN CHECK!"
                              ", BLACK IN CHECK!"))))

(defn- display-fen
  "Display FEN of currect game state."
  [state]
  (state->fen state))

(defn- display-perft
  "Display Perft of given depth."
  [state depth]
  (time (perft state depth)))

(defn- list-moves
  "List all available moves from currect state."
  [state]
  (map move->coord (legal-moves state)))

(defn- get-score
  "Calculates state's score by checking child states
   to certain depth using alpha-beta algorithm."
  [state]
  (->> state
       search-fn
       first
       str))

(defn- eval-current-state
  "Evaluates the current state and returns its score."
  [state]
  (str (evaluate state)))

(defn- get-hint
  "Evaluates all states and chooses one from top five moves at random."
  [state]
  (-> state
      shallow-search-fn
      second
      last-move
      move->coord))

(defn- set-game!
  "Sets game to given FEN state.
   Command also supports some pre-defined states, mainly for debugging."
  [s]
  (let [startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        check-fen "r1bqkbnr/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5"
        cast-fen "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7"
        prom-fen "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7"
        mate-fen "3brr2/7b/8/2pN3Q/2p2k2/5P2/4P1KR/2N2RB1 b - - 1 18"
        mate-1-fen "3brr2/2N4b/8/2p4Q/2p2k2/5P2/4P1KR/2N2RB1 w - - 1 17"
        en-fen "rnbqkb1r/pppppppp/7n/P7/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 2"]
    (if-let [fen (m/cond-match s
                               #"^startpos$" startpos
                               #"^check$" check-fen
                               #"^cast$" cast-fen
                               #"^prom$" prom-fen
                               #"^mate$" mate-fen
                               #"^bmate$" mate-1-fen
                               #"^en$" en-fen
                               #"^[1-8prnbqkPRNBQK/]{15,71}\s[wb]{1}\s[KQkq-]{1,4}\s[a-h1-8-]{1,2}\s\d+\s\d+$" s
                               ? nil)]
      (add-game-state! (fen->state fen))
      (str "Error (Invalid setboard command): " s))))

(defn- toggle-option!
  "Toggles the value of given boolean game option."
  [option]
  (set-option! option (not (get-option option))))

(defn- make-ai-move!
  "Tell engine to make an move in current game state."
  [state]
  (do (add-game-state! (second (search-fn state)))
      (str "move " (-> (current-game-state)
                       last-move
                       move->coord))))

(defn- make-human-move!
  "If given string represents chess move, apply it to current game."
  [state s]
  (when (and (move-string? s)
             (allowed? state (coord->move s)))
    (when-let [new-state (apply-move state (coord->move s))]
      (do (add-game-state! new-state)
          true))))

(defn- user-move
  "Helper function to handle user and ai moves.
   State arg included to avoid user moves when game is not set."
  [state s]
  (if (nil? (make-human-move! state s))
    (str "Illegal move: " s)
    (if (game-end? (current-game-state))
      (result (current-game-state))
      (when (get-option :ai-mode)
        (let [move (make-ai-move! (current-game-state))]
          (if (game-end? (current-game-state))
            (apply str (interpose "\n" [move (result (current-game-state))]))
            move))))))

(defn- undo-move!
  "Undo last move or if N given, N last moves."
  [& n]
  (do (dosync
       (ref-set *game-state*
                (if (nil? n)
                  (rest @*game-state*)
                  (nthnext @*game-state* n))))
      nil))

(defn- list-cecp-supported-features
  "Prints the default features of the engine."
  []
  (for [option (keys cecp-supported-features)]
    (format "feature %s=%s"
            (apply str (drop 1 (str option)))
            (cecp-supported-features option))))

(def cecp-usage '(""
                  "Available Cecp commands are:"
                  "protover N - change engine to use protocol version N"
                  "accepted - Accept last feature"
                  "rejected - Reject last feature"
                  "new - Sets the board to the chess starting position. Set White on move. Leave force mode and set the engine to play Black."
                  "variant VARIANT - change to use VARIANT rules. Only 'normal' supported"
                  "force - Disable engine AI"
                  "go - Enable engine AI"
                  ;;"playother - Tell AI to switch sides"
                  ;;"level MPS BASE INC - set time controls"
                  ;;"st TIME - set time controls"
                  "sd DEPTH - set search depth to DEPTH"
                  ;;"nps NODE_RATE - search only NODE_RATE nodes"
                  ;;"time N - set the engine clock to N centiseconds"
                  ;;"otim N - set the opponents clock"
                  "usermove MOVE - make given MOVE if legal"
                  "MOVE - make given MOVE if legal"
                  ;;"? - Tell Engine to stop thinking and make its move now"
                  "ping N - Pings the engine for pong reply"
                  "draw - offer draw to engine"
                  "result RESULT {COMMENTS} - give the game RESULT to engine, discarded currently"
                  "setboard FEN - Set the game board to given FEN."
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
                  "option NAME[=VALUE] - tell engine to use new option"))

(defn- cecp-draw
  "Offer draw to opponent."
  [state]
  (when (get-option :ai-mode)
    (str "1/2-1/2 {" (if (= (turn state) :white)
                       "WHITE"
                       "BLACK")
         " offered a draw!}")))

(defn- cecp-parse-option
  "Wrapper to parse options from string and set them."
  [option]
  (let [[key value] (.split #"=" option)]
    (set-option! (keyword key)
                 (if (nil? value)
                   true
                   value))))

(defn- unsupported-command
  "Utility to return error message for unsupported commands."
  [cmd]
  (str "Error (Unsupported command): " cmd))

(defn- process-cecp-command
  "Processes command in cecp mode."
  [cmd]
  (m/cond-match cmd
                #"^protover \d$" (do (set-option! :cecp-protocol-version (Integer/parseInt (apply str (drop 9 cmd))))
                                     (list-cecp-supported-features))
                #"^accepted$" nil ;; no-op
                #"^rejected$" nil ;; no-op
                #"^random$" nil   ;; no-op
                #"^new$" (do (set-game! "startpos")
                             (set-option! :ai-mode true))
                #"^variant normal$" (set-option! :variant (apply str (drop 8 cmd)))
                #"^variant \w+$" (str "Error (unsupported variant given): " (apply str (drop 8 cmd)))
                #"^force$" (set-option! :ai-mode false)
                #"^go$" (set-option! :ai-mode true)
                #"^playother$" (unsupported-command cmd) ;; (cecp-playother)
                #"^level \d+ [0-9:]+ \d+$" (unsupported-command cmd) ;; (set-option! :level (apply str (drop 6 cmd)))
                #"^st \d+$" (unsupported-command cmd) ;; (set-option! :time (Integer/parseInt (apply str (drop 3 cmd))))
                #"^sd \d+$" (set-option! :depth-limit (apply str (drop 3 cmd)))
                #"^nps \d+$" (unsupported-command cmd) ;; (set-option! :nps (Integer/parseInt (apply str (drop 4 cmd))))
                #"^time \d+$" (unsupported-command cmd) ;; (set-option! :engine-clock (Integer/parseInt (apply str (drop 5 cmd))))
                #"^otim \d+$" (unsupported-command cmd) ;; (set-option! :opponent-clock (Integer/parseInt (apply str (drop 5 cmd))))
                #"^usermove [a-h]{1}[1-8]{1}[a-h]{1}[1-8]{1}[rnbq]?+$" (tursas-cmd "Can't make move in a empty board!"
                                                                                   user-move (apply str (drop 9 cmd)))
                #"^[a-h]{1}[1-8]{1}[a-h]{1}[1-8]{1}[rnbq]?+$" (tursas-cmd "Can't make move in a empty board!" user-move cmd)
                #"^\?$" (unsupported-command cmd) ;; (cecp-move-now)
                #"^ping \d+$" (str "pong " (Integer/parseInt (apply str (drop 5 cmd))))
                #"^draw$" (tursas-cmd "Can't offer draw to empty board!" cecp-draw)
                #"^result (1/2-1/2 \{.+\}|1-0 \{.+\}|0-1 \{.+\}|\*)$" nil ;; no-op
                #"^setboard" (set-game! (apply str (drop 9 cmd)))
                #"^hint$" (tursas-cmd "Can't print hint from a empty board!" get-hint)
                #"^bk$" (unsupported-command cmd) ;; (cecp-bk)
                #"^undo$" (undo-move!)
                #"^remove$" (undo-move! 2)
                #"^hard$" (unsupported-command cmd) ;; (set-option! :ponder true)
                #"^easy$" (unsupported-command cmd) ;; (set-option! :ponder false)
                #"^post$" (unsupported-command cmd) ;; (set-option! :ponder-output true)
                #"^nopost" (unsupported-command cmd) ;; (set-option! :ponder-output false)
                #"^analyse$" (unsupported-command cmd) ;; (cecp-analyse-mode)
                #"^name \w+$" (set-option! :opponent-name (apply str (drop 5 cmd)))
                #"^rating$" "100"
                #"^ics$" (unsupported-command cmd) ;; (cecp-ics)
                #"^computer$" (set-option! :opponent :cpu)
                #"^pause$" (unsupported-command cmd) ;;(cecp-pause)
                #"^resume$" (unsupported-command cmd) ;; (cecp-resume)
                #"^memory \d+$" (unsupported-command cmd) ;; (set-option! :memory-limit (Integer/parseInt (apply str (drop 7 cmd))))
                #"^cores \d+$" (unsupported-command cmd) ;;(set-option! :core-limit (Integer/parseInt (apply str (drop 6 cmd))))
                #"^egtpath [\w\\/]+$" (unsupported-command cmd) ;;(set-option! :egtpath (apply str (drop 8 cmd)))
                #"^option\s\w=\".+\"|\d+$" (cecp-parse-option (apply str (drop 7 cmd)))
                ? (str "Error (Invalid command): " cmd)))

(defn- usage
  "Prints the available commands of the repl."
  []
  (concat '("Available general commands:"
            "help - display this help"
            "load - load the last saved game from file"
            "save - store the current game to file"
            "bd - display the board on the screen"
            "fd - display current game state in FEN"
            "lm - print a list of all available moves"
            ;;"gs - calculates score for the current game state"
            ;;"es - evaluates current game state"
            ;;"pf n - calculate perft score to depth of n"
            "xboard - enable CECP mode"
            "quit - quite the Tursas engine")
          (case (get-protocol)
                :general nil
                :cecp cecp-usage)))

(defn process-command
  "Processes command given by user."
  [command]
  (m/cond-match command
                #"^help$" (usage)
                #"^load$" (load-game)
                #"^save$" (save-game)
                #"^bd$" (tursas-cmd "Can't print empty board!" display-board)
                #"^fd$" (tursas-cmd "Can't display FEN for empty state." display-fen)
                #"^lm$" (tursas-cmd "Can't list moves from empty state." list-moves)
                #"^gs$" (tursas-cmd "Can't calculate score from empty state." get-score)
                #"^cp$" (do (tursas-cmd "Can't make AI move on empty board!" make-ai-move!)
                            (tursas-cmd "Can't print empty board!" display-board))
                #"^es$" (tursas-cmd "Can't eval empty game state!" eval-current-state)
                #"^pf \d+$" (tursas-cmd "Can't calculate perft from empty game-state!"
                                        display-perft (Integer/parseInt (apply str (drop 3 command))))
                #"^xboard$" (set-protocol! :cecp)
                #"^quit$" (quit)
                ? (if (= (get-protocol) :cecp)
                    (process-cecp-command command)
                    (str "Error (Invalid command): " command))))

(defn- game-print
  "Prints prompt and responses to user."
  [output]
  (cond (seq? output)
        (doseq [line output]
          (println line))
        (string? output)
        (println output)))

(defn -main
  "Starts the engine repl for input handling."
  [& args]
  (println "# Welcome to Tursas Chess Engine!")
  (println "# Type 'help' to get list of supported commands")
  (loop []
    (-> (read-line)
        process-command
        game-print)
    (recur)))
