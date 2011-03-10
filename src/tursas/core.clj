(ns tursas.core
  (:gen-class)
  (:require [clojure.contrib.string :as s]
            [clojure.contrib.seq :as seq]
            [matchure :as m])
  (:use (tursas search state move util)
        (tursas.state0x88 core move)))

(def startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def check-fen "r1bqkbnr/ppp1ppp1/n6p/1Q1p4/8/2P5/PP1PPPPP/RNB1KBNR b KQkq - 0 5")
(def cast-fen "r3k3/pp1qpppr/n1ppbn1p/8/2B5/BP1Q1P1N/P1P1P1PP/RN2K2R w KQq - 4 7")
(def prom-fen "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7")
(def mate-fen "3brr2/7b/8/2pN3Q/2p2k2/5P2/4P1KR/2N2RB1 b - - 1 18")
(def mate-1-fen "3brr2/2N4b/8/2p4Q/2p2k2/5P2/4P1KR/2N2RB1 w - - 1 17")
(def en-fen "rnbqkb1r/pppppppp/7n/P7/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 2")

(def inf Integer/MAX_VALUE)
(def -inf (+ Integer/MIN_VALUE 1))

(def cecp-supported-features {:ping 1 :setboard 1 :playother 1 :san 0
                              :usermove 0 :time 0 :draw 1 :sigint 0
                              :sigterm 0 :reuse 0 :analyse 0
                              :myname "\"Tursas 0.1\"" :variants "\"normal\""
                              :colors 0 :ics 0 :name 0 :pause 0 :nps 0
                              :debug 0 :memory 0 :smp 0 :done 1})

(def protocol (ref :general))
(def game-state (ref ()))
(def game-options (ref {:depth-limit 4 :random-mode false :ai-mode false
                        :cecp-protocol-version 1 :debug false :ponder false
                        :ponder-output false :movestogo 0 :white-increment 0
                        :black-increment 0 :move-limit nil :search-time 0}))

(defn- quit
  "Function to handle closing the engine."
  []
  (System/exit 0))

(defn- save-game
  "Saves the current game by writing game-state to file."
  []
  (binding [*out* (java.io.FileWriter. "saved-game.txt")]
    (prn @game-state)))

(defn- load-game
  "Loads the game-state from file to resume the previous game."
  []
  (try
    (let [state (read-string (slurp "saved-game.txt"))]
      (dosync (ref-set game-state state)))
    (catch Exception e nil)))

(defn- get-protocol
  "Returns currently active reader protocol"
  []
  @protocol)

(defn- set-protocol!
  "Sets the currently active reader protocol"
  [prot]
  (dosync (ref-set protocol prot)))

(defn- tursas-cmd
  "Wrapper for commands which use current game state."
  [msg f & args]
  (if (empty? (first @game-state))
    msg
    (if (nil? args)
      (f (first @game-state))
      (apply f (first @game-state) args))))

(defn- add-game-state!
  "Adds given state to game state."
  [new-state]
  (do (dosync (ref-set game-state (cons new-state @game-state)))
      nil))

(defn- display-board
  "Displays the current chess board in ASCII."
  [state]
  (str (print-board (state->fen state))
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
  (time (perft state (Integer/parseInt depth))))

(defn- list-moves
  "List all available moves from currect state."
  [state]
  (map #(move->coord %)
       (legal-moves state)))

(defn- get-score
  "Calculates state's score by checking child states
   to certain depth using alpha-beta algorithm."
  [state]
  (-> state
      (alpha-beta -inf inf (:depth-limit @game-options))
      first
      str))

(defn- eval-current-state
  "Evaluates the current state and prints its score."
  [state]
  (str (evaluate state)))

(defn- get-hint
  "Evaluates all states and chooses one from top five moves at random."
  [state]
  (-> state
      (alpha-beta -inf inf 2)
      second
      last-move
      move->coord))

(defn- set-game!
  "Sets game to given FEN state."
  [lst]
  (add-game-state!
   (let [fen (first lst)]
     (cond (= fen "startpos") (fen->state startpos)
           (= fen "check") (fen->state check-fen)
           (= fen "cast") (fen->state cast-fen)
           (= fen "prom") (fen->state prom-fen)
           (= fen "mate") (fen->state mate-fen)
           (= fen "bmate") (fen->state mate-1-fen)
           (= fen "en") (fen->state en-fen)
           :else (fen->state (s/join " " lst))))))

(defn- set-option!
  "Sets game option of given key to value."
  [k v]
  (do (dosync (alter game-options assoc k v))
      nil))

(defn- get-option
  "Returns value of given game option"
  [option]
  (@game-options option))

(defn- toggle-option!
  "Toggles the value of given boolean game option."
  [option]
  (set-option! option (not (get-option option))))

(defn- make-ai-move!
  "Make a n AI move."
  [state]
  (let [depth (get-option :depth-limit)]
    (do (add-game-state! (second (alpha-beta state -inf inf depth)))
        (str "move " (-> (first @game-state)
                         last-move
                         move->coord)))))

(defn- make-human-move!
  "If given string represents chess move, apply it to current game."
  [state s]
  (when (and (move-string? s)
             (allowed? state (coord->move s)))
    (when-let [new-state (apply-move state (coord->move s))]
      (do (add-game-state! new-state)
          true))))

(defn- user-move
  "Helper function to handle user and ai moves."
  [s]
  (if (nil? (make-human-move! (first @game-state) s))
    (str "Illegal move: " s)
    (if (game-end? (first @game-state))
      (result (first @game-state))
      (when (get-option :ai-mode)
        (let [move (make-ai-move! (first @game-state))]
          (if (game-end? (first @game-state))
            (s/join "\n" [move (result (first @game-state))])
            move))))))

(defn- undo-move!
  "Undo last move or if N given, N last moves."
  [& n]
  (do (dosync
       (ref-set game-state
                (if (nil? n)
                  (rest @game-state)
                  (nthnext @game-state n))))
      nil))

(defn- cecp-print-supported-features
  "Prints the default features of the engine."
  []
  (for [option (keys cecp-supported-features)]
    (format "feature %s=%s"
            (s/as-str option)
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
                  ;;"? - Tell Engine to stop thinking and make its move now"
                  "ping N - Pings the engine for pong reply"
                  "draw - offer draw to engine"
                  "RESULT {COMMENTS} - give the game RESULT to engine"
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

(defn- cecp-accept-feature
  "Tells the engine that GUI accepts last feature."
  [])

(defn- cecp-reject-feature
  "Tells the engine that GUI rejects given feature."
  [])

(defn- cecp-move-now
  "Tells the Engine to stop thinking and pick move immidiately."
  [])

(defn- cecp-ping
  "Tells Cecp to wait for all the stuff to complete given before this
   and once done, respond with pong"
  [n]
  (str "pong " n))

(defn- cecp-result
  "Sets the game result to engine, for learning purposes
   discarded by Tursas for now."
  [result])

(defn- cecp-draw
  "Offer draw to opponent."
  [state]
  (when (get-option :ai-mode)
    (str "1/2-1/2 {" (if (= (turn state) :white)
                       "WHITE"
                       "BLACK")
         " offered a draw!}")))

(defn- cecp-bk
  "Tells the Cecp to use Book"
  [])

(defn- cecp-send-rating
  "Prompts the Engine to send its rating."
  []
  "100")

(defn- cecp-parse-option
  "Wrapper to parse options from string and set them."
  [option]
  (let [pair (s/split #"=" option)]
    (set-option! (keyword (first pair))
                 (if (== (count pair) 1)
                   true
                   (second pair)))))

(defn- process-cecp-command
  "Processes command in cecp mode."
  [words]
  (case (first words)
        "protover" (do (set-option! :cecp-protocol-version (second words))
                       (cecp-print-supported-features))
        "accepted" (cecp-accept-feature)
        "rejected" (cecp-reject-feature)
        "new" (do (set-game! '("startpos"))
                  (set-option! :ai-mode true))
        "variant" (set-option! :variant (second words))
        "quit" (quit)
        "force" (set-option! :ai-mode false)
        "go" (set-option! :ai-mode true)
        ;;"playother" (cecp-playother)
        ;;"level" (set-option! :level (rest words))
        ;;"st" (set-option! :time (second words))
        "sd" (set-option! :depth-limit (second words))
        ;;"nps" (set-option! :nps (rest words))
        ;;"time" (cecp-set-engine-clock (second words))
        ;;"otim" (cecp-set-opponent-clock (second words))
        "usermove" (user-move (second words))
        ;;"?" (cecp-move-now)
        "ping" (cecp-ping (second words))
        "draw" (tursas-cmd "Can't offer draw to empty board!" cecp-draw)
        "result" (cecp-result (rest words))
        "setboard" (set-game! (rest words))
        "hint" (get-hint)
        ;;"bk" (cecp-bk)
        "undo" (undo-move!)
        "remove" (undo-move! 2)
        ;;"hard" (set-option! :ponder true)
        ;;"easy" (set-option! :ponder false)
        ;;"post" (set-option! :ponder-output true)
        ;;"nopost" (set-option! :ponder-output false)
        ;;"analyse" (cecp-analyse-mode)
        "name" (set-option! :opponent-name (second words))
        "rating" (cecp-send-rating)
        ;;"ics" (cecp-ics)
        "computer" (set-option! :opponent :cpu)
        ;;"pause" (cecp-pause)
        ;;"resume" (cecp-resume)
        ;;"memory" (cecp-set-memory (second words))
        ;;"cores" (cecp-set-cores (second words))
        ;;"egtpath" (cecp-set-egtpath (second words))
        "option" (cecp-parse-option (second words))
        (when (move-string? (first words))
          (user-move (first words)))))

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
            "gs - calculates score for the current game state"
            "es - evaluates current game state"
            "pf n - calculate perft score to depth of n"
            "xboard - enable CECP mode"
            "quit - quite the Tursas engine")
          (case (get-protocol)
                :general nil
                :cecp cecp-usage)))

(defn- process-command
  "Processes command given by user."
  [words]
  (case (first words)
        "help" (usage)
        "load" (load-game)
        "save" (save-game)
        "bd"   (tursas-cmd "Can't print empty board!" display-board)
        "fd" (tursas-cmd "Can't display FEN for empty state." display-fen)
        "lm" (tursas-cmd "Can't list moves from empty state." list-moves)
        "gs" (tursas-cmd "Can't calculate score from empty state." get-score)
        "cp" (do (tursas-cmd "Can't make AI move on empty board!" make-ai-move!)
                 (tursas-cmd "Can't print empty board!" display-board))
        "es" (tursas-cmd "Can't eval empty game state!" eval-current-state)
        "pf" (tursas-cmd "Can't calculate perft from game-state!" display-perft (second words))
        "xboard" (set-protocol! :cecp)
        "quit" (quit)
        (case (get-protocol)
              :general (when-not (empty? (rest words))
                         (recur (rest words)))
              :cecp (process-cecp-command words))))

(defn- init-engine
  "Initializes the chess engine."
  []
  (s/map-str println
             '("# Welcome to Tursas Chess Engine!"
               "# Type 'help' to get list of supported commands")))

(defn- game-eval
  "Evaluates given engine protocol command."
  [s]
  (process-command (re-seq #"\S+" s)))

(defn- game-read
  "Reader function to parse commandline.
   Reads user input as a string and converts it to sexp."
  []
  (read-line))

(defn- game-print
  "Prints prompt and responses to user."
  [output]
  (cond (seq? output) (s/map-str println output)
        (string? output) (println output)))

(defn -main
  "Starts the engine repl for input handling."
  [& args]
  (init-engine)
  (loop []
    (-> (game-read)
        game-eval
        game-print)
    (recur)))
