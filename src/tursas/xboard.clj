(ns tursas.xboard
  (:use [clojure.contrib.string :only [as-str map-str split]]
        [tursas.game :only [set-option!
                            set-game!
                            toggle-option!
                            quit
                            make-chess-move
                            get-hint
                            undo-move]]))

(def xboard-supported-features
  {:ping 1
   :setboard 1
   :playother 1
   :san 0
   :usermove 1
   :time 0
   :draw 1
   :sigint 0
   :sigterm 0
   :reuse 0
   :analyse 0
   :myname "\"Tursas 0.1\""
   :variants "\"normal\""
   :colors 0
   :ics 0
   :name 0
   :pause 0
   :nps 0
   :debug 0
   :memory 0
   :smp 0
   :done 1})

(defn xboard-print-supported-features
  "Prints the default features of the engine."
  []
  (dorun (map println
              (for [option (keys xboard-supported-features)]
                (format "feature %s=%s"
                        (as-str option)
                        (xboard-supported-features option))))))

(defn print-xboard-usage
  "Prints the available commands of the repl."
  []
  (map-str
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
     "setboard FEN - Set the game board to given FEN."
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
  (io! (println (str "pong " n))))

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
  (io! (println "100")))

(defn xboard-parse-option
  "Wrapper to parse options from string and set them."
  [option]
  (let [pair (split #"=" option)]
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

        "level" (set-option! :level (rest words))
        "st" (set-option! :time (second words))
        "sd" (set-option! :depth-limit (second words))
        "nps" (set-option! :nps (rest words))

        ;; set time=1 to enable these
        ;;"time" (xboard-set-engine-clock (second words))
        ;;"otim" (xboard-set-opponent-clock (second words))

        "usermove" (make-chess-move (second words))
        "?" (xboard-move-now)
        "ping" (xboard-ping (second words))

        ;; set draw=1 to enable
        ;;"draw" (xboard-draw)

        "result" (xboard-result (rest words))

        ;; setboard=0 to disable setboard and use edit words
        "setboard" (set-game! (second words))
        ;;"edit" (xboard-enter-edit-mode)
        ;;"." (xboard-exit-edit-mode)

        "hint" (get-hint)
        "bk" (xboard-bk)
        "undo" (undo-move)
        "remove" (undo-move 2)
        "hard" (set-option! :ponder true)
        "easy" (set-option! :ponder false)
        "post" (set-option! :ponder-output true)
        "nopost" (set-option! :ponder-output false)

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
        nil
        ))
