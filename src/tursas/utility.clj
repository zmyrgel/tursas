(ns tursas.utility)

(defn write
  "Writes given MSG to standard output and flushes it."
  [^String msg]
  (.write *out* (.concat msg "\n"))
  (flush))

(defn set-option
  "Sets the option given as command"
  [args] ;; name <id> [value <x>]
  args)

(defn send-command
  "Sends given command."
  [command]
  (write command))

(defn quit
  "Function to handle closing the engine."
  []
  (System/exit 0))

