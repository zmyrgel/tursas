(ns tursas.utility)

(defn write
  "Writes given MSG to standard output and flushes it."
  [^String msg]
  (.write *out* (.concat msg "\n"))
  (flush))

(defn send-command
  "Sends given command."
  [command]
  (write command))

