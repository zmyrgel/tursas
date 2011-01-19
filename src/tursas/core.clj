(ns tursas.core
  (:gen-class)
  (:use (tursas repl)))

(defn -main
  "Starts the engine repl for input handling."
  [& args]
  (println "# Welcome to Tursas Chess Engine!")
  (println "# Type 'help' to get list of supported commands")
  (loop [input (read-line)]
    (process-command input)
    (recur (read-line))))
