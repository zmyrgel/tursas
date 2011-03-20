(ns tursas.core
  (:gen-class)
  (:require [clojure.contrib.string :as s])
  (:use (tursas engine)))

(defn- init-engine
  "Initializes the chess engine."
  []
  (s/map-str println
             '("# Welcome to Tursas Chess Engine!"
               "# Type 'help' to get list of supported commands")))

(defn- game-eval
  "Evaluates given engine protocol command."
  [s]
  (process-command s))

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
