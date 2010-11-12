(ns tursas.search
  (:require [clojure.walk :as w])
  (:use (tursas state eval)))

;;; Based on 'Why Functional Programming Matters' paper
;;; http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf
;;; Uses minimax algorithm for now, to be expanded to alphabeta later on

(defn- moves
  "Generates list of nodes representing possible game states reachable
  from given gametree NODE."
  [node]
  (map #(cons % nil) (legal-states (first node))))

(defn- maptree
  "Apply f to all elements in form."
  [f tree]
  (w/walk #(lazy-seq (maptree f %)) identity tree))

(defn- tree-node?
  "Checks if X is tree node or not."
  [node]
  (list? node))

(defn- gametree
  "Create a full gametree from STATE.
   For obvious reasons, this need to be lazy."
  [state]
  (tree-seq tree-node? moves (cons state nil)))

(defn- static
  "Evaluates the given gametree NODE."
  [node]
  (evaluate-state (first node)))

(declare minimise)
(defn- maximise
  "Searches the maximum score from subtree"
  [node]
  (if (nil? (rest node))
    (:score (first node))
    #(max (map minimise (rest node)))))

(defn- minimise
  "Searches the minimum score from subtree"
  [node]
  (if (nil? (rest node))
    (:score (first node))
    #(min (map maximise (rest node)))))

(defn- prune-depth
  "Prunes the results of gametree search.
   Limit the search to certain DEPTH to complete the search
   in adequote time frame.
   Don't prune if depth is nil."
  [depth node]
  (if (nil? depth)
    node
    (cons node (when (not (zero? depth))
                 (map (partial prune-depth (dec depth)) (next node))))))

(defn- prune-nodes
  "Prune game tree based on number of nodes processed.
   No pruning if nil given."
  [nodes node]
  (if (nil? nodes)
    node
    (cons node (when (not (zero? nodes))
                 (map (partial prune-nodes (dec nodes)) (rest node))))))

(defn evaluate
  "Evaluates given STATE to certain DEPTH.
   Returns new state with evaluation score attached"
  [depth nodes state]
  (->> state
       gametree
       (prune-nodes nodes)
       (prune-depth depth)
       (maptree static)
       (trampoline maximise)))
