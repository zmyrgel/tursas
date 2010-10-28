(ns tursas.search
  (:use (tursas state)))

;;; Based on 'Why Functional Programming Matters' paper
;;; http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf

;; leaf = game position
;; subtree = all possible game positions achievable with a single move from leaf
(defrecord Node [leaf subtree])

(defn moves
  "Generates list of nodes representing possible game states reachable
  from given gametree NODE."
  [node]
  (map #(Node. % nil) (legal-states (:leaf node))))

(defn maptree
  "Apply f to all elements in form."
  [f tree]
  (walk #(lazy-seq (maptree f %)) identity tree))

(defn tree-node?
  "Checks if X is tree node or not."
  [x]
  (instance? x Node))

(defn gametree
  "Create a full gametree from STATE.
   For obvious reasons, this need to be lazy."
  [state]
  (tree-seq tree-node? moves (Node. state nil)))

(defn static
  "Evaluates the given gametree NODE."
  [node]
  (evaluate-state (:leaf node)))

(defn maximise
  "Searches the maximum score from subtree"
  [node]
  (if (nil? (:subtree node))
    (:leat node)
    #(max (map minimise (:subtree node)))))

(defn minimise
  "Searches the minimum score from subtree"
  [node]
  (if (nil? (:subtree node))
    (:leaf node)
    #(min (map maximise (:subtree node)))))

(defn prune
  "Prunes the results of gametree search.
   Limit the search to certain DEPTH to complete the search
   in adequote time frame."
  [depth node]
  (assoc node (:subtree node)
         (when (not (zero? depth))
           (map (recur (dec depth) (:subtree node)))))))

;; evaluate = maximise . maptree static . prune 5 . gametree
(def evaluate (comp (trampoline maximise)
                    (maptree static)
                    (prune 5)
                    gametree))
