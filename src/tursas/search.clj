(ns tursas.search
  (:use (tursas state)))

;;; Based on 'Why Functional Programming Matters' paper
;;; http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf

(defn moves
  "Generates list of nodes representing possible game states reachable
  from given gametree NODE."
  [node]
  (map #(cons % nil) (legal-states (first node))))

(defn maptree
  "Apply f to all elements in form."
  [f tree]
  (walk #(lazy-seq (maptree f %)) identity tree))

(defn tree-node?
  "Checks if X is tree node or not."
  [x]
  (instance? x (list? node)))

(defn gametree
  "Create a full gametree from STATE.
   For obvious reasons, this need to be lazy."
  [state]
  (tree-seq tree-node? moves (cons state nil)))

(defn static
  "Evaluates the given gametree NODE."
  [node]
  (evaluate-state (first node)))

(defn maximise
  "Searches the maximum score from subtree"
  [node]
  (if (nil? (rest node))
    (first node)
    #(max (map minimise (rest node)))))

(defn minimise
  "Searches the minimum score from subtree"
  [node]
  (if (nil? (rest node))
    (first node)
    #(min (map maximise (rest node)))))

(defn prune
  "Prunes the results of gametree search.
   Limit the search to certain DEPTH to complete the search
   in adequote time frame."
  [depth node]
  (cons node (when (not (zero? depth))
               (map (recur (dec depth) (rest node)))))))

;; evaluate = maximise . maptree static . prune 5 . gametree
(def evaluate (comp (trampoline maximise)
                    (maptree static)
                    (prune 5)
                    gametree))
