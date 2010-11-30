(ns tursas.search
  (:use (tursas state eval state0x88)))

(defn- moves
  "Generates nested list of nodes representing possible game states reachable
  from given gametree NODE or nil if no subtree available.
  ((node1) (node2) (node3) (node4))"
  [node]
  (lazy-seq
   (when (not (nil? node))
     (legal-states node))))

(defn walk
  "Traverses a tree form. inner and outer are functions.
   Applies inner to each element of form, building up a
   data structure of the same type, then applies outer to the result.
   Recognizes lists and seqs. Consumes seqs as with doall.
   Stripped down version of clojure.walk/walk"
  [inner outer tree]
  (cond
   (list? tree) (outer (apply list (map inner tree)))
   (seq? tree) (outer (doall (map inner tree)))
   :else (outer tree)))

(defn maptree
  "Performs a depth-first, post-order traversal of form. Calls f on
   each sub-form, uses f's return value in place of the original."
  [f tree]
  (walk (partial maptree f) f tree))

(defn- tree-node?
  "Predicate to check if given node is a
   tree node or not."
  [node]
  (satisfies? State node))

(defn- gametree
  "Create a full gametree from STATE.
   For obvious reasons, this need to be lazy.
   (root (child1 (grandchild1) (grandchild2)) (child2) (child3 (grandchild4)))"
  [state]
  (tree-seq tree-node? moves state))

(defn- static
  "Evaluates heuristic value for
   the given gametree NODE."
  [node]
  (evaluate-state node))

(declare minimise)
(defn- maximise
  "Searches the maximum score from subtree"
  [node]
  (if (nil? (rest node))
    (first node)
    #(max (map minimise (rest node)))))

(defn- minimise
  "Searches the minimum score from subtree"
  [node]
  (if (nil? (rest node))
    (first node)
    #(min (map maximise (rest node)))))

(defn- prune
  "Return a lazy sequence of the nodes in game tree until depth is met
   or when we hit tree end."
  [depth tree]
  (lazy-seq
   (when (pos? depth)
     (when-let [s (seq tree)]
       (cons (first s) (prune (dec depth) (rest s)))))))

(defn evaluate
  "Evaluates given STATE to certain DEPTH.
   Returns new state with evaluation score attached"
  [depth state]
  (->> state
       gametree
       (prune depth)
       (maptree static)
       (trampoline maximise)
       ))
