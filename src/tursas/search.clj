(ns tursas.search
  (:use (tursas state)))

(defrecord TreeNode [label subtree])

(defn- make-applied-node
  "Helper function to create a new node with function f
   applied to label."
  [f label subtree]
  (TreeNode. (f label) subtree))

(defn- moves
  "Function to generate all child states for given state.
   Return nil of nil state given."
  [state]
  (when-not (nil? state)
    (legal-states state)))

(declare redtree)
(defn- redtree-
  "Utility function for redtree to work with lists."
  [f g a subtree]
  (lazy-seq
   (if-let [[x & xs] (seq subtree)]
     (g (redtree f g a x)
        (redtree- f g a xs))
     a)))

(defn- redtree
  "a 'Reduce' for trees.
   This function works only for tree composed of nodes.
   Uses utility function redtree- to work with lists."
  [f g a tree]
  (f (:label tree) (redtree- f g a (:subtree tree))))

(defn- maptree
  "Make new game tree out of node by applying f to all labels."
  [f tree]
  (redtree (partial make-applied-node f) cons nil tree))

(defn- reptree
  "Creates a tree of nodes from initial value of a by
   applying f to it.
   f should be a function of generating children of a."
  [f a]
  (TreeNode. a (lazy-seq (map (partial reptree f) (f a)))))

(defn- gametree
  "Generate infinite tree of nodes from given game state.
   Uses lazy evaluation to avoid evaluating this immidiately."
  [state]
  (reptree moves state))

(declare minimise)
(defn- maximise
  "Choose maximum value from tree."
  [tree]
  (if-let [subtree (seq (:subtree tree))]
    (apply max (map minimise subtree))
    (:label tree)))

(defn- minimise
  "Choose minimum value of tree."
  [tree]
  (if-let [subtree (seq (:subtree tree))]
    (apply min (map maximise subtree))
    (:label tree)))

(defn- prune
  "Limit given gametree to certain depth.
   If depth limit is reached, pruning will check if the
   game state is left in 'dynamic' state and continues search
   until the dynamic state is resolved."
  [depth tree]
  (TreeNode. (:label tree)
             (lazy-seq
              (if (pos? depth)
                (map (partial prune (dec depth)) (:subtree tree))
                (when (dynamic? (:label tree))
                  (map (partial prune 0) (:subtree tree)))))))

(defn- evaluate-with-minmax
  "Evaluates given game state with minmax-algorithm."
  [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       maximise))

(defn- min-leq?
  "Returns true if the minimum of the list of numbers given is less than or
   equal to the given potential maximum."
  [pot nums]
  (cond (empty? nums) false
        (<= (first nums) pot) true
        :else (recur pot (rest nums))))

(defn- omit
  "omit is passed a potential maximum - the largest minimum seen
   so far - and omits any minima which are less than this from nums."
  [pot nums]
  (cond (empty? nums) nums
        (min-leq? pot nums) (omit pot (rest nums))
        :else (let [minimum (apply min nums)]
                (cons minimum
                      (omit minimum (rest nums))))))

(defn- mapmin
  "Makes new list by omitting values."
  [nums]
  (let [minimum (apply min nums)]
    (cons minimum (omit minimum (rest nums)))))

(defn- mapmax
  "Makes new list by omitting values."
  [nums]
  (let [maximum (apply max nums)]
    (cons maximum (omit maximum (rest nums)))))

(declare minimise-)
(defn- maximise-
  "Choose maximum value from node."
  [node]
  (if-let [subtree (seq (:subtree node))]
    (mapmin (map minimise- subtree))
    (:label node)))

(defn- minimise-
  "Choose minimum value of node."
  [node]
  (if-let [subtree (seq (:subtree node))]
    (mapmax (map maximise- subtree))
    (:label node)))

(defn- evaluate-with-alpha
  "Evaluate game tree with alpha-beta."
  [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       maximise-
       (apply max)))

(defn- higher?
  "Predicate to see if first node is larger than second."
  [node-1 node-2]
  (> (:label node-1)
     (:label node-2)))

(declare lowfirst)
(defn- highfirst
  "Sorts subtree of tree by putting node with largest node first."
  [tree]
  (TreeNode. (:label tree)
             (sort higher? (map lowfirst (:subtree tree)))))

(defn- lowfirst
  "Sorts subtree of tree by placing lowest node first."
  [tree]
  (TreeNode. (:label tree)
             (sort (complement higher?) (map highfirst (:subtree tree)))))

(defn- evaluate-with-alpha-1
  "Alphabeta evaluation as before but improved so that
   it sorts tree nodes before evaluating them to get
   better scores sooner which will trigger cutoff's earlier."
  [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       highfirst
       maximise-
       (apply max)))

(defn- nodett
  "Limit nodes subtree of to at most n subnodes."
  [n label sub]
  (TreeNode. label (take n sub)))

(defn- taketree
  "Function replaces all the nodes in a tree with
   nodes with at most n subnodes."
  [n tree]
  (redtree (partial nodett n) cons nil tree))

(defn- evaluate-with-alpha-2
  "Alphabeta evaluating which sorts nodes while
   evaluating and looks at most 3 CPU moves to
   limit searching."
  [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       highfirst
       (taketree 3)
       maximise-
       (apply max)))

(defn evaluate [depth eval-fn state]
  (time (evaluate-with-minmax depth eval-fn state))
  ;;(time (evaluate-with-alpha depth eval-fn state))
  ;;(time (evaluate-with-alpha-1 depth eval-fn state))
  ;;(time (evaluate-with-alpha-2 depth eval-fn state))
  ;;(time (evaluate-with-alpha-3 depth eval-fn state))
  )
