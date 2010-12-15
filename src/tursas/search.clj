(ns tursas.search
  (:use (tursas state)))

(defn- tree-reduce
  "Apply function f to each label of tree."
  [f tree]
  (when-let [[x & xs] (seq tree)]
    (lazy-seq (f x (map (partial tree-reduce f) xs)))))

(defn- maptree
  "Make new gametree by applying f to all labels of current tree."
  [f tree]
  (lazy-seq (tree-reduce (partial (fn [f x y] (cons (f x) y)) f) tree)))

(defn- reptree
  "Creates a tree of nodes from initial value of a by
   applying f to it.
   f should be a function of generating children of a."
  [f a]
  (lazy-seq (cons a (map (partial reptree f) (f a)))))

(defn- gametree
  "Generate infinite tree of nodes from given game state.
   Uses lazy evaluation to avoid evaluating this immidiately."
  [state]
  (reptree legal-states state))

(declare minimise)
(defn- maximise
  "Choose maximum value from tree."
  [tree]
  (if-let [subtree (seq (rest tree))]
    (apply max (map minimise subtree))
    (first tree)))

(defn- minimise
  "Choose minimum value of tree."
  [tree]
  (if-let [subtree (seq (rest tree))]
    (apply min (map maximise subtree))
    (first tree)))

(defn- prune
  "Limit given gametree to certain depth.
   If depth limit is reached, pruning will check if the
   game state is left in 'dynamic' state and continues search
   until the dynamic state is resolved."
  [depth tree]
  (lazy-seq
   (cons (first tree)
         (if (pos? depth)
           (map (partial prune (dec depth)) (rest tree))
           (when (dynamic? (first tree))
             (map (partial prune 0) (rest tree)))))))

(defn minmax
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
  (cond (empty? nums) nil
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
  [tree]
  (if-let [subtree (seq (rest tree))]
    (mapmin (map minimise- subtree))
    (first tree)))

(defn- minimise-
  "Choose minimum value of node."
  [tree]
  (if-let [subtree (seq (rest tree))]
    (mapmax (map maximise- subtree))
    (first tree)))

(defn- alphabeta
  "Evaluate game tree using the alpha-beta search."
  [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       maximise-
       (apply max)))

(defn- higher?
  "Predicate to see if first tree is larger than second."
  [tree1 tree2]
  (> (first tree1)
     (first tree2)))

(declare lowfirst)
(defn- highfirst
  "Sorts subtree of tree by placing largest node first."
  [tree]
  (lazy-seq (cons (first tree)
                  (sort higher? (map lowfirst (rest tree))))))

(defn- lowfirst
  "Sorts subtree of tree by placing lowest node first."
  [tree]
  (lazy-seq (cons (first tree)
                  (sort (complement higher?)
                        (map highfirst (rest tree))))))

(defn- alphabeta-mark-1
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
  (lazy-seq (cons label (take n sub))))

(defn- taketree
  "Function replaces all the nodes in a tree with
   nodes with at most n subnodes."
  [n tree]
  (tree-reduce (partial nodett n) tree))

(defn alphabeta-mark-2
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
