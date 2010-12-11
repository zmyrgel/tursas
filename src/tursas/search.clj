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
