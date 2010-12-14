
(defun tree-reduce (f tree)
  (lazy-seq
   (when-let [[x & xs] (seq tree)]
     (f x (map (partial #'tree-reduce f) xs)))))

(defun maptree (f tree)
  (tree-reduce (partial #'(lambda (f x y)
                            (cons (f x) y)) f) tree))

(defun reptree (f a)
  (lazy-seq (cons a (map (partial #'reptree f) (f a)))))

(defun gametree (state)
  (reptree #'legal-states state))

(defun maximise (tree)
  (if-let [subtree (seq (rest tree))]
          (apply max (map minimise subtree))
          (first tree)))

(defun minimise (tree)
  (if-let [subtree (seq (rest tree))]
    (apply min (map maximise subtree))
    (first tree)))

(defun prune (depth tree)
  (lazy-seq
   (cons (first tree)
         (if (pos? depth)
           (map (partial prune (dec depth)) (rest tree))
           (when (dynamic? (first tree))
             (map (partial prune 0) (rest tree)))))))

(defun evaluate-with-minmax (depth eval-fn state)
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       maximise))

(defun evaluate (depth eval-fn state)
  (evaluate-with-minmax depth eval-fn state))
