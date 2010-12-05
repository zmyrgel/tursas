(ns tursas.search
  (:use (tursas state eval state0x88)))

(defrecord Node [label subtree])

(defn- make-applied-node [f label subtree]
  (do (println "###")
      (print "MAKE-APPLIED-NODE[f label subtree]:")
      (prn f label subtree)
      (println "###"))
  (Node. (f label) subtree))

(defn- moves [state]
  (do (println "###")
      (print "MOVES[state]: ")
      (prn state)
      (println "###")
      (when (not (nil? state))
        (legal-states state))))

(declare redtree)
(defn- redtree- [f g a subtree]
  (if (empty? (first subtree))
    a
    (g (redtree f g a (first subtree))
       (redtree- f g a (rest subtree)))))

(defn- redtree [f g a node]
  (f (:label node) (redtree- f g a (:subtree node))))

(defn- maptree [f node]
  (redtree (partial make-applied-node f) cons nil node))

(defn- reptree [f a]
  (do (println "###")
      (print "REPTREE[f a]: ")
      (prn f a)
      (println "###")
      (Node. a
             (lazy-seq (map (partial reptree f) (f a))))))

(defn- gametree [state]
  (do (println "###")
      (print "GAMETREE[state]:")
      (prn state)
      (println "###")
      (reptree moves state)))

(defn- static [state]
  (do (println "###")
      (print "STATIC[state]:")
      (prn state)
      (println "###")
      (evaluate-state state)))

(declare minimise)
(defn- maximise [node]
    (if (empty? (:subtree node))
      (:label node)
      (apply max (map minimise (:subtree node)))))

(defn- minimise [node]
      (if (empty? (:subtree node))
        (:label node)
        (apply min (map maximise (:subtree node)))))

(defn- prune [depth node]
  (do (println "###")
      (print "PRUNE[depth (:label node)]:")
      (prn depth (:label node))
      (println "###")
      (Node. (:label node)
             (lazy-seq (when (pos? depth)
                         (map (partial prune (dec depth)) (:subtree node)))))))

(declare evaluate-with-alpha)
(declare evaluate-with-alpha-1)
(declare evaluate-with-alpha-2)
(declare evaluate-with-alpha-3)
(declare evaluate-minmax)
(defn evaluate [depth state]
  ;;(time (evaluate-minmax depth state))
  (time (evaluate-with-alpha depth state))
  ;;(evaluate-with-alpha-1 depth state)
  ;;(evaluate-with-alpha-2 depth state)
  ;;(evaluate-with-alpha-3 depth state)
  ;;(time (evaluate-with-alpha-3 depth state))
  )

(defn evaluate-minmax [depth state]
  (do (println "###")
      (print "EVALUATE-MINMAX[depth state]:")
      (prn depth state)
      (println "###")
      (->> state
           gametree
           (prune depth)
           (maptree static)
           maximise)))

;;;; alpha-beta pruning version ;;;;;

(defn min-leq? [pot nums]
  (do (println "###")
      (print "MIN-LEQ?[pot nums]:")
      (prn pot nums)
      (println "###")
      (cond (empty? nums) false
            (<= (first nums) pot) true
            :else (recur pot (rest nums)))))

(defn omit [pot nums]
  (do (println "###")
      (print "OMIT[pot nums]:")
      (prn pot nums)
      (println "###")
      (cond (empty? nums) nil
            (min-leq? pot nums) (omit pot (rest nums))
            :else (let [minimum (apply min nums)]
                    (cons minimum
                          (omit minimum (rest nums)))))))

(defn mapmin [nums]
  (do (println "###")
      (print "MAPMIN[nums]:")
      (prn nums)
      (println "###")
      (let [minimum (apply min nums)]
        (cons minimum (omit minimum (rest nums))))))

(declare minimise-)
(defn maximise- [node]
  (if (empty? (:subtree node))
    (:label node)
    (mapmin (map minimise- (:subtree node)))))

(defn- minimise- [node]
  (if (empty? (:subtree node))
    (:label node)
    (apply min (map maximise- (:subtree node)))))

(defn evaluate-with-alpha [depth state]
  (->> state
       gametree
       (prune depth)
       (maptree static)
       maximise-
       (apply max)))

;;;; alpha-beta improvements ;;;;;

(defn higher [node-1 node-2]
  (> (:label node-1)
     (:label node-2)))

(declare lowfirst)
(defn highfirst [node]
  (Node. (:label node)
         (sort higher
               (map lowfirst (:subtree node)))))

(defn lowfirst [node]
  (Node. (:label node)
         (sort (complement higher)
               (map highfirst (:subtree node)))))

(defn evaluate-with-alpha-1 [depth state]
  (->> state
       gametree
       (prune depth)
       (maptree static)
       highfirst
       maximise-
       (apply max)))

;;;;; more improvements ;;;;;

(defn nodett [n label sub]
  (Node. label (take n sub)))

(defn taketree [n node]
  (redtree (partial nodett n) cons nil node))

(defn evaluate-with-alpha-2 [depth state]
  (->> state
       gametree
       (prune depth)
       (maptree static)
       highfirst
       (taketree 3)
        maximise-
        (apply max)))

;;;; dynamic position consideration ;;;;;

(defn- prune-new [depth node]
  (do (println "###")
      (print "PRUNE-NEW[depth (:label node)]:")
      (prn depth (:label node))
      (println "###")
      (Node. (:label node)
             (lazy-seq (if (pos? depth)
                         (map (partial prune-new (dec depth)) (:subtree node))
                         (when (dynamic? (:label node))
                           (map (partial prune-new 0) (:subtree node))))))))

(defn- evaluate-with-alpha-3 [depth state]
  (->> state
       gametree
       (prune-new depth)
       (maptree static)
       highfirst
       (taketree 3)
       maximise-
       (apply max)))
