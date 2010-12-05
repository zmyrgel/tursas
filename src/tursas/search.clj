(ns tursas.search
  (:use (tursas state state0x88)))

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
      (Node. a (lazy-seq (map (partial reptree f) (f a))))))

(defn- gametree [state]
  (do (println "###")
      (print "GAMETREE[state]:")
      (prn state)
      (println "###")
      (reptree moves state)))

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
             (if (pos? depth)
               (map (partial prune (dec depth)) (:subtree node))
               (when (dynamic? (:label node))
                 (map (partial prune 0) (:subtree node)))))))

(defn evaluate-with-minmax [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       maximise))

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

(defn evaluate-with-alpha [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
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

(defn evaluate-with-alpha-1 [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       highfirst
       maximise-
       (apply max)))

;;;;; more improvements ;;;;;

(defn nodett [n label sub]
  (Node. label (take n sub)))

(defn taketree [n node]
  (redtree (partial nodett n) cons nil node))

(defn evaluate-with-alpha-2 [depth eval-fn state]
  (->> state
       gametree
       (prune depth)
       (maptree eval-fn)
       highfirst
       (taketree 3)
        maximise-
        (apply max)))

;;;; dynamic position consideration ;;;;;

(defn- evaluate-with-alpha-3 [depth eval-fn state]
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
  ;;(time (evaluate-with-alpha-3 depth eval-fn state))
  )
