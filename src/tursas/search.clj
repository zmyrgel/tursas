(ns tursas.search2
  (:use (tursas state eval state0x88)))

;; treeof X ::= node X (listof (treeof X))
(defrecord Node [label subtree])

(defn- make-applied-node [f label subtree]
  (do (println "###")
      (print "MAKE-APPLIED-NODE[f label subtree]:")
      (prn f label subtree)
      (println "###"))
  (Node. (f label) subtree))

;; position -> listof position
(defn- moves [state]
  (do (println "###")
      (print "MOVES[state]: ")
      (prn state)
      (println "###")
      (when (not (nil? state))
        (legal-states state))))

;;redtree' f g a nil = a
;;redtree' f g a (cons subtree rest) = g (redtree f g a subtree)
;;                                       (redtree' f g a rest)
(declare redtree)
(defn- redtree- [f g a subtree]
  (if (empty? (first subtree))
    a
    (g (redtree f g a (first subtree))
       (redtree- f g a (rest subtree)))))

;;redtree f g a (node label subtrees)
;; = f label (redtree' f g a subtrees)
(defn- redtree [f g a node]
  (f (:label node) (redtree- f g a (:subtree node))))

;; maptree f = redtree (node . f) cons nil
(defn- maptree [f node]
  (redtree (partial make-applied-node f) cons nil node))

;; reptree f a = node a (map (reptree f) (f a))
(defn- reptree [f a]
  (do (println "###")
      (print "REPTREE[f a]: ")
      (prn f a)
      (println "###")
      (Node. a
             (lazy-seq (map (partial reptree f) (f a))))))

;; gametree p = reptree moves p
(defn- gametree [state]
  (do (println "###")
      (print "GAMETREE[state]:")
      (prn state)
      (println "###")
      (reptree moves state)))

;; static: position -> number
(defn- static [state]
  (do (println "###")
      (print "STATIC[state]:")
      (prn state)
      (println "###")
      (evaluate-state state)))

;; maximise (node n nil) = n
;; maximise (node n sub) = max (map minimise sub)
(declare minimise)
(defn- maximise [node]
    (if (empty? (:subtree node))
      (:label node)
      (apply max (map minimise (:subtree node)))))

;; minimise (node n nil) = n
;; minimise (node n sub) = min (map maximise sub)
(defn- minimise [node]
      (if (empty? (:subtree node))
        (:label node)
        (apply min (map maximise (:subtree node)))))

;; prune 0 (node a x) = node a nil
;; prune n (node a x) = node a (map (prune (n-1)) x)
(defn- prune [depth node]
  (do (println "###")
      (print "PRUNE[depth (:label node)]:")
      (prn depth (:label node))
      (println "###")
      (Node. (:label node)
             (lazy-seq (when (pos? depth)
                         (map (partial prune (dec depth)) (:subtree node)))))))

;; evaluate = maximise . maptree static . prune 5 . gametree
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
;;"Elapsed time: 8348.365248 msecs" minmax

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

;; minleq nil pot = false
;; minleq (cons num rest) pot = true, if num<=pot
;; = minleq rest pot, otherwise
(defn min-leq? [pot nums]
  (do (println "###")
      (print "MIN-LEQ?[pot nums]:")
      (prn pot nums)
      (println "###")
      (cond (empty? nums) false
            (<= (first nums) pot) true
            :else (recur pot (rest nums)))))

;; omit pot nil = nil
;; omit pot (cons nums rest) =
;; = omit pot rest, if minleq nums pot
;; = cons (min nums) (omit (min nums) rest), otherwise
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

;; maximise' (node n nil) = cons n nil
;; maximise' (node n l) = map minimise l
;;                      = map (min . minimise') l
;;                      = map min (map minimise' l)
;;                      = mapmin (map minimise' l)
;;                      where mapmin = map min
;;
;;                      mapmin (cons nums rest) =
;;                       = cons (min nums) (omit (min nums) rest)

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

;; evaluate = max . maximise' . maptree static . prune 8 . gametree
(defn evaluate-with-alpha [depth state]
  (->> state
       gametree
       (prune depth)
       (maptree static)
       maximise-
       (apply max)))

;;;; alpha-beta improvements ;;;;;

;; higher (node n1 sub1) (node n2 sub2) = n1>n2
(defn higher [node-1 node-2]
  (> (:label node-1)
     (:label node-2)))

;; highfirst (node n sub) = node n (sort higher (map lowfirst sub))
(declare lowfirst)
(defn highfirst [node]
  (Node. (:label node)
         (sort higher
               (map lowfirst (:subtree node)))))

;; lowfirst (node n sub) = node n (sort (not.higher) (map highfirst sub))
(defn lowfirst [node]
  (Node. (:label node)
         (sort (complement higher)
               (map highfirst (:subtree node)))))

;; evaluate = max . maximise' . highfirst . maptree static .
;; prune 8 . gametree
(defn evaluate-with-alpha-1 [depth state]
  (->> state
       gametree
       (prune depth)
       (maptree static)
       highfirst
       maximise-
       (apply max)))

;;;;; more improvements ;;;;;

;;nodett n label sub = node label (take n sub)
(defn nodett [n label sub]
  (Node. label (take n sub)))

;;taketree n = redtree (nodett n) cons nil
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

;; prune 0 (node pos sub) = node pos (map (prune 0) sub), if dynamic pos
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
