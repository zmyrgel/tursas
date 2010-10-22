(ns tursas.search
  (:use (tursas state)))

;; function minimax(node, depth)
;;     if node is a terminal node or depth = 0
;;         return the heuristic value of node
;;     else
;;         let α := -∞
;;         foreach child of node      { evaluation is identical for both players }
;;             let α := max(α, -minimax(child, depth-1))
;;         return α

(defn minimax-search
  "Search STATEs with Minimax algorithm until DEPTH and use EVAL to
  evaluate results."
  [state depth eval-fn]
  (if (= depth 0)
    (eval-fn state)
    (loop [states (legal-states state)
           best-state nil
           best-value nil]
      (if (empty? states)
        best-value
        (let [value (- (minimax-search (first states) (- depth 1) eval-fn))]
          (if (or (nil? best-value)
                  (> value best-value))
            (recur (rest states) (first states) value)
            (recur (rest states) best-state best-value)))))))
