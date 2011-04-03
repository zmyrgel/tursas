(ns tursas.test.state0x88.core
  (:use [tursas.state] :reload)
  (:use [tursas.state0x88.core] :reload)
  (:use [tursas.state0x88.move] :reload)
  (:use [tursas.state0x88.common])
  (:use [clojure.test]))

(def startpos-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def startpos-state (make-state [5 2 4 6 3 4 2 5 0 0 0 0 4 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 15 -1 0 0 0 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 -5 -2 -4 -6 -3 -4 -2 -5 0 0 0 0 116 0 0 0] {96 -1, 97 -1, 98 -1, 99 -1, 100 -1, 101 -1, 102 -1, 103 -1, 112 -5, 113 -2, 114 -4, 115 -6, 116 -3, 117 -4, 118 -2, 119 -5} {0 5, 1 2, 2 4, 3 6, 4 3, 5 4, 6 2, 7 5, 16 1, 17 1, 18 1, 19 1, 20 1, 21 1, 22 1, 23 1}))

;; (deftest state->fen-test
;;   (testing "State to/from FEN functions"
;;     (is (= (fen->state startpos-fen) startpos-state))
;;     (is (= (startpos-fen (state->fen startpos-state))))))

;;  ;; depth nodes  totalnodes
;;  ;; 1     20         20
;;  ;; 2     400        420
;;  ;; 3     890        29322
;;  ;; 4     197281     206603
;;  ;; 5     4865609    5072212
;;  ;; 6     119060324  124132536
;;  ;; 7     3195901860 3320034396
;; (deftest perft-startpos-test
;;   (testing "Perft values from start position (rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1)"
;;     (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
;;       (is (= (perft state 1) 20))
;;       (is (= (perft state 2) 400))
;;       (is (= (perft state 3) 890))
;;       ;;(is (= (perft state 4) 197281))
;;       ;;(is (= (perft state 5) 4865609))
;;       ;;(is (= (perft state 6) 119060324))
;;       ;;(is (= (perft state 7) 3195901860))
;;       )))

;; ;; depth nodes      totalnodes
;; ;; 1     48         48
;; ;; 2     2039       2087
;; ;; 3     97862      99949
;; ;; 4     4085603    4185552
;; ;; 5     193690690  197876242
;; ;; 6     8031647685 8229523927
;; (deftest perft-testpos-test
;;   (testing "Perft values from General test position (r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1)"
;;     (let [state (fen->state "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")]
;;       (is (= (perft state 1) 48))
;;       (is (= (perft state 2) 2039))
;;       (is (= (perft state 3) 97862))
;;       ;;(is (= (perft state 4) 4085603))
;;       ;;(is (= (perft state 5) 193690690))
;;       ;;(is (= (perft state 6) 8031647685))
;;       )))

;;  ;; depth nodes      totalnodes
;;  ;; 1     24         24
;;  ;; 2     496        520
;;  ;; 3     9483       10003
;;  ;; 4     182838     192841
;;  ;; 5     3605103    3797944
;;  ;; 6     71179139   74977083
;; (deftest perft-promotion-test
;;   (testing "Perft values from promotion position (n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1)"
;;     (let [state (fen->state "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")]
;;       (is (= (perft state 1) 24))
;;       (is (= (perft state 2) 496))
;;       (is (= (perft state 3) 9483))
;;       ;;(is (= (perft state 4) 182838))
;;       ;;(is (= (perft state 5) 3605103))
;;       ;;(is (= (perft state 6) 71179139))
;;       )))

(deftest castling-value-test
  (testing "Castling value calculation"
    (let [f @#'tursas.state0x88.core/calculate-white-castling
          g @#'tursas.state0x88.core/calculate-black-castling]
      (is (= (f 15 (coord->move "e2e3")) 15))
      (is (= (f 15 (coord->move "e1e2")) 3))
      (is (= (f 15 (coord->move "a1a2")) 11))
      (is (= (f 15 (coord->move "h1h2")) 7))
      (is (= (f 15 (coord->move "a2a8")) 14))
      (is (= (f 15 (coord->move "h2h8")) 13))

      (is (= (g 15 (coord->move "e6e5")) 15))
      (is (= (g 15 (coord->move "e8e7")) 12))
      (is (= (g 15 (coord->move "a8a7")) 14))
      (is (= (g 15 (coord->move "h8h7")) 13))
      (is (= (g 15 (coord->move "h2h1")) 7))
      (is (= (g 15 (coord->move "a7a1")) 11)))))

 (deftest en-passant-value-test
   (testing "En-passant value calculation"
     (let [f @#'tursas.state0x88.core/calculate-en-passant]
       (is (= (f WHITE WHITE-PAWN EMPTY EMPTY (coord->move "a2a4")) -1))
       (is (= (f WHITE WHITE-PAWN WHITE-PAWN EMPTY (coord->move "a2a4")) -1))
       (is (= (f WHITE WHITE-PAWN BLACK-PAWN EMPTY (coord->move "a2a3")) -1))
       (is (= (f WHITE WHITE-PAWN BLACK-PAWN EMPTY (coord->move "a2a4")) 0x20))
       (is (= (f WHITE WHITE-PAWN EMPTY BLACK-PAWN (coord->move "a2a4")) 0x20))

       (is (= (f BLACK BLACK-PAWN EMPTY EMPTY (coord->move "c7c5")) -1))
       (is (= (f BLACK BLACK-PAWN BLACK-PAWN EMPTY (coord->move "c7c5")) -1))
       (is (= (f BLACK BLACK-PAWN WHITE-PAWN EMPTY (coord->move "c7c6")) -1))
       (is (= (f BLACK BLACK-PAWN WHITE-PAWN EMPTY (coord->move "c7c5")) 0x52))
       (is (= (f BLACK BLACK-PAWN EMPTY WHITE-PAWN (coord->move "c7c5")) 0x52)))))

;; full move counting

;; promotion piece reading
(deftest promotion-test
  (testing "promotion piece reading"
    (let [f @#'tursas.state0x88.core/promotion-piece]
      (is (= (f WHITE (coord->move "e7e8q")) WHITE-QUEEN))
      (is (= (f WHITE (coord->move "e7e8n")) WHITE-KNIGHT))
      (is (= (f WHITE (coord->move "e7e8")) WHITE-QUEEN))
      (is (= (f BLACK (coord->move "e2e1q")) BLACK-QUEEN))
      (is (= (f BLACK (coord->move "e2e1n")) BLACK-KNIGHT))
      (is (= (f BLACK (coord->move "e2e1")) BLACK-QUEEN)))))

;; fide-draw tests
