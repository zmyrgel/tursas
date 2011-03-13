(ns tursas.test.state0x88.core
  (:use [tursas.state] :reload)
  (:use [tursas.state0x88.core] :reload)
  (:use [clojure.test]))

(def startpos-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def startpos-state (make-state [5 2 4 6 3 4 2 5 0 0 0 0 4 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 15 -1 0 0 0 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 -5 -2 -4 -6 -3 -4 -2 -5 0 0 0 0 116 0 0 0] {96 -1, 97 -1, 98 -1, 99 -1, 100 -1, 101 -1, 102 -1, 103 -1, 112 -5, 113 -2, 114 -4, 115 -6, 116 -3, 117 -4, 118 -2, 119 -5} {0 5, 1 2, 2 4, 3 6, 4 3, 5 4, 6 2, 7 5, 16 1, 17 1, 18 1, 19 1, 20 1, 21 1, 22 1, 23 1}))

(deftest state->fen-test
  (testing "State to/from FEN functions"
    (testing "FEN to State"
      (is (= (fen->state startpos-fen) startpos-state) "State generation from FEN"))
    (testing "State to FEN"
      (is (= (startpos-fen (state->fen startpos-state) "Converting state to FEN"))))))

;; Perft test startpos
;; rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
;; depth nodes  totalnodes
;; 1     20         20
;; 2     400        420
;; 3     890        29322
;; 4     197281     206603
;; 5     4865609    5072212
;; 6     119060324  124132536
;; 7     3195901860 3320034396

(deftest perft-startpos-test
  (let [state (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
    (is (= (perft state 1) 20) "Perft(1) on startpos test")
    (is (= (perft state 2) 400) "Perft(2) on startpos test")
    (is (= (perft state 3) 890) "Perft(3) on startpos test")
    (is (= (perft state 4) 197281) "Perft(4) on startpos test")
    ;;(is (= (perft state 5) 4865609) "Perft(5) on startpos test")
    ;;(is (= (perft state 6) 119060324) "Perft(6) on startpos test")
    ;;(is (= (perft state 7) 3195901860) "Perft(7) on startpos test")
    ))

;; General test position
;; r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1
;; depth nodes      totalnodes
;; 1     48         48
;; 2     2039       2087
;; 3     97862      99949
;; 4     4085603    4185552
;; 5     193690690  197876242
;; 6     8031647685 8229523927

(deftest perft-testpos-test
  (let [state (fen->state "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")]
    (is (= (perft state 1) 48) "Perft(1) on testpos test")
    (is (= (perft state 2) 2039) "Perft(2) on testpos test")
    (is (= (perft state 3) 97862) "Perft(3) on testpos test")
    (is (= (perft state 4) 4085603) "Perft(4) on testpos test")
    ;;(is (= (perft state 5) 193690690) "Perft(5) on testpos test")
    ;;(is (= (perft state 6) 8031647685) "Perft(6) on testpos test")
    ))

;; Promotion test
;; n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1

;; depth nodes      totalnodes
;; 1     24         24
;; 2     496        520
;; 3     9483       10003
;; 4     182838     192841
;; 5     3605103    3797944
;; 6     71179139   74977083

(deftest perft-promotion-test
  (let [state (fen->state "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")]
    (is (= (perft state 1) 24) "Perft(1) on promotion pos test")
    (is (= (perft state 2) 496) "Perft(2) on promotion pos test")
    (is (= (perft state 3) 9483) "Perft(3) on promotion pos test")
    (is (= (perft state 4) 182838) "Perft(4) on promotion pos test")
    ;;(is (= (perft state 5) 3605103) "Perft(5) on promotion pos test")
    ;;(is (= (perft state 6) 71179139) "Perft(6) on promotion pos test")
    ))
