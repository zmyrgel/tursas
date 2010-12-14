(ns tursas.test.state.state0x88
  (:use [tursas.state.state0x88] :reload)
  (:use [clojure.test]))

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
