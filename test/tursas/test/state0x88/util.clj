(ns tursas.test.state0x88.util
  (:use [tursas.state] :reload)
  (:use [tursas.state0x88.common] :reload)
  (:use [tursas.state0x88.util] :reload)
  (:use [tursas.state0x88.move] :reload)
  (:use [clojure.test]))

(deftest square-color-test
  (testing "Square colors"
    (is (= (square-color 0x20) BLACK))
    (is (= (square-color 0x21) WHITE))
    (is (true? (same-color? 0x20 0x22)))
    (is (false? (same-color? 0x20 0x21)))))

(deftest board-index-tests
  (testing "Board index validation and pointing"
    (is (true? (board-index? 0x0)))
    (is (true? (board-index? 0x12)))
    (is (true? (board-index? 0x77)))
    (is (false? (board-index? 0x78)))
    (is (false? (board-index? -1)))
    (is (= (row 0x10) 1))
    (is (= (row 0x70) 7))
    (is (= (column 0x70) 0))
    (is (= (column 0x77) 7))
    (is (true? (same-column? 0x10 0x70)))
    (is (false? (same-column? 0x10 0x71)))))

(deftest piece-tests
  (testing "Piece tests"
    (is (= (piece-value \E) EMPTY))
    (is (= (piece-value \k) BLACK-KING))
    (is (= (piece-value \d) EMPTY))
    (is (= (piece-name WHITE-KING) \K))
    (is (= (piece-name BLACK-BISHOP) \b))
    (is (= (piece-name 90) \E))))

(deftest opponent-test
  (testing "Opponent utility"
    (is (= (opponent WHITE) BLACK))
    (is (= (opponent BLACK) WHITE))))

(deftest promotion-move-test
  (testing "Promotion move detection"
    (is (true? (promotion? WHITE-PAWN (coord->move "e7e8q"))))
    (is (true? (promotion? WHITE-PAWN (coord->move "e7e8"))))
    (is (false? (promotion? WHITE-PAWN (coord->move "e6e7"))))
    (is (false? (promotion? WHITE-ROOK (coord->move "e7e8"))))
    (is (true? (promotion? BLACK-PAWN (coord->move "e2e1q"))))
    (is (true? (promotion? BLACK-PAWN (coord->move "e2e1"))))
    (is (false? (promotion? BLACK-PAWN (coord->move "e3e2"))))
    (is (false? (promotion? BLACK-ROOK (coord->move "e2e1"))))))
