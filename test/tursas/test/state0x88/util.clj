(ns tursas.test.state0x88.util
  (:use [tursas.state] :reload)
  (:use [tursas.state0x88.common] :reload)
  (:use [tursas.state0x88.util] :reload)
  (:use [tursas.state0x88.move] :reload)
  (:use [clojure.test]))

(deftest square-color-test
  (testing "Square colors"
    (is (= (square-color 0x20) black))
    (is (= (square-color 0x21) white))
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
    (is (= (piece-value \E) empty-square))
    (is (= (piece-value \k) black-king))
    (is (= (piece-value \d) empty-square))
    (is (= (piece-name white-king) \K))
    (is (= (piece-name black-bishop) \b))
    (is (= (piece-name 90) \E))))

(deftest opponent-test
  (testing "Opponent utility"
    (is (= (opponent white) black))
    (is (= (opponent black) white))))

(deftest promotion-move-test
  (testing "Promotion move detection"
    (is (true? (promotion? white-pawn (coord->move "e7e8q"))))
    (is (true? (promotion? white-pawn (coord->move "e7e8"))))
    (is (false? (promotion? white-pawn (coord->move "e6e7"))))
    (is (false? (promotion? white-rook (coord->move "e7e8"))))
    (is (true? (promotion? black-pawn (coord->move "e2e1q"))))
    (is (true? (promotion? black-pawn (coord->move "e2e1"))))
    (is (false? (promotion? black-pawn (coord->move "e3e2"))))
    (is (false? (promotion? black-rook (coord->move "e2e1"))))))
