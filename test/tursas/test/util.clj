(ns tursas.test.util
  (:use [tursas.util] :reload)
  (:use [clojure.test]))

(deftest coordinate-test
  (testing "Coordinate validation"
    (is (true? (valid-coord? "e3")))
    (is (true? (valid-coord? "a8")))
    (is (false? (valid-coord? "a9")))
    (is (false? (valid-coord? "i2")))
    (is (false? (valid-coord? "v3")))
    (is (false? (valid-coord? "a22")))
    (is (false? (valid-coord? "aa2")))
    (is (true? (coordinate-string? "a1a2")))
    (is (false? (coordinate-string? "a0a2")))
    (is (true? (coordinate-string? "a7a8q")))
    (is (false? (coordinate-string? "a7a8a")))
    (is (false? (coordinate-string? "0000")))
    (is (false? (move-string? "a5i6")))
    (is (false? (move-string? "a6a7i")))
    (is (true? (move-string? "g7g8q")))))

(deftest move-splitting-test
  (testing "Move splitting"
    (is (= (split-move "e2e4") (list "e2" "e4")))
    (is (= (split-move "e7e8q") (list "e7" "e8" "q")))))

(deftest board-printing-test
  (testing "Board printing"
    (is (let [fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
              res "8| r n b q k b n r\n7| p p p p p p p p\n6| - - - - - - - -\n5| - - - - - - - -\n4| - - - - - - - -\n3| - - - - - - - -\n2| P P P P P P P P\n1| R N B Q K B N R\n------------------\n | a b c d e f g h\n"]
          (= (fen->ascii fen) res)))))
