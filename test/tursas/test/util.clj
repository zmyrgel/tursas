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

(deftest splitting-test
  (testing "Move splitting"
    (is (= (split-move "e2e4") (list "e2" "e4")))
    (is (= (split-move "e7e8q") (list "e7" "e8" "q"))))
  (testing "Sequence splitting"
    (is (= (split-on 3 '(1 2 3 4 5 6)) '((1 2) (4 5 6))))
    (is (= (split-on 3 '(1 2 4 5 6)) '(1 2 4 5 6)))
    (is (= (split-on 3 '()) '()))
    (is (= (split-on "3" '(1 2 3 4)) '(1 2 3 4)))
    (is (= (split-on "3" "abc3de") '("abc" "de")))
    ))

(deftest board-printing-test
  (testing "Board printing"
    (is (let [fen1 "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
              result1 "8| r n b q k b n r\n7| p p p p p p p p\n6| - - - - - - - -\n5| - - - - - - - -\n4| - - - - - - - -\n3| - - - - - - - -\n2| P P P P P P P P\n1| R N B Q K B N R\n------------------\n | a b c d e f g h\n"
              fen2 "r3k3/ppPqpppr/n1ppbn1p/8/2B5/BP1Q1P1N/2P1P1PP/RN2K2R w KQq - 4 7"
              result2 "8| r - - - k - - -\n7| p p P q p p p r\n6| n - p p b n - p\n5| - - - - - - - -\n4| - - B - - - - -\n3| B P - Q - P - N\n2| - - P - P - P P\n1| R N - - K - - R\n------------------\n | a b c d e f g h\n"]
          (= (fen->ascii fen1) result1)
          (= (fen->ascii fen2) result2)))))

(deftest char-digit-test
  (testing "Character digit predicate"
    (is (true? (char-digit? \9)))
    (is (true? (char-digit? \0)))
    (is (true? (char-digit? \1)))
    (is (false? (char-digit? \a)))))

(deftest expand-compress-item-test
  (testing "Sequence compacting and expansion"
    (is (= (expand-digits \E "eee22d3a#") '(\e \e \e \E \E \E \E \d \E \E \E \a \#)))
    (is (= (compact-item \E "eeeEEEEEee") '(\e \e \e \5 \e \e)))))
