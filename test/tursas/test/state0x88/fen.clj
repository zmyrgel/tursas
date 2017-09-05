(ns tursas.test.state0x88.fen
  (:use [tursas.state] :reload)
  (:use [tursas.util] :reload)
  (:use [tursas.state0x88.fen] :reload)
  (:use [clojure.test]))

(deftest castling-test
  (testing "Castling value handling"
    (let [f @#'tursas.state0x88.fen/castling->str
          g @#'tursas.state0x88.fen/castling->value]
      (is (= (f 3) "kq"))
      (is (= (f 15) "KQkq"))
      (is (= (f 0) "-"))
      (is (= (g "kq") 3))
      (is (= (g "KQkq") 15))
      (is (= (g "-") 0)))))

(deftest utility-test
  (testing "Utilities"
    (let [f @#'tursas.util/expand-digits]
      (is (= (f \E "p3pfb2") '(\p \E \E \E \p \f \b \E \E))))))
