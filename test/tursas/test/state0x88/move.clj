(ns tursas.test.state0x88.move
  (:use [tursas.state] :reload)
  (:use [tursas.state0x88.move] :reload)
    (:use [tursas.move] :reload)
  (:use [clojure.test]))

(deftest coordinate-transformation
  (testing "Coordinate transformation"
    (let [f @#'tursas.state0x88.move/index->coord
          g @#'tursas.state0x88.move/coord->index]
      (is (= (f 0x10) "a2"))
      (is (= (f 0x77) "h8"))
      (is (= (g "a2") 0x10))
      (is (= (g "h8") 0x77)))))

(deftest move-creation
  (testing "Move creation"
    (let [f @#'tursas.state0x88.move/coord->move]
      (is (nil? (f "e9e0"))))))
