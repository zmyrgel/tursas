(ns tursas.test.state0x88.movegen
  (:use [tursas.state0x88.movegen] :reload)
  (:use [tursas.state0x88.move] :reload)
  (:use [tursas.state0x88.fen] :reload)
  (:use [tursas.state0x88.core] :reload)
  (:use [tursas.state0x88.common])
  (:use [clojure.test]))

;; move opponent piece
;; make move which results in chess
;; make illegal moves for each pieces
;; test castling moves
;; test different promotions

(deftest move-type-test
  (testing "Castling detection"
    (is (true? (castling? white-king (coord->move "e1g1"))))
    (is (true? (castling? white-king (coord->move "e1c1"))))
    (is (false? (castling? white-king (coord->move "e1e2"))))
    (is (true? (castling? black-king (coord->move "e8g8"))))
    (is (true? (castling? black-king (coord->move "e8c8"))))
    (is (false? (castling? black-king (coord->move "e8e8")))))
  (testing "En-passant detection"))

