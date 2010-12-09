(ns tursas.move)

(defprotocol Move
  (move->algebraic [move])
  (from [move])
  (to [move])
  (promotion [move]))



