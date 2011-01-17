(ns tursas.move)

(defprotocol Move
  (move->coord [move])
  (from [move])
  (to [move])
  (promotion [move]))



