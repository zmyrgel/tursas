(ns tursas.move)

;;; Move data structure holds information for given move
;;; and is presented in long algebraic notation
;;; The long algebraic notation is used in UCI and XBoard protocols
;;; to describe made chess moves.
;;; Examples:  e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion)

(defprotocol Move
  (move->algebraic [move])
  (from [move])
  (to [move])
  (promotion [move]))



