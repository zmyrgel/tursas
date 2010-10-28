(ns tursas.state)

(defprotocol State
  (occupied? [state index])
  (black? [state index])
  (white? [state index])
  (opponent [state])
  (make-move [state move])
  (in-check? [state])
  (fen->state [fen])
  (state->fen [state])
  (legal-states [state])
  (get-pieces [state]))
