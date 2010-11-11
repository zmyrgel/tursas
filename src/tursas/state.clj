(ns tursas.state)

(defprotocol State
  (occupied? [state index])
  (black? [state index])
  (white? [state index])
  (apply-move [state move])
  (in-check? [state])
  (state->fen [state])
  (legal-states [state])
  (get-pieces [state]))
