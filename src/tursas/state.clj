(ns tursas.state)

(defprotocol State
  (occupied? [state index])
  (black? [state index])
  (white? [state index])
  (apply-move [state move])
  (in-check? [state])
  (game-end? [state])
  (state->fen [state])
  (legal-states [state])
  (get-pieces [state])
  (perft [state depth]))
