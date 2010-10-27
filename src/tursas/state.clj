(ns tursas.state)

(defprotocol State
  (occupied? [state index])
  (black? [state index])
  (white? [state index])
  (opponent [state]) ; either :black or :white
  (commit-move [state move]) ; new state
  (in-check? [state])
  (fen->state [fen])
  (state->fen [state])
  (legal-states [state])
  (get-pieces [state])) ;; {:index piece-char}
