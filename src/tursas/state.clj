(ns tursas.state)

(defprotocol State
  (allowed? [state move])
  (occupied? [state index])
  (black? [state index])
  (white? [state index])
  (check? [state])
  (mate? [state])
  (draw? [state])
  (state->fen [state])
  (result [state])
  (legal-states [state])
  (legal-moves [state])
  (apply-move [state move])
  (turn [state])
  (perft [state depth])
  (dynamic? [state])
  (full-moves [state])
  (evaluate [state])
  (last-move [state])
  (game-score [state])
  (game-end? [state]))
