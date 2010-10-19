(ns tursas.state)

(defprotocol State
  (occupied? [state index])
  (black? [state index])
  (white? [state index])
  (opponent [state])
  (commit-move [state move])
  (in-check? [state])
  (fen->state [fen])
  (state->fen [state])
  (pgn->state [pgn])
  (state->pgn [state])
  (available-states-from [state])
  (get-pieces [state])) ;; {:index piece-char}

;;(defrecord StateWith0x88Board)
;;(defrecord StateWithBitBoard)
;;(defrecord StateWithVectorBoard)
