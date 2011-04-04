(ns tursas.state0x88.common)

(def white-king-store 0x0c)
(def black-king-store 0x7c)
(def turn-store 0x38)
(def half-move-store 0x39)
(def full-move-n-store 0x3a)
(def full-move-store 0x3b)
(def castling-store 0x58)
(def en-passant-store 0x59)
(def dynamic-store 0x5a)

(def check-store 0x5b)

(def prev-move-from 0x6a)
(def prev-move-to 0x6b)
(def prev-piece 0x6c)

(def white-king-side 8)
(def white-queen-side 4)
(def black-king-side 2)
(def black-queen-side 1)

(def king-side 1)
(def queen-side 0)

(def white 0)
(def black 1)

(def black-queen -6)
(def black-rook -5)
(def black-bishop -4)
(def black-king -3)
(def black-knight -2)
(def black-pawn -1)

(def empty-square 0)

(def white-pawn 1)
(def white-knight 2)
(def white-king 3)
(def white-bishop 4)
(def white-rook 5)
(def white-queen 6)

(def north 16)
(def nn 32)
(def south -16)
(def ss -32)
(def east 1)
(def west -1)
(def ne 17)
(def sw -17)
(def nw 15)
(def se -15)

(def opening-game 0)
(def middle-game 1)
(def end-game 2)
