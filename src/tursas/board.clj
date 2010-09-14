;; move these
(def *side-white* 0)
(def *side-black* 1)

;; codes for different pieces
(def *pawn* 0)
(def *knight* 2)
(def *bishop* 4)
(def *rook* 6)
(def *queen* 8)
(def *king* 10)

(def *white-pawn* (+ *pawn* *side-white*))
(def *white-knight* (+ *knight* *side-white*))
(def *white-bishop* (+ *bishop* *side-white*))
(def *white-rook* (+ *rook* *side-white*))
(def *white-queen* (+ *queen* *side-white*))
(def *white-king* (+ *king* *side-white*))

(def *black-pawn* (+ *pawn* *side-black*))
(def *black-knight* (+ *knight* *side-black*))
(def *black-bishop* (+ *bishop* *side-black*))
(def *black-rook* (+ *rook* *side-black*))
(def *black-queen* (+ *queen* *side-black*))
(def *black-king* (+ *king* *side-black*))

(def *empty-square* 12)

(def *all-pieces* 12)
(def *all-squares* 64)

(def *all-white-pieces* (+ *all-pieces* *side-white*))
(def *all-black-pieces* (+ *all-pieces* *side-black*))
(def *all-bitboards* 14)

(def *castle-kingside* 0)
(def *castle-queenside* 2)

;; material values for pieces
;; pawn, knight, bishop, rook, queen, king
(def *piece-values* [100 100 300 300 350 350 500 500 900 900 2000 2000])

;; extra variables for figuring castling rules
(def *extrakings-white-kingside* (bit-or (get *square-bits* 60)
                                         (get *square-bits* 61)))
(def *extrakings-white-queenside* (bit-or (get *square-bits* 60)
                                          (get *square-bits* 59)))
(def *extrakings-black-kingside* (bit-or (get *square-bits* 4)
                                         (get *square-bits* 5)))
(def *extrakings-black-queenside* (bit-or (get *square-bits* 4)
                                          (get *square-bits* 3)))
(def *empty-squares-white-kingside* (bit-or (get *square-bits* 61)
                                            (get *square-bits* 62)))
(def *empty-squares-white-queenside* (bit-or (get *square-bits* 59)
                                             (get *square-bits* 58)
                                             (get *square-bits* 57)))
(def *empty-squares-black-kingside* (bit-or (get *square-bits* 5)
                                            (get *square-bits* 6)))
(def *empty-squares-black-queenside* (bit-or (get *square-bits* 3)
                                             (get *square-bits* 2)
                                             (get *square-bits* 1)))

;; hashing database
(def *hash-key-components* (l))
(let [pieces (vec (replicate *all-pieces* nil))
      squares (vec (replicate *all-squares* nil))]
  (loop [i 0 j 0 v (vector-of :int)]
    (cond (= j *all-squares*) (recur (inc i) 0 v)
          (= i *all-pieces*) v
          :else (recur i (inc j) (conj v 1)))))
(rand-int (Integer/MAX_VALUE))

(def *square-bits*
     (loop [i 0 v (vector-of :long)]
       (if (< i *all-pieces*)
         (recur (inc i) (conj v (bit-shift-left (long 1) i)))
         v)))


;; reference to current player
(def *current-player* (ref 0))
(def *en-passant-pawn* (ref 0))
(def *castling-status* (ref [false false false false]))
(def *has-castled* (ref [false false]))
(def *extra-kings* (ref (vec (replicate 2 (long 0)))))
(def *num-pawns* (ref (vec (replicate 2 (int 0)))))
(def *bitboards* (ref (vec (replicate *all-pieces* (long 0)))))
(def *material-value* (ref (vec (replicate 2 (int 0)))))

;; accessors
(defn castle?
  "Can the given side castle?"
  [which]
  (get *castling-status which))

(defn castled?
  "Has the given side castled?"
  (get *has-castled* which))

(defn get-en-passant-pawn []
  @en-passant-pawn)

(defn get-extra-kings
  "get 'phantom kings for player"
  [side]
  (get *extrakings* side))

(defn set-extra-kings
  "Mark squares to contain 'phantom' kings to help detect illegal castling."
  [side value]
  (dosync (alter *extrakings* ))
  )

(defn clear-extra-kings [side]
  )

(defn get-current-player []
  @*current-player*)

(defn get-bitboard [which]
  (get *bitboards* which))

(defn find-black-piece
  "Look for black pieces in the square"
  [square]
  (cond ((not (zero? (bit-and (get *bitboards* *black-king*)
                              (get *square-bits* square)))) *black-king*)
        ((not (zero? (bit-and (get *bitboards* *black-queen*)
                              (get *square-bits* square)))) *black-queen*)
        ((not (zero? (bit-and (get *bitboards* *black-rook*)
                              (get *square-bits* square)))) *black-rook*)
        ((not (zero? (bit-and (get *bitboards* *black-knigth*)
                              (get *square-bits* square)))) *black-knigth*)
        ((not (zero? (bit-and (get *bitboards* *black-bishop*)
                              (get *square-bits* square)))) *black-bishop*)
        ((not (zero? (bit-and (get *bitboards* *black-pawn*)
                              (get *square-bits* square)))) *black-pawn*)
        :else *empty-square*))

(defn find-white-piece
  "Look for white pieces in the square"
  [square]
  (cond ((not (zero? (bit-and (get *bitboards* *white-king*)
                              (get *square-bits* square)))) *white-king*)
        ((not (zero? (bit-and (get *bitboards* *white-queen*)
                              (get *square-bits* square)))) *white-queen*)
        ((not (zero? (bit-and (get *bitboards* *white-rook*)
                              (get *square-bits* square)))) *white-rook*)
        ((not (zero? (bit-and (get *bitboards* *white-knigth*)
                              (get *square-bits* square)))) *white-knigth*)
        ((not (zero? (bit-and (get *bitboards* *white-bishop*)
                              (get *square-bits* square)))) *white-bishop*)
        ((not (zero? (bit-and (get *bitboards* *white-pawn*)
                              (get *square-bits* square)))) *white-pawn*)
        :else *empty-square*))


(defn initialize-board
  "Setups the game board. By default use the normal starting position. If startpos given, use it instead"
  [] ;; add startpos argument, fen format
  (do
    (empty-board)
    (add-piece 0 *black-rook*)
    (add-piece 1 *black-knigth*)
    (add-piece 2 *black-bishop*)
    (add-piece 3 *black-queen*)
    (add-piece 4 *black-king*)
    (add-piece 5 *black-bishop*)
    (add-piece 6 *black-knigth*)
    (add-piece 7 *black-rook*)
    (loop [i 8]
      (if (= i 16)
        nil
        (add-piece i *black-pawn*)
        (recur (inc i))))
    (loop [i 48]
      (if (= i 56)
        nil
        (add-piece i *white-pawn*)
        (recur (inc i))))
    (add-piece 56 *white-rook*)
    (add-piece 57 *white-knigth*)
    (add-piece 58 *white-bishop*)
    (add-piece 59 *white-queen*)
    (add-piece 60 *white-king*)
    (add-piece 61 *white-bishop*)
    (add-piece 62 *white-knigth*)
    (add-piece 63 *white-rook*)

    (ref-set *castling-status* [true true true true])
    (ref-set *has-castled* [false false])
    (clear-en-passant-pawn)
    (dosync (ref-set *current-player* *side-white*))))

;;;; PRIVATE FUNCTIONS ;;;;
(defn- add-piece
  "Adds given piece to square."
  [square piece]
  (dosync
   (ref-set *bitboards* (assoc @*bitboards* piece
                               (bit-or (get @*bitboards* piece)
                                       (get @*square-bits* square))))

   (let [index (+ *all-pieces* (mod piece 2))]
     (ref-set *bitboards* (assoc @*bitboards* index
                                 (bit-or (get @*bitboards* index)
                                         (get @*square-bits* square)))))
   (let [index (mod piece 2)]
     (ref-set *material-value* (assoc @*material-value* index
                                      (+ (get @*material-value* index)
                                         (get *piece-values* piece)))))

   (cond ((= piece *white-pawn*)
          (ref-set *num-pawns* (assoc @*num-pawns* *side-white*
                                      (inc (get @*num-pawns* *side-white*)))))
         ((= piece *black-pawn*)
          (ref-set *num-pawns* (assoc @*num-pawns* *side-black*
                                      (inc (get @*num-pawns* *side-black*))))))))

(defn- remove-piece
  "Remove piece from square. Will fail if there's no piece."
  [square piece]
    (dosync
     (ref-set *bitboards* (assoc @*bitboards* piece
                                 (bit-xor (get @*bitboards* piece)
                                          (get @*square-bits* square))))

   (let [index (+ *all-pieces* (mod piece 2))]
     (ref-set *bitboards* (assoc @*bitboards* index
                                 (bit-xor (get @*bitboards* index)
                                         (get @*square-bits* square)))))
   (let [index (mod piece 2)]
     (ref-set *material-value* (assoc @*material-value* index
                                      (- (get @*material-value* index)
                                         (get *piece-values* piece)))))
   (cond ((= piece *white-pawn*)
          (ref-set *num-pawns* (assoc @*num-pawns* *side-white*
                                      (dec (get @*num-pawns* *side-white*)))))
         ((= piece *black-pawn*)
          (ref-set *num-pawns* (assoc @*num-pawns* *side-black*
                                      (dec (get @*num-pawns* *side-black*))))))))
(defn- empty-board
  "Clears all pieces from the board."
  []
  (dosync
       (ref-set *bitboards* (vec (replicate *all-bitboards* 0)))
       (ref-set *extrakings* (vec (replicate 2 0)))
       (ref-set *en-passant-pawn* 0)
       (ref-set *material-value* (vec (replicate 2 0)))
       (ref-set *num-pawns* (vec (replicate 2 0)))))

(defn- set-castling-status
  "Sets sides castling boolean value. Flag should be numerical value of (+ *side-white* *castle-kingside*."
  [flag value]
  (dosync (ref-set *castling-status*
                   (assoc @*castling-status* flag value))))

(defn- set-en-passant-pawn
  "Mark en-passant possible"
  [^Integer square]
  (clear-en-passant-pawn)
  (dosync (ref-set *en-passant-pawn* (bit-or @*en-passant-pawn*
                                             (get @*square-bits* square)))))

(defn- set-en-passant-pawn
  ""
  [^Long bitboard]
  (clear-en-passant-pawn)
  (dosync (ref-set *en-passant-pawn* bitboard)))

(defn- clear-en-passant-pawn
  "Clears the en-passant pawn value"
  []
  (dosync (ref-set *en-passant-pawn* (long 0))))

(defn- set-current-player
  "Sets the current player."
  [player]
  (dosync (ref-set *current-player* player)))
