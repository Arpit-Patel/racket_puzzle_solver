;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz02 (read-puzzle "puzzle02.txt"))
(define puzz03 (read-puzzle "puzzle03.txt"))
(define puzz04 (read-puzzle "puzzle04.txt"))
(define puzz05 (read-puzzle "puzzle05.txt"))
(define puzz06 (read-puzzle "puzzle06.txt"))
(define puzz07 (read-puzzle "puzzle07.txt"))
(define puzz08 (read-puzzle "puzzle08.txt"))
(define puzz09 (read-puzzle "puzzle09.txt"))
(define puzz10 (read-puzzle "puzzle10.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define grid-abcd '((#\A #\B #\C) (#\X #\Y #\Z) (#\E #\F #\G)))
(define 1-find 1)
(define end 0)
(define char-find #\#)
(define char-find2 #\.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:

;; (transpose g) consumes a g and produces
;;    a g with swapped rows and collumns.
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose grid-abcd)
              (list (list #\A #\X #\E) (list #\B #\Y #\F) (list #\C #\Z #\G)))

(define (transpose g)
  (apply map list g))

;; Tests:
(check-expect (transpose grid-abcd)
              (list (list #\A #\X #\E) (list #\B #\Y #\F) (list #\C #\Z #\G)))
(check-expect (transpose (list (list #\A #\X #\E #\Q #\D)
                               (list #\B #\Y #\F #\Z #\G)
                               (list #\C #\Z #\G #\Q#\Q)))
              (list
               (list #\A #\B #\C)
               (list #\X #\Y #\Z)
               (list #\E #\F #\G)
               (list #\Q #\Z #\Q)
               (list #\D #\G #\Q)))
(check-expect (transpose (list (list #\A #\B #\C)
                               (list #\X #\Y #\Z)
                               (list #\E #\F #\G)
                               (list #\Q #\Z #\Q)
                               (list #\D #\G #\Q)))
              (list (list #\A #\X #\E #\Q #\D)
                    (list #\B #\Y #\F #\Z #\G)
                    (list #\C #\Z #\G #\Q#\Q)))
(check-expect (transpose (list (list #\A #\C)
                               (list #\Y #\Z)
                               (list #\E #\G)
                               (list #\Q #\Q)
                               (list #\D #\Q)))
              (list
               (list #\A #\Y #\E #\Q #\D)
               (list #\C #\Z #\G #\Q #\Q)))


;; (find-wpos loc row) consumes loc and row and produces
;;   list of wpos with relation to loc.
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "##") 10)
              (list (make-wpos 10 0 true 2)))

(define (find-wpos loc row)
  (filter (lambda (wpos) (not (= 1-find (wpos-len wpos))))
          (local [;; (length-count loc acc) consumes loc and acc and produces
                  ;;   length-count lst.
                  ;; length-count: (listof Char) Nat -> (listof Nat)
                  (define (length-count loc acc)
                    (cond
                      [(empty? loc) (list acc)]
                      [(and (char=? (first loc) char-find2) (not (zero? acc)))
                       (append (list acc) (length-count (rest loc) end))]
                      [(char=? (first loc) char-find)
                       (length-count (rest loc) (add1 acc))]
                      [else (length-count (rest loc) acc)]))
                  (define length-lst (length-count loc end))
                  ;; (pos-count loc acc) consumes loc, acc, and cross?
                  ;;   and produces pos-count lst.
                  ;; pos-count: (listof Char) Nat Bool -> (listof Nat)
                  (define (pos-count loc acc cross?)
                    (cond
                      [(empty? loc) empty] 
                      [(and (char=? (first loc) char-find) cross?)
                       (append (list acc)
                               (pos-count (rest loc) (add1 acc) false))]
                      [(char=? (first loc) char-find2)
                       (pos-count (rest loc) (add1 acc) true)]
                      [else (pos-count (rest loc) (add1 acc) false)]))
                  (define pos-lst (pos-count loc end true))
                  ;; (wpos-lst len-lst pos-lst) consumes len-lst and pos-lst
                  ;;   and produces list of wpos with relation to intial loc.
                  ;; wpos-lst: (listof Num) (listof Num) -> (listof WPos)
                  (define (wpos-lst len-l pos-l)
                    (cond
                      [(or (empty? len-l) (empty? pos-l)) empty]
                      [else (append (list (make-wpos row
                                                     (first pos-l)
                                                     true
                                                     (first len-l)))
                                    (wpos-lst (rest len-l) (rest pos-l)))]))]
            (wpos-lst length-lst pos-lst))))

;; Tests:
(check-expect (find-wpos empty 5) empty)
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)
(check-expect (find-wpos (string->list "..####.#.") 5)
              (list (make-wpos 5 2 true 4)))
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#.#.#") 5)
              empty)

(check-expect (find-wpos (string->list ".......") 5) empty)
(check-expect (find-wpos (string->list "#.#.#.#.#") 5) empty)


;; (initial-state puzzle) consumes puzzle and produces
;;   the initial State to start searching from.
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(check-expect (initial-state '(("##")
                               ("PL" "HA" "HA" "HE")))
              (make-state
               (list (list #\# #\#))
               (list (make-wpos 0 0 true 2))
               (list "PL" "HA" "HA" "HE")))

(define (initial-state puzzle)
  (local [;;(all-wpos a-grid) consumes a-grid and produces all the wpos
          ;;   that exist in a-grid
          ;; allwpos: Grid -> (listof WPos)      
          (define (allwpos a-grid)
            (append (row-wpos a-grid end empty)
                    (flipper (row-wpos (transpose a-grid) end empty))))
          (define some-grid (map (lambda (x)
                                   (string->list x)) (first puzzle)))     
          ;;(flipper lst-wpos) consumes lst-wpos and produces
          ;;   a new list of WPos with the wpos flipped.
          ;; flipper: (listof WPos) -> (listof WPos)
          (define (flipper lst-wpos)
            (cond [(empty? lst-wpos) empty]
                  [else
                   (cons (flip (first lst-wpos))
                         (flipper (rest lst-wpos)))]))
          ;;(row-wpos a-grid row-num acc) consumes a-grid,row-num,acc
          ;;    and produces a list of WPos that appears in a-grid.
          ;; row-wpos: Grid Nat (listof WPos) -> (listof WPos)
          ;; requires: row-num = 0, acc is empty
          (define (row-wpos a-grid row-num acc)
            (cond [(empty? a-grid) acc]
                  [else (row-wpos (rest a-grid) (add1 row-num)
                                  (append
                                   (find-wpos (first a-grid) row-num)
                                   acc))]))]          
    (make-state
     some-grid
     (allwpos some-grid)
     (second puzzle))))

;; Tests:
(check-expect (initial-state '(("####" "####" "####" "####")
                               ("PLSG" "MEMA" "ARKS"
                                       "PLSS" "PLSS"
                                       "HAHA" "PLSE"
                                       "SADF")))
              (make-state
               (list
                (list #\# #\# #\# #\#)
                (list #\# #\# #\# #\#)
                (list #\# #\# #\# #\#)
                (list #\# #\# #\# #\#))
               (list
                (make-wpos 3 0 true 4)
                (make-wpos 2 0 true 4)
                (make-wpos 1 0 true 4)
                (make-wpos 0 0 true 4)
                (make-wpos 0 3 false 4)
                (make-wpos 0 2 false 4)
                (make-wpos 0 1 false 4)
                (make-wpos 0 0 false 4))
               (list
                "PLSG"
                "MEMA"
                "ARKS"
                "PLSS"
                "PLSS"
                "HAHA"
                "PLSE"
                "SADF")))
(check-expect (initial-state '(("#" "#" "#")
                               ("LOL")))
              (make-state (list (list #\#)
                                (list #\#)
                                (list #\#))
                          (list 
                           (make-wpos 0 0 false 3))
                          (list "LOL")))
(check-expect (initial-state '(("###" "###" "###" "###")
                               ("PLS" "GIV" "ME"
                                      "PLS" "MAR"
                                      "SRY" "IAM"
                                      "DES")))
              (make-state
               (list (list #\# #\# #\#)
                     (list #\# #\# #\#)
                     (list #\# #\# #\#)
                     (list #\# #\# #\#))
               (list (make-wpos 3 0 true 3) (make-wpos 2 0 true 3)
                     (make-wpos 1 0 true 3)
                     (make-wpos 0 0 true 3) (make-wpos 0 2 false 4)
                     (make-wpos 0 1 false 4) (make-wpos 0 0 false 4))
               (list "PLS" "GIV" "ME" "PLS" "MAR" "SRY" "IAM" "DES")))
(check-expect (initial-state puzz02)
              (make-state
               (list
                (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
                (list #\. #\. #\. #\. #\. #\. #\# #\# #\# #\# #\.)
                (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
                (list #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\#)
                (list #\. #\. #\. #\. #\. #\. #\# #\. #\# #\. #\.)
                (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
                (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
                (list #\. #\. #\. #\# #\# #\# #\# #\. #\. #\. #\.)
                (list #\. #\. #\. #\# #\. #\. #\# #\. #\. #\. #\.)
                (list #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.))
               (list (make-wpos 9 0 true 4) (make-wpos 7 3 true 4)
                     (make-wpos 3 5 true 6) (make-wpos 1 6 true 4)
                     (make-wpos 0 8 false 5) (make-wpos 3 6 false 6)
                     (make-wpos 7 3 false 3))
               (list "ADAM" "ALBERT" "DAN" "DAVE" "JOHN" "KAREN" "LESLEY")))
(check-expect (initial-state puzz03)
              (make-state
               (list (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#)
                     (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#)
                     (list #\# #\# #\# #\# #\#))
               (list
                (make-wpos 4 0 true 5)
                (make-wpos 3 0 true 5)
                (make-wpos 2 0 true 5)
                (make-wpos 1 0 true 5)
                (make-wpos 0 0 true 5)
                (make-wpos 0 4 false 5)
                (make-wpos 0 3 false 5)
                (make-wpos 0 2 false 5)
                (make-wpos 0 1 false 5)
                (make-wpos 0 0 false 5))
               (list "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"
                     "SATOR" "AREPO" "TENET" "OPERA" "ROTAS")))


;; (extract-wpos g wp) consumes g and wp and produces
;;   (listof Char) corresponding to that word position within the Grid.
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (cond
    [(empty? g) empty]
    (else 
     (local
       [;; (extracter g row-or-col) consumes g and row-or-col and produces
        ;;   (listof Char) corresponding to that word position within the Grid.
        ;; extracter: Grid Nat -> (listof Char)
        (define (extracter g row-or-col)
          (cond
            [(empty? g) empty]
            [(zero? row-or-col) (first g)]
            [else (extracter (rest g) (sub1 row-or-col))]))
        ;; (transpose-or-norm horiz?) consumes horiz? and produces
        ;;   (listof Char) corresponding whether wpos is horiz?.
        ;; transpose-or-norm: Bool -> (listof Char)
        (define (transpose-or-norm horiz?)
          (cond
            [horiz? (extracter g (wpos-row wp))]
            [else (extracter (transpose g) (wpos-col wp))]))
        ;; (word-grab loc len) consumes loc and len and produces
        ;;   (listof Char) corresponding to that word position within the Grid.
        ;; word-grab: (listof Char) Nat -> (listof Char)
        (define (word-grab loc len)
          (cond
            [(empty? loc) empty]
            [(zero? len) empty]
            [else (append (list (first loc))
                          (word-grab (rest loc) (sub1 len)))]))]
       (word-grab (transpose-or-norm (wpos-horiz? wp))
                  (wpos-len wp))))))

;; Tests:
(check-expect (extract-wpos grid-abc (make-wpos 1 1 true 2))
              (list #\X #\Y))
(check-expect (extract-wpos grid-abcd (make-wpos 2 1 true 2))
              (list #\E #\F))
(check-expect (extract-wpos grid-abcd (make-wpos 1 1 true 2))
              (list #\X #\Y))
(check-expect (extract-wpos grid-abcd (make-wpos 0 1 true 3))
              (list #\A #\B #\C))
(check-expect (extract-wpos grid-abcd (make-wpos 3 2 true 2))
              empty)

;; (replace-wpos g wp loc) consumes g, wp, and loc and produces the Grid with
;;   the word position replaced by the word represented by the (listof Char)
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (cond
    [(empty? g) empty]
    [else
     (local
       [;; (transpose-or-norm horiz?) consumes horiz? and produces the Grid with
        ;;   the word position replaced by the word represented by the
        ;;   (listof Char)
        ;; transpose-or-norm: Bool -> Grid
        (define (transpose-or-norm horiz?)
          (cond
            [horiz? (grid-extract g (wpos-row wp))]
            [else (transpose (grid-extract (transpose g) (wpos-col wp)))]))
        ;; (grid-extract grid row-or-col) consumes grid and row-or-col and
        ;;   produces the extracted (listof Char) based on col-or-row
        ;; grid-extract: Grid Nat -> (listof Char)
        (define (grid-extract grid row-or-col)
          (cond
            [(empty? grid) empty]
            [(zero? row-or-col)
             (append (list (rep-loc (first grid)
                                    (wpos-len wp) loc)) (rest grid))]
            [else (append (list (first grid))
                          (grid-extract (rest grid) (sub1 row-or-col)))]))
        ;; (rep-loc f-loc len loc) consumes f-loc, len, and loc produces
        ;;   the extracted (listof Char) based on col-or-row.
        ;; rep-loc: (listof Char) Nat (listof Char) -> (listof Char)
        (define (rep-loc f-loc len loc)
          (cond
            [(empty? f-loc) empty]
            [(zero? len) f-loc]
            [else (append (list (first loc))
                          (rep-loc (rest f-loc) (sub1 len) (rest loc)))]))]
       (transpose-or-norm (wpos-horiz? wp)))]))

;; Tests:
(check-expect (replace-wpos grid-abcd (make-wpos 0 2 false 2) '(#\J #\K))
              (list (list #\A #\B #\J) (list #\X #\Y #\K) (list #\E #\F #\G)))
(check-expect (replace-wpos grid-abcd (make-wpos 1 2 false 2) '(#\J #\K))
              (list (list #\A #\B #\J) (list #\X #\Y #\K) (list #\E #\F #\G)))
(check-expect (replace-wpos grid-abcd (make-wpos 0 3 false 2) '(#\J #\K))
              (list (list #\A #\B #\C) (list #\X #\Y #\Z) (list #\E #\F #\G)))
(check-expect (replace-wpos grid-abcd (make-wpos 1 3 false 2) '(#\J #\K))
              (list (list #\A #\B #\C) (list #\X #\Y #\Z) (list #\E #\F #\G)))
(check-expect (replace-wpos empty (make-wpos 1 3 false 2) '(#\J #\K))
              empty)
(check-expect (replace-wpos grid-abc (make-wpos 1 3 false 2) '(#\J #\K))
              (list
               (list #\A #\B #\C)
               (list #\X #\Y #\Z)))


;; (fit? word cells) consumes a word as a (listof Char) and a (listof Char)
;;   that represents a word position in the puzzle and produces true if the
;;   word can successfully be placed in the corresponding word position
;; fit?: (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS")
                    (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS")
                    (string->list "S##RT##K")) false)
(check-expect (fit? (string->list "STARWARS")
                    (string->list "S##RW##S#")) false)

(define (fit? word cells)
  (cond
    [(and (empty? word) (empty? cells)) true]
    [(or (empty? word) (empty? cells)) false]
    [(or (equal? (first word) (first cells)) (equal? (first cells) char-find))
     (fit? (rest word) (rest cells))]
    [else false]))  

;; Tests:
(check-expect (fit? (string->list "STARWARS")
                    (string->list "S##RE##S")) false)
(check-expect (fit? (string->list "STAARS")
                    (string->list "S##RW#")) false)
(check-expect (fit? (string->list "STA")
                    (string->list "S##")) true)
(check-expect (fit? (string->list "STF")
                    (string->list "S#")) false)
(check-expect (fit? empty empty) true)
(check-expect (fit? empty "A#") false)
(check-expect (fit? "AB" empty) false)

;; (neighbours s) consumes s and produces (listof State) that represents
;;   valid neighbour States with one additional word placed in the puzzle
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours state)
  (local [;; (find-neigh g pos w-init all-words) consumes g, pos, w-init and
          ;;   all-words produces (listof State) that represents valid neighbour
          ;;   States with one additional word placed in the puzzle
          ;; neighbours: Grid Nat (listof Char) (listof Char) -> (listof State)
          (define (find-neigh g pos w-init all-words)
            (cond
              [(empty? w-init) empty]
              [(fit? (string->list (first w-init))
                     (extract-wpos g (first pos)))
               (cons
                (make-state (replace-wpos
                             g
                             (first pos) (string->list (first w-init)))
                            (rest pos)
                            (remove (first w-init) all-words))
                (find-neigh g pos (rest w-init) all-words))]
              [else (find-neigh g pos (rest w-init) all-words)]))]
    (find-neigh (state-grid state) (state-positions state) (state-words state)
                (state-words state))))

;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))
(check-expect (make-state '((#\C #\. #\#))
                          (list (make-wpos 0 0 true 3))
                          '("CAT" "DOG" "CAR"))
              (make-state (list (list #\C #\. #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT" "DOG" "CAR")))
(check-expect (make-state '((#\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          '("CAT" "DOG" "CAR"))
              (make-state
               (list (list #\# #\# #\#))
               (list (make-wpos 0 0 true 3))
               (list "CAT" "DOG" "CAR")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

(check-expect (criss-cross puzz01) '("CAT"))
