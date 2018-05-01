#lang racket

(require data/bit-vector)
(require rackunit)

(provide (all-defined-out))


(struct bitboard
  (bits height width kind)
  #:transparent)

(define (create-bitboard [height 9]
                         [width 9]
                         #:kind [kind 'unspecified]
                         #:initial [initial #f])
  (let ([num-bits (ceiling (* height width))])
    (bitboard (make-bit-vector num-bits initial)
              9
              9
              kind)))

(define (get-coord board vpos hpos)
  (let* ([height (bitboard-height board)]
         [width (bitboard-width board)]
         [index (+ (* vpos width) hpos)])
    (bit-vector-ref (bitboard-bits board) index 'out-of-bounds)))

(define (index-to-coords index row-count col-count)
  (cond [(>= index (* row-count col-count))
         (raise-argument-error 'index
                               "index for which (< index (* row-count col-count))"
                               0
                               index row-count col-count)]
        [(not (or (exact-positive-integer? index)
                  (and (exact-integer? index)
                       (= index 0))))
         (raise-argument-error 'index
                               "exact-positive-integer? or an exact-integer? which is equal to zero"
                               0
                               index row-count col-count)]
        [(let* ([row-coord (floor (/ index col-count))]
                [col-coord (- index (* row-coord col-count))])
           (cons row-coord col-coord))]))

(define (get-coords-of-trues board)
  (define (iter bits row-count col-count index)
    (cond
      [(< index (bit-vector-length bits))
       #;(printf "index still less than length~n")
       (let ([bit (bit-vector-ref bits index)])
         (cond
           [bit (cons (index-to-coords index row-count col-count)
                      (iter bits
                            row-count
                            col-count
                            (add1 index)))]
           [else (iter bits
                       row-count
                       col-count
                       (add1 index))]))]
      [else '()]))
  (iter (bitboard-bits board)
        (bitboard-height board)
        (bitboard-width board)
        0))



(define (binary-bit-vector-operation bv1 bv2 proc)
  (for/bit-vector ([b1 (in-bit-vector bv1)]
                   [b2 (in-bit-vector bv2)])
                  (proc b1 b2)))

(define (bb-binary-operation board1 board2 proc)
  (let ([bits-b1 (bitboard-bits board1)]
        [bits-b2 (bitboard-bits board2)])
    (bitboard (binary-bit-vector-operation bits-b1 bits-b2 proc)
              (min (bitboard-height board1)
                   (bitboard-height board2))
              (min (bitboard-width board1)
                   (bitboard-width board2))
              (if (equal? (bitboard-kind board1)
                          (bitboard-kind board2))
                  (bitboard-kind board1)
                  'unspecified))))

(define (bb-unary-operation board proc)
  (let ([bits (bitboard-bits board)])
    (bitboard (for/bit-vector ([a-bit (in-bit-vector bits)])
                              (proc a-bit))
              (bitboard-height board)
              (bitboard-width board)
              (bitboard-kind board))))

(define (bb-fold board proc initial)
  (let ([bits (bitboard-bits board)])
    (for/fold ([acc initial])
              ([a-bit (in-bit-vector bits)])
      (proc acc a-bit))))

(define (bb-and board1 board2)
  (bb-binary-operation board1
                       board2
                       (λ (bit1 bit2)
                         (and bit1 bit2))))

(define (bb-or board1 board2)
  (bb-binary-operation board1
                       board2
                       (λ (bit1 bit2)
                         (or bit1 bit2))))

(define (bb-not board)
  (bb-unary-operation board
                      (λ (a-bit)
                        (not a-bit))))

(define (bb-any board)
  (bb-fold board
           (λ (acc a-bit)
             (or acc a-bit))
           #f))

(define (bb-xor board1 board2)
  (bb-binary-operation board1
                       board2
                       (λ (bit1 bit2)
                         (or (and (not bit1) bit2)
                             (and bit1 (not bit2))))))

(test-case "create-bitboard test case"
  (let ([test-board (create-bitboard 10
                                     10
                                     #:kind 'unspecified
                                     #:initial #t)])
    (check-equal? (bitboard-bits test-board)
                  (make-bit-vector (* 10 10) #t)
                  "bits of bitboard after creation not correct")))

(test-case "get-coord test case"
  (let ([test-board (bitboard
                     (string->bit-vector
                      (string-append "000000001"
                                     "000000010"
                                     "000000100"
                                     "000001000"
                                     "000010000"
                                     "000100000"
                                     "001000000"
                                     "010000000"
                                     "100000000"))
                     9
                     9
                     'shogi)])
    (for* ([row (in-range 9)]
           [col (in-range 9)])
      (cond [(= (+ row col) 8)
             (check-equal? (get-coord test-board row col) #t)]
            [else
             (check-equal? (get-coord test-board row col) #f)]))))

(test-case "bb-or test case"
  (let* ([test-board1 (bitboard
                       (string->bit-vector
                        (string-append "000000001"
                                       "000000010"
                                       "000000100"
                                       "000001000"
                                       "000010000"
                                       "000100000"
                                       "001000000"
                                       "010000000"
                                       "100000000"))
                       9
                       9
                       'shogi)]
         [test-board2 (bitboard
                       (string->bit-vector
                      (string-append "100000000"
                                     "010000000"
                                     "001000000"
                                     "000100000"
                                     "000010000"
                                     "000001000"
                                     "000000100"
                                     "000000010"
                                     "000000001"))
                       9
                       9
                       'shogi)]
         [or-board (bb-or test-board1 test-board2)])
    (for* ([row (in-range 9)]
           [col (in-range 9)])
      (cond [(or (= (+ row col) 8)
                 (= row col))
             (check-equal? (get-coord (bb-or test-board1 test-board2) row col)
                           #t)]
            [else
             (check-equal? (get-coord or-board row col) #f)]))))

(test-case "index-to-coords test case"
  (check-exn exn:fail:contract? (λ () (index-to-coords 81 9 9)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 81 10 8)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 80 8 10)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 80 8 10)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 6 3 2)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 7 3 2)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 1 1 1)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 81 -1 9)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 81 9 -1)))
  (check-exn exn:fail:contract? (λ () (index-to-coords 81 -4 -4)))
  (check-exn exn:fail:contract? (λ () (index-to-coords -70 9 9)))
  (check-equal? (index-to-coords 0 1 1) (cons 0 0))
  (check-equal? (index-to-coords 40 9 9) (cons 4 4))
  (check-equal? (index-to-coords 999 10 100) (cons 9 99))
  (check-equal? (index-to-coords 14 5 4) (cons 3 2))
  (check-equal? (index-to-coords 543 55 10) (cons 54 3))
  (check-equal? (index-to-coords 74 10 10) (cons 7 4)))

(test-case "get-coords-of-trues test case"
  (let ([test-board (bitboard
                     (string->bit-vector
                      (string-append "000000001"
                                     "000000010"
                                     "000000100"
                                     "000001000"
                                     "000010000"
                                     "000100000"
                                     "001000000"
                                     "010000000"
                                     "100000000"))
                     9
                     9
                     'shogi)])
    (check-equal? (get-coords-of-trues test-board)
                  '((0 . 8)
                    (1 . 7)
                    (2 . 6)
                    (3 . 5)
                    (4 . 4)
                    (5 . 3)
                    (6 . 2)
                    (7 . 1)
                    (8 . 0))))
  (let ([test-board (bitboard
                     (string->bit-vector
                      (string-append "111111111"
                                     "100000001"
                                     "101111101"
                                     "101000101"
                                     "101010101"
                                     "101000101"
                                     "101111101"
                                     "100000001"
                                     "111111111"))
                     9
                     9
                     'shogi)])
    (check-equal?
     (get-coords-of-trues test-board)
     (append '((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5) (0 . 6) (0 . 7) (0 . 8))
             '((1 . 0) (1 . 8))
             '((2 . 0) (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6) (2 . 8))
             '((3 . 0) (3 . 2) (3 . 6) (3 . 8))
             '((4 . 0) (4 . 2) (4 . 4) (4 . 6) (4 . 8))
             '((5 . 0) (5 . 2) (5 . 6) (5 . 8))
             '((6 . 0) (6 . 2) (6 . 3) (6 . 4) (6 . 5) (6 . 6) (6 . 8))
             '((7 . 0) (7 . 8))
             '((8 . 0) (8 . 1) (8 . 2) (8 . 3) (8 . 4) (8 . 5) (8 . 6) (8 . 7) (8 . 8))))))
