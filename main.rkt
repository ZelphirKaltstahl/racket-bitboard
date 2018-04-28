#lang racket

(require data/bit-vector)

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

(define (bb-binary-operation board1 board2 proc)
  (let ([bits-b1 (bitboard-bits board1)]
        [bits-b2 (bitboard-bits board2)])
    (bitboard (for/bit-vector ([b1 (in-bit-vector bits-b1)]
                               [b2 (in-bit-vector bits-b2)])
                              (proc b1 b2))
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

#;(time
 (for ([i (in-range 50000)])
   (bb-or (create-bitboard #:initial #t)
          (create-bitboard #:initial #f))))

#;(time
 (for ([i (in-range 50000)])
   (bb-and (create-bitboard #:initial #t)
           (create-bitboard #:initial #f))))

#;(time
 (for ([i (in-range 50000)])
   (bb-xor (create-bitboard #:initial #t)
           (create-bitboard #:initial #f))))

#;(time
 (for ([i (in-range 50000)])
   (bb-not (create-bitboard #:initial #f))))

#;(time
 (for ([i (in-range 50000)])
   (bb-any (create-bitboard #:initial #f))))
