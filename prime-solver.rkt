#lang racket

(require "opt.rkt")

(define o
  (command-line #:args (o) (string->number o)))

(define (mod-op op a b n)
  (modulo (op
           (modulo a n)
           (modulo b n))
          n))

(define (get-s-d x)
  (let find-s-d ((s 0)
                 (d x))
    (if (even? d)
        (find-s-d (add1 s) (arithmetic-shift d -1))
        (values s d))))

(define (mod-exp a x n)
  (cond ((zero? x) 1)
        ((odd? x)
         (mod-op * (mod-exp (mod-op * a a n) (quotient x 2) n) a n))
        ((even? x)
         (mod-exp (mod-op * a a n) (quotient x 2) n))))

(define (prime? x k (prng random))
  (let-values (((s d) (get-s-d (sub1 x))))
    (let loop ((i 0))
      (cond
        ((>= i k)
         #t)
        (else
          (let ((test (mod-exp (add1 (variable-prng (- x 3))) d x)))
            (if (or (= test 1) (= test (sub1 x)))
              (loop (add1 i))
              (let carmichael-test ((r 1)
                                    (ctest (modulo (* test test) x)))
                (cond ((>= r s)
                       #f)
                      ((= ctest 1)
                       #f)
                      ((= ctest (sub1 x))
                       (loop (add1 i)))
                      (else
                        (carmichael-test
                          (add1 r)
                          (modulo
                            (* ctest ctest) x))))))))))))

(define (variable-prng (number 2147483648))
  (let long-random-number ((build-random-number 0)
                           (num-left (integer-length number)))
    (if (<= num-left 0)
      build-random-number
      (let ((num-bits
              (if
                (>= num-left 31)
                31
                num-left)))
      (long-random-number
        (bitwise-ior
          (arithmetic-shift build-random-number num-bits)
          (random (arithmetic-shift 1 num-bits)))
        (- num-left num-bits))))))

(define number
  (let get-number
    ((build-number 1)
     (num-left 2))
    (if (<= num-left 0)
      build-number
      (let ((tested (variable-prng (arithmetic-shift 1 10))))
        (if (prime? tested 20 variable-prng)
          (get-number (* build-number tested)
                      (sub1 num-left))
          (get-number build-number num-left))))))

(define (prime-solver number order)
  (let ((len (integer-length number)))
    (let prime-solver-loop ()
      (let-values (((x fitness)
                    (svopt order
                           (variable-prng number)
                           len
                           (lambda (x)
                             (if (and (> x 1)
                                      (< x number))
                               (remainder number x)
                               (arithmetic-shift 1 (* len 3))))
                           <
                           variable-prng)))
        x))))

(let ((a (prime-solver number o)))
  (printf "~a ~a ~a ~a"
          (integer-length number)
          number
          a
          (remainder number a)))
