#lang racket

(provide (contract-out 
           (pick-without-replacement (-> (and/c integer? (not/c negative?))
                                         cons?
                                         (and/c integer? (not/c negative?))
                                         (-> integer?
                                             (and/c integer? (not/c negative?)))
                                         cons?))
           (square (-> number? number?))
           (circular-index (-> (and/c integer? (not/c negative?))
                               integer?
                               positive?
                               (and/c integer? (not/c negative?))))
           (bitwise-toggle (-> (and/c integer? (not/c negative?))
                               (and/c integer? (not/c negative?))
                               (and/c integer? (not/c negative?))))))



(define (pick-without-replacement k 
                                  subject-list 
                                  subject-list-size 
                                  (prng random))
  (if (or (= k 0) (empty? subject-list))
    empty
    (let ((dice (prng)))
      (if (< dice (/ k subject-list-size))
        (cons (first subject-list)
              (pick-without-replacement
                prng
                (- k 1)
                (rest subject-list)
                (- subject-list-size 1)))
        (pick-without-replacement
          prng
          k
          (rest subject-list)
          (- subject-list-size 1))))))

(define (square x) (* x x))

(define (circular-index pos offset size)
  (modulo (+ pos (remainder offset size) size) size))

(define (bitwise-toggle x pos)
  (bitwise-xor
    x
    (arithmetic-shift 1 pos)))
