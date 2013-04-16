#lang racket

(require "utilities.rkt")

(provide
  long-random
  (contract-out
    (struct solution
            ((bits exact-positive-integer?)
             (fitness number?)))
    (single-crossover
      (-> exact-positive-integer?
          (-> exact-positive-integer? non-negative-integer?)))
    (double-crossover
      (-> exact-positive-integer?
          (-> exact-positive-integer? non-negative-integer?)))
    (uniform-crossover
      (-> exact-positive-integer?
          (-> exact-positive-integer? non-negative-integer?)))
    (genetic (-> (-> non-negative-integer? number?)
                 exact-positive-integer?
                 exact-positive-integer?
                 (-> number? number? boolean?)
                 procedure?
                 list?))))

(define-struct solution (bits fitness) #:transparent)

(define (long-random (number empty))
  (if (empty? number)
    (random)
    (if (> (integer-length number) 31)
      (bitwise-ior
        (arithmetic-shift (random (sub1 (arithmetic-shift 31 1))) 31)
        (long-random (arithmetic-shift number -31)))
      (random number))))

;; Crossover section
(define (single-crossover len (prng long-random))
  (sub1 (arithmetic-shift 1 (- len (prng len)))))


(define (double-crossover len (prng long-random))
  (bitwise-xor
    (single-crossover len prng)
    (single-crossover len prng)))

(define (uniform-crossover len (prng long-random))
  (prng (arithmetic-shift 1 len)))

;; Mutation and Generation
(define (generate a b n fitness (prng long-random))
  (let ((crossover-mask (single-crossover n prng)))
    (let ((bits (bitwise-ior (bitwise-and (solution-bits a) crossover-mask)
                             (bitwise-and (solution-bits b)
                                          (bitwise-not crossover-mask)))))
      (solution bits (fitness bits)))))

(define (mutate a b n fitness (prng long-random))
  (let ((probability-of-mutation
          (* 4 (square (1/2 - (/ (- n (count-bits (bitwise-xor a b))) n))))))
    (if (< (prng) (exact->inexact probability-of-mutation))
      (let ((new-bits (bitwise-toggle (solution-bits a) (prng n))))
        (solution new-bits (fitness new-bits)))
        a)))

(define (genetic fitness n len (less-than? <) (prng long-random))
  (let ((population (build-list len (λ (n) (prng (arithmetic-shift 1 len))))))
    (let countdown ((times 10000)
                    (pop population))
      (if (zero? times)
        pop
        (countdown
          (sub1 times)
          (let-values (((chosen residue)
                        (pick-without-replacement 4 population
                                                  (length population)
                                                  prng)))
            (let ((foe-1 (first chosen))
                  (foe-2 (second chosen))
                  (hottie (third chosen))
                  (buff (fourth chosen)))
              (cons
                ;; change to killing off by rank
                (if (less-than? (prng) (/ (solution-fitness foe-1)
                                          (+ (solution-fitness foe-1)
                                             (solution-fitness foe-2))))
                  foe-1
                  foe-2)
                (cons
                  (generate hottie buff fitness n prng)
                  (cons
                    (mutate hottie buff fitness n prng)
                    (cons
                      buff
                      residue)))))))))))
