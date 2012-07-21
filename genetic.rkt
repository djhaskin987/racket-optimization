#lang racket

(require "utilities.rkt")

(define (single-crossover len (prng random))
  (let ((crossover-point (add1 (prng (sub1 len)))))
    (arithmetic-shift 
      (sub1 (expt 2 (- len crossover-point))) crossover-point)))

(define (double-crossover len (prng random))
  (let ((first-crossover-point (prng (add1 len)))
        (second-crossover-point (prng (add1 len))))
    (arithmetic-shift 
      (sub1 (expt 2 (abs (- first-crossover-point second-crossover-point))))
        (min first-crossover-point second-crossover-point))))

(define (uniform-crossover len (prng random))
  (prng (expt 2 len)))

(define (mutate patient len (prng random))
  (bitwise-toggle patient (prng len)))

(define (generate mother father mask)
  (bitwise-ior (bitwise-and mother (not mask))
               (bitwise-and father mask)))

(define (family mother father size len crossover (prng random))
  (let breeding ((children empty)
                 (num-left size))
    (if (<= num-left 0)
      (cons father
            (cons mother children))
      (breeding
        (cons
          (generate 
            mother
            father
            (crossover len prng))
          children)
        (sub1 num-left)))))

(define (genetic population-size
                 solution-length
                 fitness-function
                 (replacement-rate 1/10)
                 (mutation-rate 1/20)
                 (crossover single-crossover)
                 (prng random))
  (let-values (((population max-solution min-solution avg-solution)
                (let population-init ((population-list empty)
                                      (solution-sum 0)
                                      (max-so-far 0)
                                      (min-so-far (expt 2 solution-length))
                                      (num-left population-size))
                  (if (<= num-left 0)
                    (values population-list 
                            max-so-far 
                            min-so-far 
                            (/ solution-sum population-size))
                    (let ((new-solution (prng (expt 2 solution-length))))
                      (population-init 
                        (cons new-solution population-list)
                        (+ solution-sum new-solution)
                        (max max-so-far new-solution)
                        (min min-so-far new-solution)
                        (sub1 num-left)))))))
        (pop-max
                 
