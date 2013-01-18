#lang racket

(require "utilities.rkt")

(define-struct solution (bits fitness))

(provide
  (contract-out
    (struct solution
            (positive-integer? number?))
    (single-crossover
      (-> positive-integer? (-> positive-integer? non-negative-integer?)))
    (double-crossover
      (-> positive-integer? (-> positive-integer? non-negative-integer?)))
    (uniform-crossover
      (-> positive-integer? (-> positive-integer? non-negative-integer?)))
    (genetic (-> positive-integer?
                 positive-integer?
                 (-> non-negative-integer? number?)
                 number?
                 number?
                 portion?
                 portion?
                 (-> positive-integer? (-> positive-integer?
                                           non-negative-integer?))
                 procedure?))))

# Crossover section
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

# Mutation and Generation
(define (mutate patient len (prng random))
  (bitwise-toggle patient (prng len)))

(define (generate mother father mask)
  (bitwise-ior (bitwise-and mother (bitwise-not mask))
               (bitwise-and father mask)))

# (-> (listof solution?) solution? solution? positive-integer?
#     positive-integer? (-> solution? solution? solution?) procedure?)
(define (add-family pool
                    mother
                    father
                    size
                    len
                    (crossover single-crossover)
                    (prng random))
  (if (<= size 0)
    (cons mother
          (cons father pool))
    (add-family (cons
                  (generate mother
                            father
                            (crossover len
                                       rng))
                  pool)
                mother
                father
                (sub1 size)
                len
                crossover
                prng)))

# Battle and elimination

# (-> solution? solution? (-> number? boolean?) procedure?)
(define (battle
          left
          right
          (cmp >)
          (prng random))
  (if (cmp
         (/ (solution-fitness left)
            (+ (solution-fitness left)
               (solution-fitness right))
         (prng))
    left
    right))

(define (war blues
             reds
             (cmp >)
             prng)
  (let war-rec ((left blues)
                (right reds)
                (victors empty))
    (if (or (empty? left)
            (empty? right))
      victors
      (war-rec (rest left)
               (rest right)
               (cons (battle
                       (first left)
                       (first right)
                       cmp)
                     victors)))))


(define (select-competitors match-ups-count
                            subject-list
                            subject-list-size
                            (prng random))
  (if (> (arithmetic-shift match-ups-count 1)
         subject-list-size)
    (error 'select-competitors "more match ups than the subject size allows")
    (let select-competitors-rec
      ((chosen-list-left empty)
       (chosen-list-right empty)
       (num-left-left match-ups-count)
       (num-left-right match-ups-count)
       (source-list subject-list)
       (source-list-size subject-list-size)
       (residue-list empty))
      (if (or
            (and
              (= num-left-left 0)
              (= num-left-right 0))
            (empty? source-list))
        (values chosen-list-left
                chosen-list-right
                (append residue-list source-list))
        (let ((dice (prng)))
          (cond ((< dice (/ num-left-left source-list-size))
                 (select-competitors-rec
                   (cons (first source-list) chosen-list-left)
                   chosen-list-right
                   (sub1 num-left-left)
                   num-left-right
                   (rest source-list)
                   (sub1 source-list-size)
                   residue-list))
                ((< dice (/ (+ num-left-left num-left-right)
                            source-list-size))
                 (select-competitors-rec
                   chosen-list-left
                   (cons (first source-list) chosen-list-right)
                   num-left-left
                   (sub1 num-left-right)
                   (rest source-list)
                   (sub1 source-list-size)
                   residue-list))
                (else
                  (select-competitors-rec
                    chosen-list-left
                    chosen-list-right
                    num-left-left
                    num-left-right
                    (rest source-list)
                    (sub1 source-list-size)
                    (cons (first source-list) residue-list)))))))))

(define (eliminate population
                   population-size
                   solution-length
                   fitness-function
                   replacement-rate
                   prng)
  (let replacement
    ((leftover-replacement-rate replacement-rate)
     (leftover-population population)
     (leftover-population-size population-size))
    (if (< leftover-replacement-rate 1/2)
      (let-values ((
                    (pick-without-replacement
                      k
                      subject-list
                      subject-size
                      prng)))))))

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

