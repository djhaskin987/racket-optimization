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
                               (and/c integer? (not/c negative?))))
           (non-negative-integer? (-> any/c boolean?))
           (positive-integer? (-> any/c boolean?))
           (portion? (-> any/c boolean?))))

(define (non-negative-integer? a)
  (and (number? a)
       (exact-integer? a)
       (not (negative? a))))

(define (positive-integer? a)
  (and (number? a) 
       (exact-integer? a)
       (not (negative? a))))

(define (portion? a)
  (and (number? a)
       (>= a 0)
       (< a 1)))

(define (pick-without-replacement k 
                                  subject-list 
                                  subject-list-size
                                  (prng random))
  (let pick-without-replacement-rec
    ((chosen-list empty)
     (num-left k)
     (residue-list empty)
     (source-list subject-list)
     (source-list-size subject-list-size))
    (if (or (= num-left 0) (empty? source-list))
      (values chosen-list
              (append residue-list source-list))
      (if (< (prng) (/ num-left source-list-size))
        (pick-without-replacement-rec
          (cons (first source-list) chosen-list)
          (sub1 num-left)
          residue-list
          (rest source-list)
          (sub1 source-list-size))
        (pick-without-replacement-rec
          chosen-list
          num-left
          (cons (first source-list) residue-list)
          (rest source-list)
          (sub1 source-list-size))))))

(define (square x) (* x x))

(define (circular-index pos offset size)
  (modulo (+ pos (remainder offset size) size) size))

(define (bitwise-toggle x pos)
  (bitwise-xor
    x
    (arithmetic-shift 1 pos)))
