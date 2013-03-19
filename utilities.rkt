#lang racket

(provide (contract-out
           (pick-without-replacement
             (-> (and/c exact-integer? (not/c negative?))
                 list?
                 (and/c exact-integer? (not/c negative?))
                 procedure?
                 (values list? list?)))
           (count-bits
             (-> (and/c exact-integer? (not/c negative?))
                 (and/c exact-integer? (not/c negative?))))
           (square
             (-> number? number?))
           (circular-index
             (-> (and/c exact-integer? (not/c negative?))
                 exact-integer?
                 positive?
                 (and/c exact-integer? (not/c negative?))))
           (bitwise-toggle
             (-> (and/c exact-integer? (not/c negative?))
                 (and/c exact-integer? (not/c negative?))
                 (and/c exact-integer? (not/c negative?))))
           (non-negative-integer? (-> any/c boolean?))
           (truth-value? (-> any/c boolean?))))

(define (non-negative-integer? a)
  (and (number? a)
       (exact-integer? a)
       (not (negative? a))))

(define (truth-value? a)
  (and (number? a)
       (>= a 0)
       (<= a 1)))

(define (count-bits number)
  (if (zero? number)
    0
    (let ((count-rest (count-bits (arithmetic-shift number -1))))

      (if (zero? (bitwise-and 1 number))
        count-rest
        (add1 count-rest)))))

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
