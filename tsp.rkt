#lang racket

(define (tsp num-cities max-cost (min-cost 1))
  (build-vector
    num-cities
    (λ (i)
       (build-vector
         num-cities
         (λ (j)
            (if (= i j)
              (cons #f 0)
              (cons #t (+ min-cost
                 (random (- max-cost min-cost))))))))))

(define (bits-to-tsp-solution size-bits bit-solution tsp-problem)
  (let ((tsp-problem (vector-copy tsp-problem))
        (tsp-size (vector-length tsp-problem)))
    (let consume-bits ((bit-position (sub1 size-bits))
                       (row 0)
                       (rows-left tsp-size)
                       (element-index 0)
                       (elements-left tsp-size))
      (cond
        ((= rows-left 0) (values 0 empty))
        ((= elements-left 1)
              (vector-set!
                (vector-ref tsp-problem row)
                element-index
                (cons
                  #f
                  (cdr (vector-ref
                         (vector-ref tsp-problem row)
                         element-index))))
              (vector-set!
                (vector-ref tsp-problem element-index)
                row
                (cons
                  #f
                  (cdr (vector-ref
                         (vector-ref tsp-problem element-index)
                         row))))
              (let-values (((cost-so-far soln-so-far)
                           (consume-bits bit-position
                                         element-index
                                         (sub1 rows-left)
                                         0
                                         tsp-size)))
                (values (+ cost-so-far
                           (cdr (vector-ref (vector-ref tsp-problem row)
                                       element-index)))
                        (cons element-index soln-so-far))))
        (else
          (let ((new-elements-left (arithmetic-shift elements-left 1)))
            (if (bitwise-bit-set? bit-solution bit-position)
              (consume-bits (sub1 bit-position)
                            row
                            rows-left
                            (+ element-index new-elements-left)
                            (- elements-left new-elements-left))
              (consume-bits (sub1 bit-position)
                            row
                            rows-left
                            element-index
                            (- elements-left new-elements-left)))))))))
