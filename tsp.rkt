(define (tsp num-cities max-cost (min-cost 1))
  (build-vector
    num-cities
    (λ (i)
       (build-vector
         num-cities
         (λ (j)
            (if (= i j)
              0
              (+ min-cost
                 (random (- max-cost min-cost)))))))))
