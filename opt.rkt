(define (square x) (* x x))

(define (bitwise-toggle x pos)
  (bitwise-xor
    x
    (arithmetic-shift 1 pos)))

(define (circular-index pos offset size)
  (modulo (+ pos (remainder offset size) size) size))


(define (prime-solver number order)
  (let ((len (integer-length number)))
    (let prime-solver-loop ()
      (let-values (((x fitness)
                    (svopt order
                           (random number)
                           len (lambda (x) 
                                 (if (and (> x 1)
                                          (< x number))
                                   (remainder number x)
                                   (expt 2 (+ len 5))))
                           <)))
        (if (= fitness 0)
          x
          (prime-solver-loop))))))


(define (svopt order x len fitness (cmp >) (prng random))
  (let svopt-sentinel ((sent-x x)
                       (sent-fitness-x (fitness x)))
    (let-values (((sent-obtained-x sent-obtained-fitness-x)
                  (let svopt-loop ((loop-order 0)
                                   (loop-x sent-x)
                                   (loop-fitness-x sent-fitness-x))
                    (if (>= loop-order order) 
                      (values loop-x loop-fitness-x)
                      (let-values (((obtained-x obtained-fitness-x)
                                    (opt loop-order
                                         loop-x
                                         loop-fitness-x
                                         len
                                         fitness
                                         cmp
                                         prng)))
                        (if (cmp obtained-fitness-x loop-fitness-x)
                          (svopt-loop (add1 loop-order)
                                      obtained-x 
                                      obtained-fitness-x)
                          (svopt-loop (add1 loop-order)
                                      loop-x
                                      loop-fitness-x)))))))
      (if (cmp sent-obtained-fitness-x sent-fitness-x)
        (svopt-sentinel sent-obtained-x
                        sent-obtained-fitness-x)
        (values sent-x sent-fitness-x)))))

(define (opt order x x-fitness len fitness (cmp >) (prng random))
  (let opt-run ((run-x x)
                (run-fitness x-fitness))
    (let-values (((obtained-x obtained-fitness)
                  (let opt-rec ((ord order)
                                (cur-x run-x)
                                (cur-fit run-fitness)
                                (start (prng len))
                                (rec-len len))
                    (if (or (>= order rec-len)
                            (<= rec-len 0))
                      (values cur-x cur-fit)
                      (let floop ((current-bit start)
                                  (current-x cur-x)
                                  (current-fit cur-fit)
                                  (loop-length rec-len))
                        (if (<= loop-length 0)
                          (values current-x current-fit)
                          (let* ((changed-x
                                   (bitwise-toggle current-x current-bit))
                                 (changed-fitness (fitness changed-x)))
                            (let-values
                              (((tried-x tried-fitness)
                                (if (> ord 0)
                                  (opt-rec (sub1 ord)
                                           changed-x
                                           changed-fitness
                                           (circular-index current-bit 1 len)
                                           (sub1 loop-length))
                                  (values changed-x changed-fitness))))
                              (if (cmp tried-fitness current-fit)
                                (floop (circular-index current-bit 1 len)
                                       tried-x
                                       tried-fitness
                                       (sub1 loop-length))
                                (floop (circular-index current-bit 1 len)
                                       current-x
                                       current-fit
                                       (sub1 loop-length)))))))))))
      (if (cmp obtained-fitness run-fitness)
        (opt-run obtained-x obtained-fitness)
        (values run-x run-fitness)))))
