(define (square x) (* x x))

(define (bitwise-toggle x pos)
  (bitwise-xor
    x
    (arithmetic-shift 1 pos)))

(define (circular-index pos offset size)
  (modulo (+ pos (remainder offset size) size) size))

(define (opt order x len fitness (cmp >) (prng random))
  (let opt-run ((run-x x)
                (run-fitness (fitness x)))
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
        (values obtained-x obtained-fitness)))))
