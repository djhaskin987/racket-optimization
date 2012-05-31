(define (square x) (* x x))

(define (test-fitness x)
  (* (square (+ x 5))
     (+ (square x)
        (* -27 x)
        216)))

(define (bitwise-toggle x pos)
  (bitwise-xor
    x
    (arithmetic-shift 1 pos)))

(define (opt order x len fitness (cmp >))
  (let opt-rec ((ord order)
                (cur-x x)
                (cur-fit (fitness x))
                (start 0)
                (end (- len order)))
    (if (>= start end)
      (values cur-x cur-fit)
      (let ((len-rec (- end start)))
        (let floop ((current-bit start)
                    (current-x cur-x)
                    (current-fit cur-fit))
          (if (>= current-bit end)
            (values current-x current-fit)
            (let* ((changed-x
                     (bitwise-toggle current-x current-bit))
                   (changed-fitness (fitness changed-x)))
              (let-values (((tried-x tried-fitness)
                            (if (> order 0)
                              (opt-rec (sub1 order)
                                       changed-x
                                       changed-fitness
                                       (add1 current-bit)
                                       (add1 end))
                              (values changed-x changed-fitness))))
                (if (cmp tried-fitness current-fit)
                  (floop (add1 current-bit)
                         tried-x
                         tried-fitness)
                  (floop (add1 current-bit)
                         current-x
                         current-fit))))))))))
