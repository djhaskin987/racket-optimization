(define (square x) (* x x))

(define (test-fitness x)
  (* (square (+ x 5))
     (+ (square x)
        (* -27 x)
        216)))

(define (opt order x len fitness (cmp >))
  (define (opt-rec ord cur-x cur-fit start end)
    (if (>= start end)
      (values cur-x cur-fit)
      (let floop ((num-left (- end start))
                  (current-bit start)
                  (current-x cur-x)
                  (current-fit cur-fit))
        (if (> num-left 0)
          (let* ((changed-x
                   (bitwise-xor
                     current-x
                     (arithmetic-shift 1 current-bit)))
                 (changed-fitness (fitness changed-x))
                 (tried-x
                   (if (> order 0)
                     (opt-rec (sub1 order)
                              changed-x
                              changed-fitness
                              (add1 current-bit)
                              (add1 end))
                     changed-x))
                 (tried-fitness (fitness tried-x)))
            (if (cmp tried-fitness current-fit)
              (floop (- end start)
                     (modulo (add1 current-bit) len)
                     tried-x
                     tried-fitness)
              (floop (sub1 num-left)
                     (modulo (add1 current-bit) len)
                     current-x
                     current-fit)))
          (values current-x
                  current-fit)))))
  (opt-rec order x (fitness x) 0 len))
