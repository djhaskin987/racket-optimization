(define (opt num order fitness (relation >))
  (define (opt-rec number number-fitness current-bit opt-order)
    (cond ((> current-bit (integer-length number))
           (error 'opt "Current bit must not exceed number's length."))
          ((= current-bit 0)
           (values number number-fitness))
          ((> opt-order (add1 current-bit))
           (values number number-fitness))
          (else
            (let-values (((base-number base-fitness)
                     (if (= order 1)
                       (values number number-fitness)
                       (opt-rec number
                                number-fitness
                                (sub1 current-bit)
                                (sub1 opt-order)))))
              (let* ((checked-permutation
                       (bitwise-xor number
                                    (arithmetic-shift 1 current-bit)))
                     (checked-fitness
                       (fitness checked-permutation)))
                (let-values
                  (((checked-number checked-number-fitness)
                    (if (= order 1)
                      (values checked-permutation checked-fitness)
                      (opt-rec checked-permutation
                               checked-fitness
                               (sub1 current-bit)
                               (sub1 opt-order)))))
              (if (relation checked-number-fitness base-fitness)
                (opt-rec checked-number
                         checked-number-fitness
                         (sub1 current-bit)
                         opt-order)
                (opt-rec base-number
                         base-fitness
                         (sub1 current-bit)
                         opt-order))))))))
  (opt-rec num (fitness num) (sub1 (integer-length num)) order))
