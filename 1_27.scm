  (define (fast-expt b n)
    (cond ((= n 0) 1)
    	  ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (all-less-congruent? n)
  (define (congruent-modulo? a b) 
    (= (remainder a n) (remainder b n)))
  
  (define (test-congruent counter)
    (cond ((= counter 0) true)
          ((congruent-modulo? (fast-expt counter n) (remainder counter n)) (test-congruent (- counter 1)))
      	  (else false)))
 
  (test-congruent n)
)

;testing
(all-less-congruent? 5)
(all-less-congruent? 7)
(all-less-congruent? 8)
(all-less-congruent? 9)

;carmichaels
(all-less-congruent? 561)
(all-less-congruent? 1105)
(all-less-congruent? 1729)
(all-less-congruent? 2465)
(all-less-congruent? 2821
(all-less-congruent? 6601)