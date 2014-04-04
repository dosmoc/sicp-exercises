(define (even? n)
  (= (remainder n 2) 0))

(define (exp-iter b n a)
	(cond ((= n 0) a)
    	  ((even? n) (exp-iter (square b) (/ n 2) a))	
     	  (else (exp-iter b (- n 1) (* a b)))))

(define (fast-exp-i b n)
  (exp-iter b n 1))

(fast-exp-i 2 0)
(fast-exp-i 2 1)
(fast-exp-i 2 2)
(fast-exp-i 2 3)
(fast-exp-i 2 4)
(fast-exp-i 2 5)


