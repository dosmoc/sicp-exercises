(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 

(define (even? n)
  (= (remainder n 2) 0))


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;(b^n/2)^2 = (b^2)^n/2
; keep exponent n and base b, state variable a, a * b^n is unchanged from state to state

(define (fast-expt-iter b n a)
	(cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* b a)))))

(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(fast-expt-i 2 0)
(fast-expt-i 2 1)
(fast-expt-i 2 2)
(fast-expt-i 2 3)
(fast-expt-i 2 4)
(fast-expt-i 2 5)

