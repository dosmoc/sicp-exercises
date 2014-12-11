(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (m-iter a b)
  (define (mul-iter a b)
    (cond  ((= b 0) 0) ;Mixing iterative with recursive calls introduces additional conditions
      	   ((= b 1) a)
           ((even? b) (mul-iter (double a) (halve b)))
           (else (+ a (mul-iter a (- b 1))))))
  (mul-iter a b))

;the sicp answers solution is fully recursive... the double is deferred
(define (* a b) 
    (cond ((= b 0) 0) 
          ((even? b) (double (* a (halve b)))) 
          (else (+ a (* a (- b 1)))))) 

(define (add x y) 
  (+ x y))

(m-iter 0 1)
(m-iter 0 2)
(m-iter 0 3)
(m-iter 3 0)
(m-iter 3 1)
(m-iter 3 2)
(m-iter 3 3)
(m-iter 3 4)
(m-iter 3 5)
(m-iter 3 6)
(m-iter 3 7)
(m-iter 3 300)
