(define (fact x)
	(if (= 0 x)
		1
		(* x (fact (- x 1)))))

(define (comb n k)
  (/ (fact n) 
     (* (fact k)
        (fact (- n k)))))

;exercise 11
(define foo1
  (lambda (x)
    (* x x)))

;application:
(foo1 (sqrt 3))

;definition
(define foo2
  (lambda (x y)
    (/ x y)))

