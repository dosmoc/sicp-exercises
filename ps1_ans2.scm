(define (fact x)
	(if (= 0 x)
		1
		(* x (fact (- x 1)))))

(define (comb n k)
  (/ (fact n) 
     (* (fact k)
        (fact (- n k)))))