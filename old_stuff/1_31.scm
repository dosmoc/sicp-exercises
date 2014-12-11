(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))



;part a
(define (factorial n)
    
  (define (next x) (+ x 1))
  
  (product identity 1 next n))

(factorial 5)

(define (pi-approximations n)
  (define (next x) (+ x 1))
  
  (define (offset n) (+ n 2))

  (define (numer n)
  	(if (or (= 0 n) (even? n))
  		(offset n)
  		(inc (offset n))))

  (define (denom n)
  	(if (or (= 0 n) (even? n))
  		(inc (offset n))
  		(offset n)))

  (define (term x)
  	(/ (numer x) (denom x)))
  
  (* 4.0 (product term 0 next n)))

(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

;part b
(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))


(pi-approximations 10000)

(product identity 2 inc 5)

(product-recur identity 2 inc 5)

