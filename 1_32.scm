
;iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))


(define (filtered-accumulate combiner null-value term a next b filter)
  (define (filter-combine result a)
    (if (filter a)
        (combiner result a)
        result))
  
  (define (iter a result)
    (if (> a b)
        result
        (iter (filtered (next a)) (filter-combine result (term a)))))
  (iter a null-value))


(define (identity x) x)

(define (inc x) (+ x 1))


(define (sum term a next b)
	(accumulate + 0 term a next b))

(define (product term a next b)
	(accumulate * 1 term a next b))


(define (factorial n)
  (define (next x) (+ x 1))

  (product identity 1 next n))

(factorial 5)
(factorial 3)


