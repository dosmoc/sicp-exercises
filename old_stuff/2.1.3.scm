(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
       (if (good-enough? guess)
           guess
           (iter (improve guess))))
    
    (iter first-guess)))


;cons as integer

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (num-divs n d)
  (define (iter x result)
    (if (= 0 (remainder x d))
        (iter (/ x d) (+ 1 result))
        result))
  (iter n 0))

(define (num-divisions n base)
  (define (iter x count)
    (if (= 0 (remainder current base))
      (iter (/ x base) (inc count))
      count))
  (iter current 0))

(define (num-expt goal base count)
  (let ((e (expt base count)))
   (if (= goal e)
      count
      (num-expt goal base (inc count)))))

(define (car x)
  (let ((base 2))
   (num-expt (/ x (num-divisions base x 0)) base 0)))

(define (cdr x)
  (let ((base 3))
   (num-expt (/ x (num-divisions base x 0)) base 0)))

(cons 0 18)
(cons 1 17)
(cons 2 16)
(cons 3 15)
(cons 4 14)

