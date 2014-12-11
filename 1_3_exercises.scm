;1.3  Formulating Abstractions with Higher-Order Procedures

;1.3.1  Procedures as Arguments

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)

(integral cube 0 1 0.001)

;Exercise 1.29
(define (integral-simpson f a b n)
  (define h (/ (- b a ) n))
    
  (define (yk-next x)
    (newline)
    (display (* x 1.0))
    (newline)
    (+ x h)) 
  (* (/ h 3.0)
     (sum f a yk-next b)))

(define (integral-simpson2 f a b n)
  (define h (/ (- b a ) n))
  
  (define (seq a) (/ a h))
  
  (define (y-k x) (+ x h))
  
  (define (yk-next x)
    (cond ((= a (seq x)) (y-k x))
          ((= b (seq x)) (y-k x))
          ((even? (seq x)) (* 2 (y-k x)))
          (else (* 4 (y-k x)))))
   
  (* (/ h 3.0)
     (sum f a yk-next b)))

;the function is not applied
;to each term directly
;but in accordance with yk = f(a + kh)
(define (integral-simpson3 f a b n)
  (define h (/ (- b a ) n))
  
  (define (inc x)
    (+ x 1))
  
  (define (y k)
   (f (+ a (* k h))))
  
  (define (term k)
   (* (cond ((odd? k) 4)
            ((or (= k 0) (= k n)) 1)
            ((even? k) 2))
      (y k)))
   
  (* (/ h 3.0)
     (sum term 0 inc n)))

(integral cube 0 1 .001)

(integral-simpson cube 0 1 1000)

(integral-simpson2 cube 0 1 1000)

(integral-simpson3 cube 0 1 1000)


;Exercise 1.30
(define (sum-itered term a next b)
  (define (iter a result)
    (newline)
    (display a)
    (display result)
    
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(sum-itered identity 2 inc 4)

(sum-itered cube 2 inc 4)

(sum-cubes 2 4)

;Exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


;a.
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

;b.
(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))


(pi-approximations 10000)

(product identity 2 inc 5)

(product-recur identity 2 inc 5)

;Exercise 1.32
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


(define (identity x) x)

(define (inc x) (+ x 1))


(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
         ((filter (term a))
          (combiner (term a)
                    (filtered-accumulate combiner null-value term (next a) next b filter)))
         (else 
          (filtered-accumulate combiner null-value term (next a) next b filter))))
  
(define (odd? x) (not (= (remainder x 2) 0)))
  
(define (inc x) (+ x 1))
(odd? 5)

  
(define (sum-odd a b)
  (filtered-accumulate + 0 identity a inc b odd?))

;testing, should be 25
(sum-odd 1 10)

;a.
(define (sum-squared-primes a b)
 (filtered-accumulate + 0 square a inc b prime?))

;b.
(define (product-relatively-prime n)
  (define (relatively-prime? i n)
    (= (gcd i n) 1))
  
  (filtered-accumulate * 1 square 1 inc (-n1) relatively-prime?))

;1.3.2 Construction Procedures Using Lambda

;Exercise 1.34
(define (f g)
  (g 2))

(f square)
;4

(f (lambda (z) (* z (+ z 1))))
;6
;(f f)
;this would be equivalent to:
;((lambda (f g) (g 2)) (lambda (f g) (g 2))):
;((lambda (f g) (g 2)) 2)
;(2 2)
;2 is not a valid operation
(f f)
;The object 2 is not applicable.

;1.3.3 Procedures as general methods
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;Exercise 1.35
(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 
                                  1))

;Value: 1.6180327868852458

;Exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x**x y) (fixed-point (lambda (x) (/ (log y) (log x))) 
                  2))

(x**x 1000)
(define (average x y) (/ (+ x y) 2))

(define (x**x y) (fixed-point (lambda (x) (/ (log y) (log x))) 
                  2))

(solve-x**x 1000)
;33 iterations

(define (solve-damped-x**x y) 
  (fixed-point (lambda (x) (average x (/ (log y) (log x)))) 
               2))

(solve-damped-x**x 1000)
;8 iterations

;Exercise 1.37


;Recursive
(define (cont-frac n d k)
  (define (recur-cont counter)
    (let ((numer (n counter))
          (denom (d counter)))
      (if (= counter k)
          (/ numer denom)
          (/ numer (+ denom (recur-cont (+ counter 1)))))))
  (recur-cont 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           2)

(/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                 11))
;1.6179...
(/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                 12))
;1.6180...
;K must be at least 12 to get an approximation of the 
;golden ratio accurate to four decimal places

;Iterative
(define (cont-frac-i n d k) 
  (define (iter results count) 
    (let ((numer (n k))
          (denom (d k)))
      (if (= 1 count)
          (/ numer results)
          (iter (+ denom (/ numer results)) (- count 1)))))
  (let ((final-term (/ (n k) (d k))))
    (iter final-term k)))

;Test the iterative version
(cont-frac-i (lambda (i) 1.0)
             (lambda (i) 1.0)
             1)

(cont-frac-i (lambda (i) 1.0)
             (lambda (i) 1.0)
             2)

(/ 1 (cont-frac-i (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  12))

;Exercise 1.38
(define (divides? a b)
  (= (remainder a b) 0))

(define (euler-d i)
  (let ((offset-i (- i 2)))
   (cond ((<= 2 i) i)
         ((divides? offset-i 3) (- i (/ offset-i 3)))
         (else 1.0))))

(cont-frac-i (lambda (i) 1.0)
              euler-d
              12)

(cont-frac (lambda (i) 1.0)
            euler-d
            24)

(define (compute-e k)
  (+ 2 
     (cont-frac-i (lambda (i) 1.0)
                  euler-d
                  k)))

(compute-e 24)
;2.6180339886704433

;Exercise 1.39
(define (tan-cf x k)
  (cont-frac 
    (lambda (i) (if (< 1 i) (- (square x)) x))
    (lambda (i) (+ i (- i 1)))
    k))

;for testing
(define (numer-t x) 
  (lambda (i) (if (< 1 i) (- (square x)) x)))
(define denom-t (lambda (i) (+ i (- i 1))))

((numer-t 5) 1)
;5
((numer-t 5) 2)
;-25
((numer-t 5) 3)
;-25
(denom-t 1)
;1
(denom-t 2)
;3
(denom-t 3)
;5

(tan-cf 5.0 25)
;-3.3805150062465867
;kewl

;1.3.4 Procedures as Returned Values
(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))



(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;Exercise 1.40
(define (cubic a b c) 
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))


(define (zeros-cubic a b c) 
  (newtons-method (cubic a b c) 1))

(define (inc x) (+ x 1))

;Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))


;(((lambda (x) (double (double x))) inc) 5)
;
;((lambda (x) (double (dobule inc))) 5)
;
;(lambda (x) (+ (+ x 1) 1))
;
;((lambda (x) (+ (+ (+ (+ x 1) 1) 1) 1)) 5)
;
;(double (double dobule))
;
;(lambda (x) ((double (double x)) ((double (double x)) x)))

;Exercise 1.42
(define (compose f g) (lambda (x) (f (g x))))

;Exercise 1.43
(define (repeated f n)
  (define (iter repeatedf count) 
    (if (<= count 1)
        repeatedf
        (iter (compose f repeatedf) (- count 1))))
  (iter f n))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n) ((repeated smooth n) f))

(define (n-fold-smooth f n) (repeated (smooth f) n))

;Exercise 1.45
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))


(define (cbrt x)
  (fixed-point-of-transform (lambda (y) (/ x (square y)))
                            average-damp
                            1.0))


(define (4thrt x)
  (fixed-point-of-transform (lambda (y) (/ x (cube y)))
                            (repeated average-damp 2)
                            1.0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (nth-rt x n)
  (let ((power (max (- n 1) 0))
        (num-damps (max (- n 2) 0)))
   (fixed-point-of-transform (lambda (y) (/ x (fast-expt y power)))
                             (repeated average-damp num-damps)
                             1.0)))

;Exercise 1.46
(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
       (if (good-enough? guess)
           guess
           (iter (improve guess))))
    
    (iter first-guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrtit x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  
  (define (improve guess)
    (average guess (/ x guess)))
  
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))

  ((iterative-improve close-enough? f) first-guess))