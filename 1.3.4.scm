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

(define (cubic a b c) 
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))


(define (zeros-cubic a b c) 
  (newtons-method (cubic a b c) 1))

(define (inc x) (+ x 1))

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
;(double (dobule dobule))
;
;(lambda (x) ((double (double x)) ((double (double x)) x)))

(define (compose f g) (lambda (x) (f (g x))))

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