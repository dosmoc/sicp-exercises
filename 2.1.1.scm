(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

;(define (make-rat n d) (cons n d))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (norm-rat n d)
  (define (normalize n g)
    (cons (/ (- n) g) (/ (- d) g)))
  
  (let ((g (gcd n d)))
   (cond ((and (negative? n) (positive? d)) (normalize n g))
         ((and (negative? n) (negative? d)) (normalize n g))
         (else (cons (/ n g) (/ d g))))))

(define (recip-rat x)
  (make-rat (denom x) (numer x)))

(define (make-rat n d) (norm-rat n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)

(print-rat (recip-rat one-half))


(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(define (make-point x y) (cons x y))

(define (make-segment start end)
  (cons start end))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (make-point 1 2))

(define (make-slope ri ru))

(define (slope-segment s)
  (define (point->rat p)
    (make-rat (x-point p) (y-point p)))
  
  (let ((start-rat (point->rat (start-segment s)))
        (end-rat (point->rat (end-segment s))))
    (sub-rat start-rat end-rat)))

(define (invert-slope s)
  (let ((recip (recip-rat s)))
    (make-rat (numer recip) (- (denom recip)))))

(define (perpendicular? s1 s2)
  (let ((slope1 (slope-segment s1))
        (slope2 (slope-segment s2)))
    (= (invert-slope slope1) slope2)))