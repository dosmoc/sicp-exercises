;2.1 Introduction to Data Abstraction

;Exercise 2.4
;save the old version
(define orig-cons cons)
(define orig-car car)
(define orig-cdr cdr)


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;(cons 1 2)
; => (lambda (m) (m 1 2))
; (car (lambda (m) (m 1 2)))
; ((lambda (m) (m 1 2)) (lambda (p q) p)))
; ((lambda (p q) p) (1 2))
; 1
; sooo
(define (cdr z)
  (z (lambda (p q) q)))

;Exercise 2.5
(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
       (if (good-enough? guess)
           guess
           (iter (improve guess))))
    
    (iter first-guess)))


(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;Looks like I didn't finish car
(define (cdr b)
	((iterative-improve (lambda (guess) (= b (* (expt 2 (expt (- 1 guess))) 
                                             (expt 3 guess))))
                     	(lambda (guess) (+ 1 guess))) 
  b))

;Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))
;two should be 1 + 1
;retrieve body
;(lambda (f) (lambda (x) (f ((n f) x))))
;substitute
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;(lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;initially though this:
(define (add-church n m)
  (lambda (f) (lambda (x) (n ((m f) x)))))

;but actually this, after banging head online
;seems obvious in retrospect, since we need to
;get the church numeral of n first to be able to
;wrap it around the other one 
;(... (f (f (f (fx)))))
(define (add-church n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

;multiplication?
(define (mul-church n m)
	(lambda (f) (lambda (x) ((m (n f)) x))))



;restore to the originals
(define cons orig-cons)
(define car orig-car)
(define cdr orig-cdr)
;2.1.4 Extended Exercise: Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;Exercise 2.7 

(define (make-interval a b) (cons a b))

;Define selectors upper-bound and lower-bound to complete the implementation

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))


(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;Excercise 2.9
;do this laaaater
(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(width-interval 
  (add-interval (make-interval 1 3) (make-interval 2 5)))

(width-interval 
 (sub-interval (make-interval 1 3) (make-interval 2 5)))

;Exercise 2.10
(define (div-interval x y)
 (define (spans-zero? i)
   (and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))
 
 ;the actual division
 (define (div-i x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
 
 (if (spans-zero? y)
     (error "Interval " y " spans zero -- DIV-INTERVAL")
     (div-i x y)))


;Exercise 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (neg? x) (> 0 x))

(define (pos? x) (< 0 x))


;Exercise 2.11

;(define (mul-interval x y)
;  (let ((s-x-upper (pos? (upper-bound x)))
;        (s-x-lower (pos? (lower-bound x)))
;        (s-y-upper (pos? (upper-bound y)))
;        (s-y-lower (pos? (upper-bound y))))
;    
;   (cond 
;
; 
;     ;all-positive
;     ((and s-x-upper s-x-lower s-y-upper s-y-lower) 
;       (make-interval (* (upper-bound x) (upper-bound y))
;                      (* (lower-bound x) (lower-bound y)))) 
;     ;all-negative
;     ((not (and s-x-upper s-x-lower s-y-upper s-y-lower))
;       (make-interval (* (upper-bound x) (lower-bound y))
;      (else nil))))
;                      (* (lower-bound x) (lower-bound y))))
;     
;     ;xs are positive, ys are negative
;     ((and (and s-x-upper s-x-lower) (not (and s-y-upper s-y-lower)))
;       (make-interval (* (upper-bound x) (lower-bound y))
;                      (* (lower-bound x) (lower-bound y))))
;     
;      ;xs are negative, ys are positive
;     ((and (not (and s-x-upper s-x-lower)) (and s-y-upper s-y-lower))
;       (make-interval (* (upper-bound x) (lower-bound y))
;                      (* (lower-bound x) (lower-bound y))))
;
;      (else nil))))

;So by testing the signs, you only need to multiply
;twice at most intead of four times... 
;Define a bunch of intervals to find out all the conditions
;or maybe reason about it
(define i1 (make-interval -1  2))
(define i2 (make-interval  2  3))
(define i3 (make-interval -3  2))
(define i4 (make-interval  4  3))
(define i5 (make-interval -5  -2))
(define i6 (make-interval -6  3))
(define i7 (make-interval -7 -2))
(define i8 (make-interval 0 3))
(define i9 (make-interval -3 0))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;Exercise 2.12
(define (p->decimal x)
  (/ x 100))

(define (percent-of-x x percentage)
  (* x (p->decimal percentage)))

(define (x-percent-of-y x y)
  (/ y x))

(define (make-center-percent c p)
  (let ((w (percent-of-x c p)))
    (make-center-width c w)))

(define (percent i)
  (let ((c (center i))
        (w (- (upper-bound i) (center i))))
    (x-percent-of-y w c)))

;Exercise 2.13
;Todo figure out what 2.13 is talking about

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;Exercise 2.14

(define i1 (make-interval -1  2))
(define i2 (make-interval  2  3))
(define i3 (make-interval -3  2))
(define i4 (make-interval  4  3))
(define i5 (make-interval -5  -2))
(define i6 (make-interval -6  3))
(define i7 (make-interval -7 -2))
(define i8 (make-interval 0 3))
(define i9 (make-interval -3 0))


(div-interval i2 i2)
(div-interval i2 i4)

(div-interval i4 i4)
(div-interval i4 i7)

(div-interval i7 i7)
(div-interval i7 i5)

(div-interval i5 i5)
(div-interval i5 i7)

