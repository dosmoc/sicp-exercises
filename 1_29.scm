(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

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


;exercise 1_30
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