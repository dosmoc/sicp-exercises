(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* p q) (* q (+ p q)))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


; p = 0, q = 1
;T = a <- a + b and b <- a is a special case where p = 0 and q = 1
;Tpq a <- bq + aq + ap and b <- bp + aq

;starting with (a, b) (1, 0), apply T twice:

;first
; a = 0q + 1q + 1p ; b = 0p + 1q
; a = 0(1) + 1(1) + 1(0) ; b = 0(0) + 1(1)
; a = 1 ; b = 1

;second (a, b) = (1, 1)
;a <- bq + aq + ap and b <- bp + aq
; a = 1q + 1q + 1p ; b = 1p + 1q
; a = 1(1) + 1(1) + 1(0) ; b = 1(0) + 1(1)
; a = 2 ; b = 1

;third
; a = 0q + 1q + 1p ; b = 0p + 1q
; a = 1(1) + 2(1) + 1(0) ; b = 1(0) + 2(1)
; a = 3; b = 2

;forth
; a = 0q + 1q + 1p ; b = 0p + 1q
; a = 2(1) + 3(1) + 3(0) ; b = 2(0) + 3(1)
; a = 5 ; b = 3

;figure this out for (p'q') for second iteration
;Tp'q' a <- bq' + aq' + ap' and b <- bp' + aq'
;for, defining p'q' in terms of pq (0, 1):
;2 = 0q' + 1q' + 1p' ; 1 = 0p' + 1q'
;2 = 0(1) + 1(1) + 1(1) ; 1 = 0(1) + 1(1)
;p' = 1 ; q' = 1
;p' = p + q ; q' = q + q + p

;figure this out for (p'q') for forth iteration
;Tp'q' a <- bq' + aq' + ap' and b <- bp' + aq'
;for, defining p'q' in terms of p'q' (1, 1):
;5 = 0q'' + 1q'' + 1p'' and 3 = 0p'' + 1q''
;p'' = 2 ; q'' = 3, 
;p'' = p' + q' ; q'' = q' + q' + p'

;figure this out for (p'q') for 8th iteration
;Tp'q' a <- bq' + aq' + ap' and b <- bp' + aq'
;for, defining p''''q'''' in terms of p'''q''' (5, 8):
;34 = 0q''' + 1q''' + 1p''' and 21 = 0p''' + 1q'''
;p'''' = 13 ; q'''' = 21, 
;p'''' = q'' ; q'''' = p + q + q


;(0, 1)   
;(1, 1)   (+ 0 q)       (+ 0 q)  ??
;(2, 3)   (+ p q)       (+ p q q) 
;(13, 21) (+ p p q q q) (+ p p p q q q q q) 

;(+ 1)         (+ 1) 
;(+ 1 1)       (+ 1 1 1)
;(+ 2 2 3 3 3) (+ 2 2 2 3 3 3 3 3)
;(+ (* p p) (* q q)) (+ (* p q) (* q (+ p q)))  

;(+ (* p p) (* q q)) (+ (* p q) (* q (+ p q)))  
;(+ (* 2 2) (* 3 3)) (+ (* 2 3) (* 3 (+ 2 3)))
;(+ 4 9) (+ 6 (* 3 5))
;(+ 4 9) (+ 6 15)
;(13) (21)
;
;(+ (* p p) (* q q)) (+ (* p q) (* q (+ p q)))  
;(+ (* 1 1) (* 1 1)) (+ (* 1 1) (* 1 (+ 1 1)))
;(+ 1 1) (+ 1 (* 1 2))
;(2) (+ 1 2)
;(2 3)  
;
;(+ (* p p) (* q q)) (+ (* p q) (* q (+ p q)))  
;
;(+ (* 0 0) (* 1 1)) (+ (* 0 1) (* 1 (+ 0 1)))  
;
;(+ 0 1) (+ 0 1)
;(1 1)