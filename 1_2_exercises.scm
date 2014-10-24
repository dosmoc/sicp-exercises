;dec and inc
(define (dec x)
  (- x 1))

(define (inc x)
  (+ x 1))

;1.2.1  Linear Recursion and Iteration

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;Exercise 1.9

;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
;
;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))
;
;(+ 4 5)
;
;first version is a recursive process:
;
; (+ 4 5)
; (inc (+ 3 5)))
; (inc (inc (+ 2 5))))
; (inc (inc (inc (+ 1 5))))))
; (inc (inc (inc (inc (+ 0 5)))))))
; (inc (inc (inc (inc 5))))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;
;second version is iterative:
;
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 0

;Exercise 1.10
;Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)

(define (f n) (A 0 n))

;(define (A 0 n)
;  (cond ((= n 0) 0)
;        ((= 0 0) (* 2 n))
;        ((= n 1) 2)
;        (else (A (- 0 1)
;                 (A 0 (- n 1))))))
;computes 2n because (= x 0)

(define (g n) (A 1 n))

;(define (A 1 n)
;  (cond ((= n 0) 0)
;        ((= 1 0) (* 2 n))
;        ((= n 1) 2)
;        (else (A (- 1 1)
;                 (A 1 (- n 1))))))
;
; Any input > 1 evaluates to the else clause:
;
;(A (- 1 1) (A 1 (- n 1)))))
;which reduces to:
;(A 0 (A 1 (- n 1)))
;which reduces to:
;(* 2 (A 1 (- n 1)))
;so until you get n = 1 you have something like:
;(* 2 (* 2 (* 2 (A 1 (- n 1)))))
;Once (= n 1), the function returns 2 so for, say (g 3), you get:
;(* 2 (* 2 2))
;or (g 4):
;(* 2 (* 2 (* 2 2)))
;that looks like 2^n

(define (h n) (A 2 n))

;(cond ((= n 0) 0)
;      ((= 2 0) (* 2 n))
;      ((= n 1) 2)
;      (else (A (- 2 1)
;               (A 2 (- n 1))))
;
; let's evaluate (h 3):
;
; (A 2 3) (A x y)
;
; retrieve the body and substitute:
;(define (A x y)
;  (cond ((= y 0) 0)
;        ((= x 0) (* 2 y))
;        ((= y 1) 2)
;        (else (A (- x 1)
;                 (A x (- y 1))))))
; substitute:
; (cond ((= 3 0) 0)
;       ((= 2 0) (* 2 3))
;       ((= 3 1) 2)
;       (else (A 1 (A 2 2))))
;
; substitution will take forever, but it seems 
; I'm getting better at it
; (A 2 3)
; (A 1 (A 2 2))
; (A 1 (A 1 (A 2 1)))
; (A 1 (A 1 2))
; (A 1 (A 0 (A 1 1))
; (A 1 (A 0 2))
; (A 1 4)
; (A 0 (A 1 3))
; (A 0 (A 0 (A 1 2))
; (A 0 (A 0 (A 0 (A 1 1))))
; (A 0 (A 0 (A 0 2))))
; (A 0 (A 0 (* 2 2))
; (A 0 (A 0

(define (k n) (* 5 n n))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;Example: Counting change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;Exercise 1.11

;recursive is easy
(define (f n)
  (if (< n 3) 
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 3) ;4
(f 4) ;11
(f 5) ;25
(f 6) ;59
(f 7) ;142

;Ugh, iterative
; in fib, the transformation is:
; a <- a + b
; b <- a
;
; so maybe:
; a <- a + 2b + 3c
; b <- a
; c <- b
(define (f-iter n)
  (define (iter a b c count)
    (if (= count 0) 
      c
     (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n)) ;since fib start 1 0, do 2 1 0

(f-iter 3)
(f-iter 4)
(f-iter 5)
(f-iter 6)
(f-iter 7)

  
;Exercise 1.12

;Pascal's triangle

;1 1 1 1 1
;1 2 3 4
;1 3 6
;1 4
;1

;coordinates:
; (0,0) (0,1) (0,2) (0,3) (0,4)

; (1,0) (1,1) (1,2) (1,3) (1,4)

; (2,0) (2,1) (2,2) (2,3) (2,4)

; (3,0) (3,1) (3,2) (3,3) (3,4)

; (4,0) (4,1) (4,2) (4,3) (4,4)

;To calculate at 6 at (2,2) 
;we must calculate    (2,1)  (1,2)
;and thus             (2,0), (1,1), (0,2)
;If x or y is zero, then the value is 1
;Otherwise add the previous two Pascal numbers


 
(define (pascal x y)
  (if (or (= x 0) (= y 0))
      1
      (+ (pascal (dec x) y) (pascal x (dec y)))))

;Exercise 1.13

;TODO: learn proof by induction, thanks Bastrop school system

;Exercise 1.14

;TODO: make a drawing

;Exercise 1.15

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;(sine 12.5)
;
;(if (not (> (abs 12.5) 0.1))
;      12.5
;      (p (sine (/ 12.5 3.0))))
;
;(p (sine (/ 12.5 3.0)))
;
;(p (sine 4.166666666666667))
;(p (p (sine 1.388888888888889)))
;(p (p (p (sine .462962962962963))))
;(p (p (p (p (sine .154320987654321)))))
;(p (p (p (p (p (sine .051440329218107005)))))
;
; 5 times
; 
; looks logarithmic because the angle is divided by three
; each time the procedure is applied; 
; I can't express it more formally

;1.2.4 Exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;Exercise 1.16
;(b^n/2)^2 = (b^2)^n/2
; keep exponent n and base b, state variable a, a * b^n is unchanged from state to state

(define (fast-expt-iter b n a)
	(cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* b a)))))

(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(fast-expt-i 2 0)
(fast-expt-i 2 1)
(fast-expt-i 2 2)
(fast-expt-i 2 3)
(fast-expt-i 2 4)
(fast-expt-i 2 5)

;Exercise 1.17 (from old work)
(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (m-iter a b)
  (define (mul-iter a b)
    (cond  ((= b 0) 0) ;Mixing iterative with recursive calls introduces additional conditions
      	   ((= b 1) a)
           ((even? b) (mul-iter (double a) (halve b)))
           (else (+ a (mul-iter a (- b 1))))))
  (mul-iter a b))

;the sicp answers solution is fully recursive... the double is deferred
(define (* a b) 
    (cond ((= b 0) 0) 
          ((even? b) (double (* a (halve b)))) 
          (else (+ a (* a (- b 1)))))) 

(define (add x y) 
  (+ x y))

(m-iter 0 1)
(m-iter 0 2)
(m-iter 0 3)
(m-iter 3 0)
(m-iter 3 1)
(m-iter 3 2)
(m-iter 3 3)
(m-iter 3 4)
(m-iter 3 5)
(m-iter 3 6)
(m-iter 3 7)
(m-iter 3 300)

;Exercise 1.18 (from old work)
(define (m-iter a b)
  (define (mul-iter a b product)
    (newline)
    (display b)
    (cond  ((= b 0) product)
           ((even? b) (mul-iter (double a) (halve b) product))
           (else (mul-iter a (- b 1) (+ product a)))))
  (mul-iter a b 0))

(m-iter 0 1)
(m-iter 0 2)
(m-iter 0 3)
(m-iter 2 5)
(m-iter 3 0)
(m-iter 3 1)
(m-iter 3 2)
(m-iter 3 3)
(m-iter 3 4)
(m-iter 3 5)
(m-iter 3 6)
(m-iter 3 7)
(m-iter 3 300)

;Exercise 1.19
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

;Exercise 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;(gcd 206 40)
;Normal order... operands are not evaluated until needed
;
;(if (= 40 0)
; 	206
;	(gcd 40 (remainder 206 60)))	
;
;(gcd 40 (remainder 206 60))
;
;(if (= (remainder 206 60) 0)
;      40
;      (gcd (remainder 206 60) (remainder 40 (remainder 206 60))))
;
;(gcd (remainder 206 60) (remainder 40 (remainder 206 60)))
;
;(if (= (remainder 40 (remainder 206 60)) 0)
;      (remainder 206 60)
;      (gcd (remainder 40 (remainder 206 60)) 
;           (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))))
;
;(if (= (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) 0)
;      (remainder 40 (remainder 206 60))
;      (gcd (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) 
;           (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))))
;
;(if (= (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))) 0)
;      (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))
;      (gcd (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))) 
;           (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))))))
;
;(if (= (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))) 0)
;      (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))
;      (gcd (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) 
;                      (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))) 
;           (remainder (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))) (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))))))
;lots!
;applicative
;
;(remainder 206 40)
;(if (= 40 0)
;      206
;      (gcd 40 (remainder 206 40)))
;
;(gcd 40 6)
;
;(if (= 6 0)
;      40
;      (gcd 6 (remainder 40 6)))
;
;(gcd 6 4)
;
;(if (= 4 0)
;      6
;      (gcd 4 (remainder 6 4)))
;
;(gcd 4 2)
;
;(if (= 2 0)
;      4
;      (gcd 2 (remainder 2 4)))
;
;(if (= 2 0)
;      2
;      (gcd 2 (remainder 2 2)))
;5 times