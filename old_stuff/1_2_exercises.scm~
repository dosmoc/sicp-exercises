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