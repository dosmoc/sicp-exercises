;1.1.1  Expressions

486

(+ 137 349)

(- 1000 334)

(* 5 99)

(/ 10 5)

(+ 2.7 10)

(+ 21 35 12 7)

(+ 21 35 12 7)

(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;1.1.2  Naming and the Environment

(define size 2)

size

(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

(define circumferences (* 2 pi radius))
circumferences

;1.1.3  Evaluating Combinations
;nothing to evaluate

;1.1.4  Compound Procedures

(define (square x) (* x x))

(square 21)

(square (+ 2 5))

(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 3)))

(f 5)

;1.1.5  The Substitution Model for Procedure Application

;lets evaluate (f 5)

(f 5)

;retrieve the body of the procedure
;(sum-of-squares (+ a 1) (* a 2))

;becomes:
(sum-of-squares (+ 5 1) (* 5 2))

;evaluate the operator, evaluate the operands
;using knowledge from later, that could be read as:

((lambda (x y) (+ (square x) (square y))) 6 10)

;applied becomes
(+ (square 6) (square 10))

;becomes
(+ (* 6 6) (* 10 10))

;becomes:
(+ 36 100)

;finally 
136

;Applicative order versus normal order

(f 5)

;would proceed according to the sequence of expansions

(sum-of-squares (+ 5 1) (* 5 2))

(+ (square (+ 5 1)) (square (* 5 2))  )

(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))

;followed by the reductions

(+ (* 6 6) (* 10 10))

(+ 36 100)

136

;1.1.6  Conditional Expressions and Predicates

;case analysis, typed manually to get used to the form
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0) 
        ((< x 0) (- x))))

 ;I initially was bothered by the two semicolons at the start
 ;but I get their use now
 ;Clojure doesn't do this -- undecided which is visually better
 
 ;what does the MIT interpreter say when a case falls through?
 (define (abs2 x)
  (cond ((> x 0) x)
        ((< x 0) (- x))))
 
 (abs2 0)
 
 ;Unspecified return value

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;this bit from the text:

;To evaluate an if expression, 
;the interpreter starts by evaluating the <predicate> 
;part of the expression. If the <predicate> evaluates 
;to a true value, the interpreter then evaluates 
;the <consequent> and returns its value.
;Otherwise it evaluates the <alternative> and returns its value.

;Implies that the if special form does not follow the 
;applicative order model of evaluation

;same for the logical composition operators, and / or
;not can be an ordinary procedure because it is not
;a connective operation -- it only has one formal parameter

;btw: formal parameter are the names to refer to arguments
; --arguments are the actual values "sent" to a procedure

(define (and-example x)
 (and (> x 5) (< x 10)))

(and-example 5)
#f

(and-example 6)
#t

;I've skipped over this one before:
(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))

;Exercise 1.1

10
;Value: 10
(+ 5 3 4)
;Value: 12
(- 9 1)
;Value: 8
(/ 6 2)
;Value: 3
(+ (* 2 4) (- 4 6))
;Value: 6
(define a 3)
;Value: a
(define b (+ a 1))
;Value: b
(+ a b (* a b))
;Value: 19
(= a b)
;Value: #f
(if (and (> b a) (< b (* a b)))
    b
    a)
;Value: 3
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;Value: 16

;Note: I evaluated "(else 25))" alone by mistake in SublimeREPL
;and it said:
;Unbound variable: else
;which says to me that that else is not a keyword
;and cond is probably a macro 

(+ 2 (if (> b a) b a))
;Value: 16

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;Value: 16

;Exercise 1.2
;translate 5 + 4 + (2 - (3 - (6 + (4 / 5))) / 3(6 - 2)(2 - 7)
;into preficx form

(/ (+ 5
	  4
	  (- 2
	     (- 3
	        (+ 6
	           (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;Exercise 1.3
(define (squared-largest x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> y x) (> z x)) (sum-of-squares y z))
        (else (sum-of-squares x z))))

;testing
(squared-largest 1 2 3)
;Value: 13

(squared-largest 3 2 1)
;Value: 13

(squared-largest 1 3 2)
;Value: 13

;but what about two arguments that are the same?
(squared-largest 2 2 3)
;Value: 13

(squared-largest 3 2 2)
;Value: 13


(squared-largest 2 3 2)
;Value: 8
;oops!

;this seems to do it
(define (squared-largest x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= y x) (>= z x)) (sum-of-squares y z))
        (else (sum-of-squares x z))))

;Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;if b is greater than zero, add a and b
;else subtract b from a

;Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;(test 0 (p))

;This one always gets me
;Applicative order asks you to evaluate the operands
;and arguments before applying the operands to the argument
;thus:

;(test 0 (p))

;means that you will always be evaluating (p) because you have
;to evaluate the arguments before applying the body of test

;for normal order, the arguments are not evaluated until needed
;...the test (= x 0) evaluates to #t with an argument of 0
;so the procedure returns 0. Th

;1.1.7 Example: Square Roots by Newton's Method

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;testing
(sqrt 9)

(sqrt (+ 100 37))

(sqrt (+ (sqrt 2) (sqrt 3)))

(square (sqrt 1000))

;Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;Since new-if is an ordinary procedure, its arguments are always
;evaluated. The procedure reaches maximum recursive depth
;because the sqrt-iter is always evaluated

;Exercise 1.7

;Lets try very small and very large numbers

(sqrt .0000000000000000004)
;Value: .03125
;according to google: 6.3245553e-10
;This is not anywhere near correct. The tolerance specified 
;earlier is too small so the procedure thinks it's good-enough?
;too early

(sqrt 40000000000000000000)
;Value: 6324555320.336759
;according to google: 6324555320.34
;not sure how to explain this one... it appears the good-enough?
;also exits to early in this case

;Lets try this again:

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;from sicp-answers
(define (good-enough? guess x) 
	(< (abs (- (improve guess x) guess)) 
	   (* guess .001))) ;duh, I sux at math

(define (sqrt x)
  (sqrt-iter 1.0 x))
 
;testing
(sqrt .0000000000000000004)

(sqrt 40000000000000000000)

;Exercise 1.8

;(define (cubrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (cubrt-iter (improve guess x)
;                 x)))
;
;(define (improve guess x)
;  (/ (+ (/ x (square guess)) 
;        (* 2 guess))
;     3))
;
;;from sicp-answers
;(define (good-enough? guess x) 
;	(< (abs (- (improve guess x) guess)) 
;	   (* guess .001))) ;duh, I sux at math
;
;(define (cubrt x)
;  (cubrt-iter 1.0 x))
;
;let's use block structure from the next
;section so we can have sqrt later:

(define (cubrt x)
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess))
       (* guess .001)))
  
  (define (cubrt-iter guess)
    (if (good-enough? guess)
        guess
        (cubrt-iter (improve guess))))
  ;start the thing off
  (cubrt-iter 1.0))

;1.1.8  Procedures as Black-Box Abstractions


