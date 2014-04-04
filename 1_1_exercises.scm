(/ (+ 5  4 (- 2 (- 3 (+ (/ 4 5))))) (* 3 (- 6 2) (- 2 7) ) )

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (sqrt-iter (improve guess x)
		 guess
		 x)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess old-guess x)
  (< (abs (- (square guess) x)) 
     (if (< .0001 (/ old-guess guess)
	    (/ old-guess guess)
)))

;we want good enough's comparison to be a small fraction
;of the change between guesses?


(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

(sqrt (+ 100 37))

(sqrt .001)

