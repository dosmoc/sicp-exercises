(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)   ;cons the first multiplication onto the output stream
                  (stream-car s2))
               ;the rest of mul-series is the sums of the rest of s1 s2
               ;and this series
               (add-streams (mul-streams (stream-cdr s1)  
                                         (stream-cdr s2))
                            (mul-series s1 s2))))

; I'm still not understanding this recursive definition
; the series:  
; (a0 + a1 x + a2 x2 + a3 x3 )(b0 + b1 x + b2 x2 + b3 x3 )
; is (a0 * b0) + (the rest of the a series * the rest of the b series)
;
; using the thinking behind the section "Defining streams implicitly"
;
; the first element of mul-series is a0 * b0
; the multiplication of the rest of a and b series added to mul-series


; Still not understanding this... let's try it this way
; We have two series:
; (a0 + a1 + a2 + a3 ...)
; (b0 + b1 + b2 + b3 ...)
; The definition of multiplying a series is such that we want to get:
;
; (a0 * b0 + a1 * b1 + a2 * b2 + a3 * b3 ... an * bn)

; what does stream-add get us:

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)

; the first element of mul-series is

; (* a0 b0)

; add-streams is then fed
; (mul-series (stream-cdr s1) (stream-cdr s2))
; and (mul-series s1 s2)
;
; which is

;(add-streams ...the series starting with (* a1 b1)
;             ...this series, which starts with (* a0 b0))

;or rearranged a little:

;(add-streams ...this series, which starts with (* a0 b0)
;             ...the series starting with (* a1 b1))


;thus, the second element of the series is:
; (stream-map + (* a1 b1) (* a0 b0))

; mul-series        (* a0 b0)     --(+ (* a0 b0) (* a1 b1))------- (+ (+ (* a0 b0) (* a1 b1)) (* a2 b2)) ...
;                   V            /  V                          /
;   add-streams     (* a0 b0) --/   (+ (* a0 b0) (* a1 b1))---/
;                              /                             /
;     mul-streams   (* a1 b1)-/     (* a2 b2)---------------/
;        s1         a1              a2  
;        s2         b1              b2

;we're building a series that's something like:

; ((* a0 b0) (+ (* a0 b0) (* a1 b1)) (+ (* a0 b0) (* a1 b1) (* a2 b2)) ...)
; though the structure looks like:
; 
; (* a0 b0)
; (+ (* a0 b0) (* a1 b1))
; (+ (+ (* a0 b0) (* a1 b1)) (* a2 b2))
; (+ (+ (+ (* a0 b0) (* a1 b1)) (* a2 b2)) (* a3 b3))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define a (integers-starting-from 1))
\
(define b (integers-starting-from 2))

(define x (mul-series a b))

(stream-ref x 0)
; (* 1 2) => 2
(stream-ref x 1)
; (+ (* 1 2) (* 2 3)) => (+ 2 6) => 8
(stream-ref x 2)
; (+ (+ (* 1 2) (* 2 3)) (* 3 4)) => (+ (+ 2 6) 12) => (+ 8 12) => 20

; this is probably related to corecursion... 
; structural recursion operates on data that gets progressively smaller
; before hitting a base case
; here we're starting with a base case and the data gets 
; bigger (potentially infinite)
; I'm just thinking about how to understand weaving streams 
; to achieve a goal
; I suppose it just takes practice... like keeping track of 
; state manipulation in imperative code

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))


(define ones (cons-stream 1 ones))

(define zeros (cons-stream 0 zeros))

(define (negative-stream the-stream)
  (stream-map - zeros the-stream))

(define (integrate-series input-stream)
  (mul-streams (div-streams ones integers) input-stream))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (negative-stream (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(list-ref cosine-series 0)
(stream-ref cosine-series 1)
(stream-ref cosine-series 2)
(stream-ref cosine-series 3)
(stream-ref cosine-series 4)
(stream-ref cosine-series 5)
(stream-ref cosine-series 6)




(define test-mul-series
  (add-streams (mul-series cosine-series cosine-series)
               (mul-series sine-series sine-series)))

(stream-ref test-mul-series 0)
(stream-ref test-mul-series 1)
(stream-ref test-mul-series 2)
(stream-ref test-mul-series 3)


;Exercise 3.61
(define zeros (cons-stream 0 zeros))

(define (negative-stream the-stream)
  (stream-map - zeros the-stream))


(define (invert-unit-series s)
  (stream-cons 1 
               (mul-series (negative-stream (stream-cdr s))
                           (invert-unit-series s))))

(define s3 
  (mul-series s1
              (invert-unit-series s2)))

(stream-ref s3 0)
;1
(stream-ref s3 1)
;-3
; which is 1 + (-2 * 2)
(stream-ref s3 2)
;9
(stream-ref s3 3)
;-39

;this is wrong
;we want something like

;if the series is 1 2 3 4

(stream-ref x 0)
; (* a0 b0) => 1
(stream-ref x 1)
; (- (* a0 b0) (* a1 b0)) => (- 1 

(stream-ref x 2)
; (- (- (* a0 b0) (* a1 b1)) (* a2 b2)) => 1
