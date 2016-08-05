;3.5 Streams

;we need the prime? predicate
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


;3.5.1  Streams Are Delayed Lists

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

;enumerate-interval constructs the entire interval
;at once before filtering
(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;Implementing delay and force

;By this point, we've seen syntactic sugar for defining functions:
;(define (some-name x y z) (..))
;for local variables:
;(let ((x 10) (y 11)) (+ x y))
;cons-stream: (cons-stream <a> <b>) becomes (cons <a> (delay <b>))
;and now delay, (delay <exp>) is sugar for (lambda () <exp>)
;
;We haven't seen how syntax is implemented yet. It seems delay
;is part of the Scheme standard (at 
;least in R5RS), but cons-stream is not. Here's
;an implementation, using memo-proc discussed in the text:
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;from: http://stackoverflow.com/questions/14640833/how-is-the-sicp-cons-stream-implemented
;using delay as in the text
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

;no memoization:
; (define-syntax delay
;   (syntax-rules ()
;     ((_ exp) (lambda () exp))))
; 
; (define-syntax cons-stream
;   (syntax-rules ()
;     ((cons-stream a b)
;      (cons a (delay b)))))

;the things we need for working with streams:
(define (force delayed-object)
  (delayed-object))
(define the-empty-stream '())
(define (stream-null? s) (null? s))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;by note 54
;these are already defined in mit-scheme
;(define the-empty-stream '())
;(define (stream-null? s) (null? s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;now we can try
(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))
;10009 
;that's cool

;MIT-Scheme names promises explicitly, printing as #[promise 16]
;where the use of thunks in
;SICP just prints as #[compound-procedure 27]
;It seems that this extra type info allows MIT scheme to use
;data directed dispatch on the type streams vs. ordinary lists

;Just a note: apparently Scheme uses hygenic macros via  
;define-syntax whereas Common Lisp uses def-macro (which
;aren't hygenic by design).
;
;Clojure has defmacro, with macros for gensym based hygiene. I've 
;actually used this and it seems straightforward. This is 
;the solution that Common Lisp uses. I do not yet
;know how Scheme's define-syntax works.
;
;PS Kernel is the other way around, as far as I can tell. Most
;Lisps evaluate the arguments, then apply the operand. A 
;special form is needed to introduce syntax... this special
;form (def-macro, define-syntax, defmacro, etc.) is a function;
;that's run at compile time that transforms its input
;into code without first evaluating the input.
;
;Kernel, on the other hand, makes this distinction (from http://fexpr.blogspot.com/2011/04/fexpr.html):
;    A list to be evaluated is a combination; its first element is the
;    operator, and the rest of its elements are operands. 
;    The action designated by the operator is a combiner.  
;    A combiner that acts directly on its operands is an operative.  
;    (Legacy terms: an operative that is a data value is a fexpr, 
;    an operative that is not a data value is a special form.) 
;    A combiner that isn't operative is applicative; 
;    in that case, the operands are all evaluated,
;    the results of these evaluations are called arguments, and the
;    action is performed on the arguments instead of on the operands. 
;
;Kernel seems to allow the programmer to choose between applicative
;order evaluation and normal order evaluation explicitely via the
;use of the vau-calculus...blerg, I'm starting to get ahead
;of my understanding, but now it's not such a complete mess

;Exercise 3.50. 
;We will use the map from section 2.2.3, not the footnote: the footnote
;describes the behavior of what we're supposed to implement, not the 
;procedure we're supposed to use. We also use map as defined
;in 2.2.3:
(define nil '())
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(stream-car 
  (stream-cdr 
    (stream-cdr 
      (stream-map + 
                  (stream-enumerate-interval 1 3) 
                  (stream-enumerate-interval 4 6)))))
;9

;Exercise 3.51

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
;1
;2
;3
;4
;5
;Value: 5

(stream-ref x 7)
;6
;7
;Value: 7

;Interesting, is this because of memoization?
(stream-ref x 3)
;Value: 3

;yes! If we evaluate all these definitions again,
;with the non-memo version the output is more like:

(stream-ref x 5)
;1
;2
;3
;4
;5
;Value: 5

(stream-ref x 7)
;1
;2
;3
;4
;5
;6
;7
;Value: 7

(stream-ref x 3)
;1
;2
;3
;Value: 3

; note that you have to re-evaluate
; all the procedures that depend on cons-stream
; you can't simply evaluate the define-syntax
; expression again. Is this because of expansion
; vs. runtime?


;Exercise 3.52
(define (even? n)
  (= (remainder n 2) 0))

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
;sum is 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;sum is 1 because we've applied accum once in cons-stream
(define y (stream-filter even? seq))
;sum is 6, 
;y is (6 . #[compound-procedure 33])
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;sum is 10 with memoization, 15 without
;z is (10 . #[compound-procedure 35])
(stream-ref y 7)
;evaluates to 136 with memoization, 162 without
;sum is 136 with memoization, 162 without
;sum seems to be whatever the car of the most recently filtered stream
;when using memoization

(stream-ref seq 0)
; 1
(stream-ref seq 1)
; 3
(stream-ref seq 2);
; 6
 
(display-stream z)
;10
;15
;45
;55
;105
;120
;190
;210
;Value: done

;sum is 210

;without memoization:
;15
;180
;230
;305

;sum is 362


; right now I'm inclined to just say, don't mix delayed evaluation with state manipulation
; without a detailed explanation of the differences in the memoized vs not memoized evaluations
; beyond the general notion that the memoized version doesn't recalculate the values, so 
; each (accum 1) ... (accum n) just returns the result previously calculated
; the (set! sum (+ x sum)) is run once each time accum is evaluated for a particular number

;3.5.2  Infinite Streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(stream-ref integers 10)

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))


(stream-ref no-sevens 100)
;Value: 117

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)
;223

(stream-ref primes 19)

;Is this really the Sieve of Eratosthenes:
;O'Neill, Melissa E., "The Genuine Sieve of Eratosthenes", Journal of Functional Programming, Published online by Cambridge University Press 9 October 2008 doi:10.1017/S0956796808007004, pp. 10, 11 (contains two incremental sieves in Haskell: a priority-queue–based one by O'Neill and a list–based, by Richard Bird).

(stream-car (integers-starting-from 2))

;Defining streams implicitly

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))


(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;Exercise 3.53.

(define s (cons-stream 1 (add-streams s s)))

;first element
(stream-car s)
;1

;next would be
;(stream-car (stream-cdr (add-streams (cons-stream 1 ...) (cons-stream 1 ...))))
;(stream-car (stream-cdr (2 . #apromise)))
;2

;(stream-car (stream-cdr (stream-cdr (add-streams (2 . #apromise) (2 . #apromise)))))
;(stream-car (stream-cdr (stream-cdr (4 . #apromise))))
;stream of powers of 2
;this is kind of making me nuts

;Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

;it is pretty easy to define directly the partial sum of integers
(define partial-sums (cons-stream 0 (add-streams integers partial-sums)))

(define (partial-sums the-stream) 
  (cons-stream 0
    (add-streams the-stream (partial-sums the-stream))))

;Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 5)
                                (merge (scale-stream S 2)  (scale-stream S 3)))))

;Exercise 3.57
;Show this

;Exercise 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;these give you the decimal representation of the
;ratios 1/7 and 3/8
;as a stream of digits
(define x (expand 1 7 10))
(stream-ref x 0) ;1
(stream-ref x 1) ;4
(stream-ref x 2) ;2
(stream-ref x 3) ;8
(stream-ref x 4) ;5
(stream-ref x 5) ;7
(stream-ref x 6) ;1
(stream-ref x 7) ;4
(stream-ref x 8) ;2
(stream-ref x 9) ;8
(stream-ref x 10) ;5
(stream-ref x 11) ;7
(stream-ref x 12) ;1
;142857 should repeat forever

(define y (expand 3 8 10))
(stream-ref y 0) ;3
(stream-ref y 1) ;7
(stream-ref y 2) ;5
(stream-ref y 3) ;0
(stream-ref y 4) ;0
(stream-ref y 5) ;0
(stream-ref y 6) ;0
(stream-ref y 7) ;0
(stream-ref y 8) ;0
(stream-ref y 9) ;0
(stream-ref y 10) ;0
(stream-ref y 11) ;0

;Exercise 3.59

(define (integrate-series input-stream)
  ())