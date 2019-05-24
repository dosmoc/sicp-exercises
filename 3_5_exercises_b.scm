;3.5 Streams

;3.5.1 Streams are Delayed Lists

;Accumulation vs. operations on lists

;We need the prime? predicate:

;we need the prime? predicate
(define (prime? n)
  (define (smallest-divisor)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
    (define (divides? a b)
      (= (remainder b a) 0))
      
  (= n (smallest-divisor)))


;accumulation version doesn't use list abstractions, 
;keeping the sum in the accum variable
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;the makes the whole list of numbers
(enumerate-interval 1 100)
;Value 13: (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100)

;this would enumeriate nearly a million integers:
(car (cdr (filter prime?
                  (enumerate-interval 10000 1000000))))

;the accumulation version mixes enumeration and filtering

; With streams we can achieve the best of both worlds: We can formulate programs elegantly as 
; sequence manipulations, while attaining the efficiency of incremental computation.

; Streams have the same interface as lists, but have different names for procedures
; that manipulate them, since the Scheme we're using is an applicative order language
; (stream-car (cons-stream x y)) = x
; (stream-cdr (cons-stream x y)) = y

; The essence of streams: the cdr of the stream be
; evaluated when accessed by the stream-cdr procedure.
; The rest of a stream is only evaluated when accessed.

; Streams are the same as lists as a data abstraction.
; The underlying difference is when elements are evaluated.
; Delay returns a delayed object.
; Force evaluates the object.

; cons-stream and delay must both be special forms
; because we don't want the cdr to be evaluated in
; cons-stream, and we don't want the expression in delay
; to be evaluated 
; see the problem mentioned later on with using delay.
; Anything dealing with delayed arguments must take a 
; delayed argument or be written as a special form if
; the language is written in an applicative language

;delay without memoization
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

;but we'll just used memoized delay.
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;note on syntax rules: it uses pattern matching.
;The underscore is a convention letting the programmer
;know that the first position always matches the
;key word... so here, _ always matches `delay`
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))


(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;procedures we need for working with streams:
(define (force delayed-object)
  (delayed-object))
(define the-empty-stream '())
(define (stream-null? s) (null? s))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

;stream analogs; We have to define these after
;the redefinition of delay, force, and cons-stream
;don't know why
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


;The stream implementation in action
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

;Exercise 3.50
;Just getting this from my last attempt:
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
;Value: 9

;Exercise 3.51.  In order to take a closer look at delayed evaluation, 
; we will use the following procedure, 
; which simply returns its argument after printing it:

(define (show x)
  (display-line x)
  x)

;What does the interpreter print in response to evaluating each expression in the following sequence?59

(define x (stream-map show (stream-enumerate-interval 0 10)))
;0
;;Value: x

;?? Somehow mapping show against the stream doesn't show everying
;just the first value

(stream-ref x 5)
;1
;2
;3
;4
;5
;;Value: 5
(stream-ref x 7)
;6
;7
;;Value: 7

(stream-ref x 3);
;Value: 3

;This isn't occuring because of memoization like I
;previously thought. It's because of the behavior of 
;cons-stream
;Using the simpler stream map def:
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

; (define-syntax delay
;   (syntax-rules ()
;     ((_ exp) (lambda () exp))))
; 
; (define-syntax cons-stream
;   (syntax-rules ()
;     ((_ a b) (cons a (delay b)))))

; cons-stream is matched and re-written to:

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (proc (stream-car s))
            (delay (stream-map proc (stream-cdr s))))))

; delay is matched and re-written to:
;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons (proc (stream-car s))
;            (lambda () (stream-map proc (stream-cdr s))))))

;filling in the procedure:

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons (show 0) ;this one is evaluated because cons is applicative order 
;            (lambda () (stream-map show (stream-cdr s)))))) ;the thunk isn't evaluated until we call stream-ref:
;
;(define (stream-ref s n)
;  (if (= n 0)
;      (stream-car s)
;      (stream-ref (stream-cdr s) (- n 1))))
;
;(stream-ref x 5)
;;Here stream cdr from the stream-ref at this point is something like:
;(stream-cdr (lambda () (stream-map show (enumerate-interval 1 10))))
;;which is
;(force (lambda () (stream-map show (stream-cdr s))))
;;which is
;((lambda () (stream-map show (enumerate-interval 1 10))))
;;which is 
;(stream-map show (enumerate-interval 1 10))
;which finally shows 1 until we get to 5
;This isn't a completely accurate re-writing / evaluation of stuff, but it's 
;mainly showing show cons-stream delays the evaluation of all expressions... 
;even those with a stream-cdr

;Incorporate the above answer into 3_5_exercises.scm

;Okay, lets go back to the stream-map with an arbitrary number of streams
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;Exercise 3.52.  Consider the sequence of expressions

;See the old version.


;3.5.2  Infinite Streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))


(stream-ref no-sevens 100)
;Value: 117

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

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


;This definition says that fibs is a stream beginning with 0 and 1, such that the rest of the stream can be generated by adding fibs to itself shifted by one place:
;
;1   1   2   3   5   8   13  21  ... = (stream-cdr fibs)
;0   1   1   2   3   5   8   13  ... = fibs
;0   1   1   2   3   5   8   13  21  34  ... = fibs

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(stream-ref double 0)
;1
(stream-ref double 1)
;2
(stream-ref double 2)
;4
(stream-ref double 3)
;8

(define (square n) (* n n))

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

;Exercise 3.53:

(define s (cons-stream 1 (add-streams s s)))

;Let's re-write this...
;s is defined as (add-streams (cons-stream 1 s) (cons-stream 1 s))... which means s first element
;is 2. Then you (add-streams (cons-stream 2 s) (cons-stream 2 s))... which means the second
;element of s is 4.... it's powers of two

;The zeroeth element turns out to be 1, which makes sense since 2⁰ = 1

;It's like two undulating branches:
;
;  /--1/--2/--4 ; s, shifted
; 1   2   4   8 ; the stream, s
;  \--1\--2\--4 ; s, shifted
;

;Exercise 3.54

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;let's test it out
;

(define squares (mul-streams integers integers))

(stream-ref squares 0)
;1
(stream-ref squares 1)
;4
(stream-ref squares 2)
;9
(stream-ref squares 3)
;16
(stream-ref squares 4)
;25

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

; need to get someting like this
; ----2---3---4   ; integers starting from one
; 1   2   6   24  ; factorials
; ----1\--2\--6   ; factorials shifted
;
(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)

;stream-map is a method of combining streams
;the shifting of the factorials is achieved
;by it being the part consed onto
;whereas the integers are not shifted by
;using the stream-cdr

;Exercise 3.55

;
; 1---2---3---4   ;integers
; 1   3   6   10  ;partial sums
;  \--1\--3\--6   ;partial sums shifted 

(define (partial-sums the-stream) 
  (cons-stream (stream-car the-stream)
    (add-streams (stream-cdr the-stream) (partial-sums the-stream))))

(define partial-sum-of-integers (partial-sums (integers-starting-from 1)))

(stream-ref partial-sum-of-integers 0)
(stream-ref partial-sum-of-integers 1)
(stream-ref partial-sum-of-integers 2)
(stream-ref partial-sum-of-integers 3)
(stream-ref partial-sum-of-integers 4)
(stream-ref partial-sum-of-integers 5)

; Thinking about how regular lists are mixed with streams in Clojure be
; makes it possible to define procedures that realize an entire 
; sequence in memory instead of streaming it when this is not the
; intention
; I wonder if Clojure provides compile time warnings for such 
; mixing, or if it could only warn at runtime.
; Are there any systems that provide runtime warnings for dynamic
; languages?

;Exercise 3.56
; Enumerate, in ascending order, all positive integers with no
; prime factors other than 2, 3, or 5
;
; facts:
; - s begins  with 1
; - the elements of (scale-stream s 2) are also elements of s
; - the same is true for (scale-stream s 3) and (scale-steam 5 s)

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
                                (merge (scale-stream S 3)
                                       (scale-stream S 2)))))

(stream-ref S 0)
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 3)
(stream-ref S 4)
(stream-ref S 5)
(stream-ref S 6)

;Exercise 3.57

; Probably the growth with respect to n
; is linear

;Exericise 3.58

;See old answer

;Exercise 3.59

; What is a power series? 
; I don't know what a power series is, but from the text
; it looks like its an series of terms that defines a function
; So its an infinite series of terms that can be used to
; represent a function

; a. The integral of the series a₀ + a₁x + a₂x² + a₃x³ ... is 
; the series c + a₀x + 1/2a₁x² + 1/3a₂x³ + 1/4a₃x⁴ ...

(define (div-streams s1 s2)
  (stream-map / s1 s2))


(define 1-div-n-series (div-streams ones integers))

(stream-ref 1-div-n-series 0)
(stream-ref 1-div-n-series 1)
(stream-ref 1-div-n-series 2)
(stream-ref 1-div-n-series 3)

(define (integrate-series the-series)
  (mul-streams 1-div-n-series the-series))

;b. The function x  ex is its own derivative. This implies that ex 
;   and the integral of ex are the same series, except for the constant term, 
; which is e0 = 1. Accordingly, we can generate the series for ex as

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(stream-ref exp-series 0)
(stream-ref exp-series 1)
(stream-ref exp-series 2)
(stream-ref exp-series 3)
(stream-ref exp-series 4)

;rest of this is from old answer
(define cosine-series
  (cons-stream 1 (negative-stream (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;or use scale like here: http://community.schemewiki.org/?sicp-ex-3.59
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

;Exercise 3.60
;I couldn't figure this one out before looking at http://community.schemewiki.org/?sicp-ex-3.60,
;but this answer:

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)   ;cons the first multiplication onto the output stream
                  (stream-car s2))
               ;the rest of mul-series is the sums of the rest of s1 s2
               ;and this series
               (add-streams (mul-streams (stream-cdr s1)  
                                         (stream-cdr s2))
                            (mul-series s1 s2))))

;Exercise 3.61.

; series S
; constantterm is 1
; we want 1/S
; the series X such that S * X = 1 (multiplying a ratio by its inverse gives 1)
; rewriting to S = 1 + Sᵣ where S_R is the part of S after the constant term

;thus 
; S * X = 1
; (1 + S_R) * X = 1
; X + S_R * X = 1
; X = 1 - S_R * X

(define invert-unit-series s
  (cons-stream 1 (add-streams (negative-stream))))

;Exercise 3.62
;Latersss