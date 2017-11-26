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

;stream analogs
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

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

;procedures we need for working with streams:
(define (force delayed-object)
  (delayed-object))
(define the-empty-stream '())
(define (stream-null? s) (null? s))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;delay without memoization
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))


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


;This isn't occuring because of memoization like 
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
      (cojs (proc (stream-car s))
            (delay (stream-map proc (stream-cdr s))))))

; delay is matched and re-written to:
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (proc (stream-car s))
            (lambda () (stream-map proc (stream-cdr s))))))

filling in the procedure:

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (show 0) ;this one is evaluated because cons is applicative order 
            (lambda () (stream-map show (stream-cdr s)))))) ;the thunk isn't evaluated until we call stream-ref:

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(stream-ref x 5)
;Here stream cdr from the stream-ref at this point is something like:
(stream-cdr (lambda () (stream-map show (enumerate-interval 1 10))))
;which is
(force (lambda () (stream-map show (stream-cdr s))))
;which is
((lambda () (stream-map show (enumerate-interval 1 10))))
;which is 
(stream-map show (enumerate-interval 1 10))
;which finally shows 1 until we get to 5
;This isn't a completely accurate re-writing / evaluation of stuff, but it's 
;mainly showing show cons-stream delays the evaluation of all expressions... 
;even those with a stream-cdr