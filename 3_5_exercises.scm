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

;here, enumerate-interval constructs the entire interval
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
;this are already defined in mit-scheme
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
;actually used this and it seems straightforward. I do not yet
;know how Scheme's define-syntax works.
;
;PS Kernel is the other way around, as far as I can tell. Most
;Lisps evaluate the arguments, then apply the operand. A 
;special form is needed to introduce syntax... this special
;form (def-macro, define-syntax, defmacro, etc.) is a function;
;that's run at compile time that transforms it's input
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
;We will use the map from section 2.2.3, not the footnote:
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

;Exercise 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
;7
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
  
