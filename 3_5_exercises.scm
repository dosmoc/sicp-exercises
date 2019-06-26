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
;I was having a hard time understanding how this memo-proc could
;memoize the results of multiple calls to a procedure, but
;it must be used in conjunction with delay. For example:
;(define (y x) (display x) (* x x))
;(define z (delay (y 6)))
;(force z)
;6
;value 36
;(force z)
;value 36
;
;memo-proc is used to memoize the results of delayed objects
;like (delay (y 5)).
;I'm not exactly sure, but usage with cons-stream means that 
;you get the result and not a delayed object... in some computations
;you don't need to even evaluate a delayed object 


;from: http://stackoverflow.com/questions/14640833/how-is-the-sicp-cons-stream-implemented
;using delay as in the text
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

;I think it's very important to emphasize how much memoization plays a role in streams.
;Without it, computations are often repeated; the tradeoff for computation is additional
;space. Streams maybe be equivalent to mutation or iteration is terms of CPU, with
;memoizagion, but strict memoization will usually result in more memory usage. For very
;large computations, that could havepotentially result in slownes (in the base, we have
;RAM out the wazoo now adays) because of swapping... (even swapping may not be a problem
;soon).
;Of course, memoization can be defined such that it frees storage.
;Also it's interesting how memo-proc still must use a set! There's a point where
;you can't abtract away mutation of state.

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
; expression again. 

;trying to evaluate cons-stream gives you a syntax error:

; Syntactic keyword may not be used as an expression: #[keyword-value-item 14]
; To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

; In the environmental model of evaluation, a name in an environment
; can point to a pair of the lambda expression and a pointer to the environment
; it was created in
; but define-syntax makes something different
; 

;Exercise 3.52
(define (even? n)
  (= (remainder n 2) 0))

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
;sum is 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
seq
;(1 . #[compound-procedure 24])

(define y (stream-filter even? seq))
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

; because seq is used both in defining y
; and defining z:

; with memoization, the calls to (accum x) in 
; defining z using seq would just return
; 1, 3, 6 from the previously
; forced result of y 
; the set! is skipped
; 
; without memoization, set! is called when
; forcing z
; causing sum to increase anytime a delayed
; accum is forced
;
; in SICP's memoization procedure:
; memoized procedures that set! a value are only executed once when forced
; non-memoized procedures will set! the value whenever forced
; hypothesis: unless the set! is idempotent, then the variable updated
; by set! will have a different value between memoized and non-memoized
; versions of the procedure if the stream it is a part of is 
; used more than once

; basically: don't mix mutation and delayed evaluation unless you 
; know what you're doing

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
;defining integers this way feels a little like peano arithmetic

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
;I dont' think its so difficult to show if one understands that recursive
;fibonnaci calculations are already exponential, delaying doesn't really solve
;the problem without memoization. It'd probably be pretty similar to showing
;a recursive procedure grows exponentially.

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

;a.
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series input-stream)
  (mul-streams (div-streams ones integers) input-stream))

;b.
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define zeros (cons-stream 0 zeros))

(define (negative-stream the-stream)
  (stream-map - zeros the-stream))

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

(define (make-mul-test)
 (let ((the-test-stream  
         (add-streams (mul-series cosine-series cosine-series)
                      (mul-series sine-series sine-series))))
   (lambda (x) (stream-ref the-test-stream x))))

(define test-mul-series (make-mul-test))

(test-mul-series 1)
(test-mul-series 2)
(test-mul-series 3)
(test-mul-series 4)


;I'm still not understanding this recursive definition
;the series:  
; (a0 + a1 x + a2 x2 + a3 x3 )(b0 + b1 x + b2 x2 + b3 x3 )
; is (a0 * b0) + (the rest of the a series * the rest of the b series)
;
; using the thinking behind the section "Defining streams implicitly"
;
; the first element of mul-series is a0 * b0
; the multiplication of the rest of a and b series added to mul-series

;Exercise 3.61
;todo understand power series and ... calculus

(define (invert-unit-series s)
  (cons-stream 1 
               (mul-series (negative-stream (stream-cdr s))
                           (invert-unit-series s))))


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define s1 (integers-starting-from 1))
(define s2 (integers-starting-from 1))

(define s4 (invert-unit-series s2))

(stream-ref s4 0)
(stream-ref s4 1)
(stream-ref s4 2)
(stream-ref s4 4)

;the series is 1 2 3 4 5
; inverse = 1 - 3 - (02
; -

(define s3 
  (mul-series s1
              (invert-unit-series s2)))

(stream-ref s3 0)
(stream-ref s3 1)
;-3
; which is 1 + (-2 * 2)
(stream-ref s3 2)
;9
(stream-ref s3 3)
;-39

;Exercise 3.62
;todo understand power series and ... calculus


;3.5.3  Exploiting the Stream Paradigm

;Formulating iterations as stream processes
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define sqrt-2-aproximations (sqrt-stream 2))

(stream-ref sqrt-2-aproximations 2)
(stream-ref sqrt-2-aproximations 3)
(stream-ref sqrt-2-aproximations 4)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-ref pi-stream 0)
(stream-ref pi-stream 1)
(stream-ref pi-stream 2)
(stream-ref pi-stream 3)
(stream-ref pi-stream 4)
(stream-ref pi-stream 5)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define pi-stream2 (euler-transform pi-stream))

(stream-ref pi-stream2 0)
(stream-ref pi-stream2 1)
(stream-ref pi-stream2 2)
(stream-ref pi-stream2 3)
(stream-ref pi-stream2 4)
(stream-ref pi-stream2 5)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define pi-stream3 (accelerated-sequence euler-transform pi-stream))

(stream-ref pi-stream3 0)
(stream-ref pi-stream3 1)
(stream-ref pi-stream3 2)
(stream-ref pi-stream3 3)
(stream-ref pi-stream3 4)
(stream-ref pi-stream3 5)

;Exercise 3.63
;Louise thinks this is more straightforward:
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

;vs this:
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
  
;Alyssa says it is less efficient because 
;it performs redundant calculations
(sqrt-stream 4) ;gives us something like:

(cons-stream 1.0
               (delay (stream-map (lambda (guess)
                                    (sqrt-improve guess 4))
                                  (sqrt-stream 4))))

(stream-ref (sqrt-stream 4) 0) ;just gives us the car
;1.
;forces at least once
(stream-ref (sqrt-stream 4) 1)
;2.5

(cons-stream 1.0 
      ((if (stream-null? s)
           the-empty-stream
           (cons-stream (proc (stream-car (sqrt-stream 4)))
                        (stream-map proc (stream-cdr (sqrt-stream 4)))))))
;it looks like we're calculating (sqrt-stream 4) twice here so instinctively
;this feels like a recursive tree process like in 1.2.2
;although the calculations are delayed, you get redundant computations at
;each force, building a new stream at each level
 
(cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess 4))
                             guesses))
;forced at least once
(cons-stream 1.0  
      ((if (stream-null? s)
           the-empty-stream
           (cons-stream (proc (stream-car guesses))
                        (stream-map proc (stream-cdr guesses))))))
;guesses is the same stream -- You aren't re-calculating
;at each branch, just accessing the previously calculated values

;so should we instead do something like this:
(define (mul-series s1 s2)
  (define multiplied
    (cons-stream (* (stream-car s1)   ;cons the first multiplication onto the output stream
                    (stream-car s2))
               ;the rest of mul-series is the sums of the rest of s1 s2
               ;and this series
                 (add-streams (mul-streams (stream-cdr s1)  
                                           (stream-cdr s2))
                              multiplied)))
  multiplied)

(test-mul-series 1)
(test-mul-series 2)
(test-mul-series 3)
(test-mul-series 4)

;this is equivalent, but my definition of mul-series is wrong

;Exercise 3.64
(define (stream-limit the-stream tolerance)
  (define (examine the-stream)
    (let ((element1 (stream-car the-stream))
          (element2 (stream-car (stream-cdr the-stream))))
      (if (< (abs (- element1 element2)) tolerance)
          element2
          (examine (stream-cdr the-stream)))))
  (examine the-stream))


(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;Exercise 3.65
(define -ones (cons-stream -1 -ones))

(define (alternate s1 s2)
  (cons-stream (stream-car s1) (alternate s2 s1)))

(define ln2-denominators 
  (let ((alt-sign (alternate ones -ones)))
    (mul-streams integers alt-sign)))

(define ln2
  (div-streams ones ln2-denominators))

;this implicit style using stream-map kind of makes me confused
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
;the sign alternates because we're applying the procedure
;to the stream-car of the stream itself

(define ln2-stream 
  (partial-sums (ln2-summands 1)))

(define accelerated-ln2-stream
  (accelerated-sequence euler-transform
                        ln2-stream))

(stream-ref accelerated-ln2-stream 0)
(stream-ref accelerated-ln2-stream 1)
(stream-ref accelerated-ln2-stream 2)
(stream-ref accelerated-ln2-stream 3)
(stream-ref accelerated-ln2-stream 4)
;pretty quickly

;Infinite streams of pairs
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

; this is different from my alternate procedure,
; and more correct because it handles the null stream

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))

(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

;Exercise 3.66
;let's first take only the first part of 
;an infinite stream
;so it's easier to display
(define (dec n) (- n 1))

(define (take s n)
  (if (= n 0)
      the-empty-stream
      (cons-stream (stream-car s) (take (stream-cdr s) (dec n)))))

(display-stream (take int-pairs 20))
;(1 1)
;(1 2)
;(2 2)
;(1 3)
;(2 3)
;(1 4)
;(3 3)
;(1 5)
;(2 4)
;(1 6)
;(3 4)
;(1 7)
;(2 5)
;(1 8)
;(4 4)
;(1 9)
;(2 6)
;(1 10)
;(3 5)
;(1 11)
;Value: done

(define (take-while pred-fn s)
  (define (taker s)
    (let ((val (stream-car s)))
       (if (pred-fn val)
        the-empty-stream
        (cons-stream val (taker (stream-cdr s))))))
  (taker s))

(display-stream
  (take-while 
    (lambda (x) (equal? x '(1 100)))
    int-pairs))

(define (count-stream s)
  (define (iter s n)
    (if (stream-null? s)
        n
        (iter (stream-cdr s) (+ n 1))))
  
  (iter s 0))

(count-stream
  (take-while 
    (lambda (x) (equal? x '(1 100)))
    int-pairs))
;197 pairs

(count-stream
  (take-while (lambda (x) (equal? x '(99 100)))
              int-pairs))
;Aborting!: out of memory
;GC #17: took:   0.25  (45%) CPU time,   0.26  (46%) real time; free: 3408073
;GC #18: took:   0.25 (100%) CPU time,   0.24 (100%) real time; free: 3408131

;whoa, why are we running out of memory?
;the car increases veeery slowly
;so it looks like it'll take a long time to get to '(99 100)

;Are we running out of memory because its memoized?