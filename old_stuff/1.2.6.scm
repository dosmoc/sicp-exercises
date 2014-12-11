(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n))) 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; trying to understand this
; the (fermat-test n) evaluates to a boolean, which means
; in a cond, the consequent will be evaluated
; which is a recursive call to fast-prime?
; if ever (fermat-test n) is false, the procedure returns false --it does not return 
; the results of the fermat-test directly because it needs to test multiple times
; to reduce the probablility of error
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1))) ;if the fermat test is true, loop
        (else false)))

;Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start limit)
  (define (get-odd n)
    (if (even? n) (+ n 1) n))
  
  (define (search-it current found dummycall)
    (cond ((= found limit) (newline))
          ((prime? current) (search-it (+ current 2) (+ found 1) (timed-prime-test current)))
          (else (search-it (+ current 2) found dummycall))))
  (search-it (get-odd start) 0 (timed-prime-test start)))


(search-for-primes 10000000000 3)
(search-for-primes 100000000000 3)
(search-for-primes 1000000000000 3)
(search-for-primes 10000000000000 3)

;Exercises 1.25 and 1.26:

;the substitution of fast-expt makes large deferred operations?
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (expmod-ph base exp m)
  (remainder (fast-expt base exp) m))

;this is tranformed to O(n) because the explicit multiplications
;make this a tree recursion

;is this a rule of thumb? iterative processes must only call
;the procedure once to be iterative, as that makes another branch
(define (expmod-lr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod-lr (/ exp 2) m)
                       (expmod-lr (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod-lr (- exp 1) m))
                    m))))

;Exercise 1.27
  (define (fast-expt b n)
    (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (all-less-congruent? n)
  (define (congruent-modulo? a b) 
    (= (remainder a n) (remainder b n)))
  
  (define (test-congruent counter)
    (cond ((= counter 0) true)
          ((congruent-modulo? (fast-expt counter n) (remainder counter n)) (test-congruent (- counter 1)))
          (else false)))
 
  (test-congruent n)
)

;testing
(all-less-congruent? 5)
(all-less-congruent? 7)
(all-less-congruent? 8)
(all-less-congruent? 9)

;carmichaels
(all-less-congruent? 561)
(all-less-congruent? 1105)
(all-less-congruent? 1729)
(all-less-congruent? 2465)
(all-less-congruent? 2821
(all-less-congruent? 6601)

