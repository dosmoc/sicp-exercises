(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;Exercise 2.58

;b.
(define x3 '(x * y + y * (x + 3)))
;this is a sum with a1 (x * y) and a2 (y * (x + 2))
; to get the a1 and a2, you have to find the first
; occurence of the '+ symbol and seperate the terms at that
; occurence

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (take-until pred list-x)
  (define (iter-append result curr-list)
    (let ((current-item (car curr-list)))
      (if (pred current-item)
          result
          (iter-append (append result (list current-item))
                       (cdr curr-list)))))
  (iter-append '() list-x))

(define (drop-until pred list-x)
  (let ((current-item (car list-x))
        (rest (cdr list-x)))
    (if (pred current-item)
        rest
        (drop-until pred rest))))

(define (eq+? x) (eq? x '+))
(define (eq*? x) (eq? x '*))

(define (single-element? x) (= (length x) 1))
(define (extract-term term-list) 
  (if (single-element? term-list)
        (car term-list)
        term-list))

(define (make-sum a1 a2) (list a1 '+ a2))

(define (make-product m1 m2) (list m1 '* m2))

(define (sum? x)
  (and (pair? x) (not (not (memq '+ x)))))

(define (addend x)
  (extract-term (take-until eq+? x)))

(define (augend x)
  (extract-term (cdr (memq '+ x))))

;the ordering is enforced by testing to see
;if it is also a sum
;this way it doesn't matter what order the
;derivation rules are written in
(define (product? x)
  (and (pair? x)
       (not (not (memq '* x)))
       (not (sum? x))))

(define (multiplicand x)
  (extract-term (take-until eq*? x)))

(define (multiplier x)
  (extract-term (cdr (memq '* x))))

;(deriv '(x + 3 * (x + y + 2)) 'x)

(define x1 '(x + 3 * (x + y + 2)))

(define x2 '(x * y * (x + 3)))
;(deriv x2)
;(x * y + y * (x + 3))
;(((y * (x + 3)) * 1) + ((((x + 3) * 0) + ((1 + 0) * y)) * x))
;reduces to
;(y * (x + 3) + (y * x))
;so that's correct

(define x0 '(x + 3))

;let's use the simplification procedures from earlier
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(deriv '(x * y * (x + 3)) 'x)
;((y * (x + 3)) + (y * x))
;hmmm, much reduced, but we want (y * (x + 3) + y * x)
