;2.3  Symbolic Data

;2.3.1  Quotation

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)

(car '(a b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))

(memq 'apple '(x (apple sauce) y apple pear))

;Exercise 2.53

(list 'a 'b 'c)
;(a b c)

(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
;((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;(y1 y2)


(pair? (car '(a short list)))
;#f
(memq 'red '((red shoes) (blue socks)))
;#f
(memq 'red '(red shoes blue socks))
;(red shoes blue socks)

;Exercise 2.54
(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

(define (u-equal? a b) 
  (if (not (pair? a))
      (eq? a b)
      (and (u-equal? (car a) (car b))
           (u-equal? (cdr a) (cdr b)))))
;works on the examples:
(u-equal? '(this is a list) '(this is a list))

(u-equal? '(this is a list) '(this (is a) list))

;but I missed some conditions that make this work completely.
;The code from billthelizard
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or  (null? a) (null? b)) #f)
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f)
        (else (eq? a b))))
;bruised ego


;Exercise 2.55
(car ''abracadabra)
;quote
;Quote makes 'abacadabra be treated as a data object. 
;Since quote is syntatic sugar for the quote procedure,
;the data object 'abracadabra is actually: (quote abacadabra)
;so just a list of the symbols quote and abacadabra, which the car
;of is quote
;
;I feel like I'm missing some subleties about quoting
;like conceptually, the list (a b c) is the literal list 
;object, but we can't write it as such in Scheme because
;that tries to apply a to b and c... 
;this is why a vector is written as #() in clojure... you can't 
;write a literal list as the intepreter prints back to you 
;like you could in Python because the language represents 
;code as lists
;So quotation like this is necessary because Lisp represents
;code and data the same way, whereas other languages do not
;It eschews convenient syntax for greater flexibility in 
;the language itself
;To do the symbolic differentiation example in Python would require
;a lot of objects to be written or an interpreter for strings
;
;You can't embed a DSL as easily

;2.3.2  Example: Symbolic Differentiation

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
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponent (base exp) 
                                                    (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
;(+ 1 0)
(deriv '(* x y) 'x)
;(+ (* x 0) (* 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* (* x y) (+ 1 0))
;   (* (+ (* x 0) (* 1 y))
;      (+  x 3)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
;1
(deriv '(* x y) 'x)
;y
(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))

;Exercise 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponent base exponent) 
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

;Exercise 2.57
;new
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (augend s) 
  (if (null? (cdddr s))
      (caddr s)
      (append (list '+ ) (cddr s))))

(define (addend s) (cadr s))

(define (multiplicand s) 
  (if (null? (cdddr s))
      (caddr s)
      (append (list '* ) (cddr s))))

(define (multiplier s) (cadr s))

;there's got to be a way that's less ugly than this
(define (simplifier op op-sym start const-pred)
  (define (not-number? x) (not (number? x)))
  
  (define (s . terms)
    (let ((cmp-or-vars (filter not-number? terms))
          (const (fold-right op start (filter number? terms))))
      (cond ((const-pred cmp-or-vars const) const)
            ((= (length cmp-or-vars) 1) (car cmp-or-vars))
            (else 
              (let ((const (if (= const start) '() (list const))))
               (append (append (list op-sym) cmp-or-vars) const))))))
  s)

(define make-product 
  (simplifier * '* 1 (lambda (cvs const)
                       (or (= const 0) (null? cvs)))))

(define make-sum 
  (simplifier + '+ 0 (lambda (cvs const) (null? cvs))))

(deriv '(+ x 3) 'x)
;1
(deriv '(* x y) 'x)
;y
(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))
(deriv '(* x y (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))
;works!

;Exercise 2.58

;a.
;just swap the position of the first 
;term and the operation in the selectors / constructors :
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

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

(deriv '(x + (3 * (x + (y + 2)))) 'x)

(deriv '(x + 3) 'x)
;1
(deriv '(x * y) 'x)
;y
(deriv '((x * y) * (x + 3)) 'x)
;((x * y) + (y * (x + 3)))

;b.