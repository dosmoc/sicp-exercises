;2.5.3  Example: Symbolic Algebra
;Arithmetic on polynomials

(load "./2_5_exercises.scm")

;a simple polynomial in x:
;5x² + 2x + 7 

;a polynomial in x with coefficients that are polynomials in y:
;(y² + 1)x³ + (2y)x + 1 

;Are (= '(5x² + 2x + 7 ) '(5x² + 2x + 7 ))?
;Yes if we are considering them as mathematical functions,
;no if they are a syntatic form
;
; should the system recognize this?
; how should it be represented?
;
; SICP makes the decision that a polynomial will be
; a particular syntactic form
;
; - just addition and multiplication
; - polynomials to be combined will have the same indeterminate


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))  
  (define (variable? x) (symbol? x))
  
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; continued on next page
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  ;<procedures used by add-poly>
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else 
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else 
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  ;<procedures used by mul-poly>
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-terms-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (=zero? p)
    (= 0 (reduce + 0 (map coeff (term-list p)))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  ;From Exercise 2.87
  (put '=zero? '(polynomial) =zero?)
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;Exercise 2.87
;lets define them outside of the package to test

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define polynomial-terms-b '((100 1) (2 2) (0 1)))

(define polynomial-terms-b
  (adjoin-term (make-term 100 1)
    (adjoin-term (make-term 2 2))
      (adjoin-term (make-term 0 1) '())))

(adjoin-term (make-term 1 1) '())

(map type-tag '((1 1)))

 (coeff (make-term (make-scheme-number 1) (make-scheme-number 1)))
 
 (type-tag 1)

;=zero? for a polynomial must test to see if
;all coefficients of the polynomial are zero
;assuming we're using the representation above

;If the sum of coefficients is zero, the polynomial
;is zeros
(= 0 (reduce add 0 (map coeff polynomial-terms-)))
;#f

(define polynomial-terms-c '((100 0) (2 0) (0 0)))
(= 0 (reduce add 0 (map coeff polynomial-c)))
;#t

;see the procedure added to the polynomial package above
(=zero? (make-polynomial 'x polynomial-terms-c))

;Exercise 2.88

;Exercise 2.89

;Exercise 2.90
;This one sounds the funnest

;Exercise 2.91
;(define (div-terms L1 L2)
;  (if (empty-termlist? L1)
;      (list (the-empty-termlist) (the-empty-termlist))
;      (let ((t1 (first-term L1))
;            (t2 (first-term L2)))
;        (if (> (order t2) (order t1))
;            (list (the-empty-termlist) L1)
;            (let ((new-c (div (coeff t1) (coeff t2)))
;                  (new-o (- (order t1) (order t2))))
;              (let ((rest-of-result
;                     <compute rest of result recursively>
;                     ))
;                <form complete result>
;                ))))))