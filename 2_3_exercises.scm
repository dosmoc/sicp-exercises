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

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

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

;(deriv '(x + 3 * (x + y + 2)) 'x)
;laterz

;2.3.3  Example: Representing Sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))       
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;Exercise 2.60.

;stays the same, but potentially could have to
;iterate through many duplicated items
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;this one is better... you don't need the element-of-set
;test
(define (adjoin-set x set) (cons x set))

;this one gets worse because you can have duplicates
;so element-of-set? may be called more
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;this one is better... you don't need the element-of-set
;test
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (append set1 set2))))

;Todo: applications?

;Sets as ordered lists
;still O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;from O(n^2) to O(n)

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
       (else (let ((x1 (car set1)) (x2 (car set2)))
               (cond
                ((= x1 x2) 
                 (cons x1 (union-set (cdr set1) (cdr set2))))
                ((< x1 x2)
                 (cons x1 (union-set (cdr set1) set2)))
                ((< x2 x1)
                 (cons x2 (union-set set1 (cdr set2)))))))))

;Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;Exercise 2.63.  Each of the following two procedures
; converts a binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;definition of append:
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define tree-a '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

(define tree-b '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define tree-c '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))


;a. Do the two procedures produce the same result for 
;   every tree? If not, how do the results differ?
;   What lists do the two procedures produce 
;   for the trees in figure 2.16?

(tree->list-1 tree-a)
;(1 3 5 7 9 11)
(tree->list-1 tree-b)
;(1 3 5 7 9 11)
(tree->list-1 tree-c)
;(1 3 5 7 9 11)

(tree->list-2 tree-a)
;(1 3 5 7 9 11)
(tree->list-2 tree-b)
;(1 3 5 7 9 11)
(tree->list-2 tree-c)
;(1 3 5 7 9 11)

;they appear to produce the same results

;b. Do the two procedures have the same order of growth
;   in the number of steps required to convert a balanced 
;   tree with n elements to a list? If not, which one grows 
;   more slowly?

;todo orders of growth stuff

;Sets and information retrieval

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set)))))

(define (lookup given-key set-of-records)
  (if (null? set-of-records) false
        (let ((entry-key (key (entry set-of-records))))
         (cond 
          ((equal? given-key entry-key)
           (entry set-of-records))
          ((< given-key entry-key)
           (lookup given-key (left-branch set)))
          ((> given-key entry-key)
           (lookup given-key (right-branch set)))))))

;2.3.4  Example: Huffman Encoding Trees

;Representing Huffman trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;The decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;Sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
;(a d a b b c a)

;Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))
  
  (define (iter bits current-branch)
    (cond ((and (leaf? current-branch)
                (equal? s (symbol-leaf current-branch))) bits)
          ((element-of-set? s (symbols (left-branch current-branch)))
           (iter (append bits (list '0)) 
                 (left-branch current-branch)))
          ((element-of-set? s (symbols (right-branch current-branch)))
           (iter (append bits (list '1)) 
                 (right-branch current-branch)))
          (else (error "symbol not in encoding tree"))))
  (iter '() tree))

;testing
(equal?
  (encode (decode sample-message sample-tree) sample-tree)
  sample-message)
;#t

;Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define freqs '((A 4) (B 2) (C 1) (D 1)))

(define leaf-set (make-leaf-set freqs))

(define leaf-set2 (make-code-tree (car leaf-set) (cadr leaf-set)))

(make-code-tree (car leaf-set2) (cadr leaf-set2))

(define (successive-merge nodes)
  (if (null? (cdr nodes))
      (car nodes)
      (successive-merge 
        (cons (make-code-tree (cadr nodes) (car nodes))
              (cddr nodes)))))

(successive-merge leaf-set)
;((leaf a 4) ((leaf b 2) ((leaf c 1) (leaf d 1) (c d) 2) (b c d) 4) (a b c d) 8)
;is it possible to get rid of that final car?

;Exercise 2.70

;manually reordered -- sorting how?
(define alpha-freqs
  '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1)))

(define h-tree (generate-huffman-tree alpha-freqs))

(define message
  '(get a job
          sha na na na na na na na na
          get a job
          sha na na na na na na na na
          wah yip yip yip yip yip yip yip yip yip
          sha boom))

(define encoded-message
 (encode message (generate-huffman-tree alpha-freqs)))

(decode encoded-message )

encoded-message
;(1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 0)
(length encoded-message)
;87 bits needed
;There are 8 sequences, so we need three bits per character if
;we use fixed codes, so 108 bits total

;Exercise 2.71
;todo do a skeeeetch
;One bit for the most frequent
;n - 1 for the least frequent

(define freqs2
  '((a 16) (b 8) (c 4) (d 2) (e 1)))

(generate-huffman-tree freqs2)

;((leaf a 16) ((leaf b 8) ((leaf c 4) ((leaf d 2) (leaf e 1) (d e) 3) (c d e) 7) (b c d e) 15) (a b c d e) 31)

;Exercise 2.72
;Todo orders of growth analysis