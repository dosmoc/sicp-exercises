;2.2 Hierarchical Data and the Closure Property

;2.2.1 Representing Sequences
;nil isn't predefined, so defining it even though we don't learn
;about quotation until later
(define nil '())

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;Exercise 2.17
;first attempt
(define (last-pair items)
  (define (last items prev)
    (if (null? items)
        prev
        (last (cdr items) (car items))))
  
  (last (cdr items) (car items)))
  
;works! but maybe there's something better:
(define (last-pair items)
  (let ((next (cdr items)))
   (if (null? next)
      items
      (last-pair next))))
;also works!

;Exercise 2.18
(define (reverse items)
  (define (reverse-it reversed items)
    (if (null? items)
        reversed
        (reverse-it (cons (car items) reversed) (cdr items))))
  
  (reverse-it nil items))
;this one works

;trying for more recursive
(define (reverse2 items)
  (if (null? (cdr items))
      (car items)
      (cons (reverse (cdr items)) (car items))))

;this gets (reverse (list 1 2 3) => ((3 . 2) . 1), so not quite
;want we want to get is (cons 3 (cons 2 (cons 1 nil))
;what this turns into is:
;(cons (cons 3 2) 1)
;what we want is
;(cons 3 (cons 2 (cons 1 nil))

;(cons (car (cdr (cdr s))) 
;      (cons (car (cdr s)) 
;            (cons (car s) nil)))

;Exercise 2.19
(define us-coins (list 25 50 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination items)
  (car items))

(define (except-first-denomination items)
  (cdr items))

(define (no-more? items)
  (null? items))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)

;The order of the list does not affect the answer by cc?
;Todo: why?

;Exercise 2.20
(define (item-filterer items filter-fn?)
  (cond ((null? items) nil)
        ((filter-fn? (car items)) 
         	(cons (car items) (item-filterer (cdr items) filter-fn?)))
        (else 
          	(item-filterer (cdr items) filter-fn?))))

(define (same-parity x . rest)
  (cons x (item-filterer rest (if (even? x) even? odd?))))

;Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list items)
  (map (lambda (x) (* x x)) 
       items))

(square-list (list 1 2 3 4))

;Exercise 2.22

;See my Exercise 2.18 implementation of reverse
;Essentially he's consing up a list starting with
;the first element: (cons first-element nil)
;then iteratively consing on more elements:
;(cons second-element (cons first-element nil))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3 4))

;And doing something similar for the second, which has 
;this form (cons (cons (cons nil third-element) second-element) first-element))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))

;Exercise 2.23
;kinda dumb way, taking advantage of the evaluation of
;forms in a definition... I don't actually know if 
;this fact is specified earlier in SICP
;not so good because we're still consing up the list
;just throwing it away to meet the specifications of the 
;exercise
(define (for-each proc items)
  (map proc items)
  #t)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;so probably it's more this:
;See footnotes on cond under 1.1.6
(define (for-each proc items)
  (cond ((null? items) #t)
        (else
          (proc (car items))
          (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;2.2.2  Hierarchical Structures
(cons (list 1 2) (list 3 4))

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;Exercise 2.24
 (list 1 (list 2 (list 3 4)))
;result from interpreter: (1 (2 (3 4)))
 
;Exercise 2.25
(define list-1 (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr list-1)))))

(define list-2 (list (list 7)))

(car (car list-2))

(define list-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-3))))))))))))

;Exercise 2.26.  Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;(1 2 3 4 5 6)
(cons x y)
;((1 2 3) 4 5 6)
(list x y)
;((1 2 3) (4 5 6))

;Exercise 2.27
(define (deep-reverse items)
  (define (reverse-it reversed items)
    (cond ((null? items) reversed)
          ((pair? (car items))
           (reverse-it (cons (reverse-it nil (car items))
                             reversed)
                       (cdr items)))
          (else (reverse-it (cons (car items) reversed) 
                            (cdr items)))))
  (reverse-it nil items))

(define x (list (list 1 2) (list 3 4)))

(reverse x)

(deep-reverse x)


;Exercise 2.28
;(define (fringe items)
;  (cond ((null? (cdr items)) nil)
;    	((not (pair? items)) items)
;        (else (cons (fringe (car items)) (fringe (cdr items))))))
;doesn't work

(define x (list (list 1 2) (list 3 4)))

;(define (fringe items)
;  (define (iter x items answer) 
;    (cond ((null? x) (iter (car items) (cdr items) answer))
;          ((not (pair? x) (iter (car items) (cdr items) answer))
;          (predicate2 consequent2))))
;  
;(define (fringe items)
;  (if (pair? (car items))
;      (fringe (cdr items))
;      (cons (car items) (fringe (cdr items)))))
;we car the first item, find it's a pair
;the we must send that pair to again and car ... if it's not a pair, than that's the answer
;then extract the rest of that, car it, and see if it's a pair

(define 1st (car (car x)))
(define 2nd (car (cdr (car x))))
(define 3rd (car (car (cdr x))))
(define 4th (car (cdr (car (cdr x)))))

(cons (car (car x)) (cons (car (cdr (car x))) (cons (car (car (cdr x))) (cons (car (cdr (car (cdr x)))) nil))))

(define (fringe items) 
  (cond 
    ((null? items) items)
    ((pair? (car items)) (fringe (append (car items) (fringe (cdr items)))))
    ((not (pair? (car items))) (cons (car items) (fringe (cdr items))))
    (else items)))
;works!

(define (fringe items) 
  (cond 
    ((pair? items) (append (fringe (car items)) (fringe (cdr items))))
    ((null? items) nil)
    (else (list items))))
;this one is a bit more clear

(list (list 1 2))

;this is what a list looks like (cons 1 (cons 2 (cons 3)))
;x is (cons (cons 1 (cons 2 nil)) (cons (cons 3 (cons 4 nil)) nil))

;Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;this is a tree with data at the nodes:

;a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;b)
(define m1 
  (make-mobile (make-branch 1 (make-mobile (make-branch 2 3) (make-branch 3 4))) (make-branch 5 6)))

(define (weight? structure)
  (not (pair? structure)))

(define (identity x) x)

(define (processor structure)
  (if (weight? structure)
      identity
      total-weight))

(define (total-weight mobile)
   (define (branch-weights x)
     (let ((structure (branch-structure x)))
      (if (weight? structure)       
         structure  
         (+ (branch-weights (left-branch structure))
            (branch-weights (right-branch structure))))))
   
   (+ (branch-weights (left-branch mobile))
      (branch-weights (right-branch mobile))))

(define (total-weight mobile)
   (let ((left (left-branch mobile))
         (right (right-branch mobile)))
     (let ((lstruct (branch-structure left))
           (rstruct (branch-structure right)))
       (cond ((and (weight? lstruct) (weight? rstruct)) 
              	(+ lstruct rstruct))
             ((weight? lstruct) 
              	(+ lstruct (total-weight rstruct)))
             (else (+ rstruct (total-weight lstruct)))))))


(define (total-weight mobile)
   (let ((left (left-branch mobile))
         (right (right-branch mobile)))
     (let ((lstruct (branch-structure left))
           (rstruct (branch-structure right)))
       (+ ((processor lstruct) lstruct) ((processor rstruct) rstruct)))))

;starting to see the need for type tagging... if you can tag as a mobile or struct, 
;the program might start to be easier to understand

(define m1 
  (make-mobile (make-branch 1 (make-mobile (make-branch 2 3) (make-branch 3 4))) (make-branch 5 6)))

;c)
(define (balanced-mobile? mobile)
   (define (balanced? wx lx wy ly)
     (= (* wx lx) (* wy ly)))
  
   (let ((left (left-branch mobile))
         (right (right-branch mobile)))
     (let ((lstruct (branch-structure left))
           (rstruct (branch-structure right))
           (llength (branch-length right))
           (rlength (branch-length right)))
       (cond ((and (weight? lstruct) (weight? rstruct)) 
              	(balanced? lstruct llength rstruct rlength))
             ((weight? lstruct) 
              	(and (balanced? lstruct llength (total-weight rstruct) rlength)
                     (balanced-mobile? rstruct)))
             (else (and (balanced? rstruct rlength (total-weight lstruct) llength)
                        (balanced-mobile? lstruct)))))))

(balanced-mobile? m1)
(define m2 (make-mobile (make-branch 1 2) (make-branch 1 2)))
(balanced-mobile? m2)
;d)
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;don't have to change much, just the selectors for right-branch and branch-structure
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(define m1 
  (make-mobile (make-branch 1 (make-mobile (make-branch 2 3) (make-branch 3 4))) (make-branch 5 6)))

;Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))

;Exercise 2.31.  
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

;(subsets (list 1))
;(rest (subsets '()))
;(append (subsets '()) (map ? (subsets '())))
;(append (nil) (map ? (nil)))
;This is where I noticed that there's no way
;to have 1 as part of the answer unless it's 
;used in the map function
;(append (nil) (map (lambda (x) (append (list (car (list 1)) x))) (nil)))
;reduces to
;(append (nil) (map (lambda (x) (append (1) x))) (list nil))
;the map creates a list of appends of the car of s to the rest
;(append (nil) ((append (1) nil))
;(append (nil) ((1))
;(() (1))
;
;you strip off the first item in the set and append it to the subsets
;
;(subsets (list 1 2))
;(rest (subsets (list 2))
;(append (subsets (list 2)) (map (lambda (x) (append (list (car s)) x)) (subsets (list 2)))

(subsets (list 1 2 3))

;2.2.3  Sequences as Conventional Interfaces
;we need a definition of fib for these to work
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* p q) (* q (+ p q)))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(map square (list 1 2 3 4 5))
;(1 4 9 16 25)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))
;(1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
;15
(accumulate * 1 (list 1 2 3 4 5))
;120
(accumulate cons nil (list 1 2 3 4 5))
;(1 2 3 4 5)


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
;(2 3 4 5 6 7)

;To enumerate the leaves of a tree, we can use

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;(1 2 3 4 5)

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(list-fib-squares 10)
;(0 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))

;Exercise 2.33.  
;Fill in the missing expressions to complete the 
;following definitions of some basic 
;list-manipulation operations as accumulations:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;Exercise 2.34
;Horener's rule structures computation as:
;(...(AnX + An-1)X + ... + a1)X + A0
;start with an, multiple by x, add an-1, multiply by x

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you
;would evaluate
(horner-eval 2 (list 1 3 0 5 0 1))

(horner-eval 3 (list 1 3 0 5 0 1))

(define x 3)
(+ (* 1 (expt x 0)) (* 3 (expt x 1)) (* 0 (expt x 2)) (* 5 (expt x 3)) (* 0 (expt x 4)) (* 1 (expt x 5)))

;Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:

;original:
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;accumulate:
(define (inc x) (+ x 1))

;count-leaves
;works, but I don't think this is actually what 
;they're looking for, but introduces a constant function
(define (count-leaves t)
  (accumulate +
              0 (map (lambda (x) 1) (enumerate-tree t))))

;this would work too, but doesn't use map like in the 
;text:
(define (count-leaves t)
  (length (enumerate-tree t)))

(count-leaves (cons (cons 1 2) 3))

(length (enumerate-tree (list 1 2 (list 3 4) (list 5 6))))

;you could do it this way, but doesn't seem as clear
(define (count-leaves t)
  (accumulate +
              0 (map (lambda (sub-tree)
				         (if (pair? sub-tree)
				             (count-leaves sub-tree)
				             1)) 
                     t)))


;Exercise 2.36.  
;(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) =>  
;(accumulate-n + 0 s)
;(22 26 30)

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
  
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 s)

;that was suprisingly cool; map is cool...so so cool

;Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(define v 
  (list (list 1 2 3 4) 
        (list 4 5 6 6) 
        (list 6 7 8 9)))

;Exercise 2.38
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;What are the values of
(fold-right / 1 (list 1 2 3)) 
;3/2
(fold-left / 1 (list 1 2 3))
;1/6
(fold-right list nil (list 1 2 3))
;(1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
;(((() 1) 2) 3)
;the op must be communitivity to guarantee the fold-right and fold-left will produce the same 
;values for any sequence

;Exercise 2.39
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (reverse sequence)
  (fold-right (lambda (x y) (cons y x)) nil sequence))
;this gets us the wrong thing:
;((((() . 4) . 3) . 2) . 1)
; something like (op 1 (op 2 (op 3 (op 4 nil))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;after looking at my notes on the reverse from Exercise 2.18
;and noticed append and went duh
;(it felt crazy at first, but makes sense)
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

;much clearer for fold left:
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;Nested Mappings


;(accumulate append
;            nil
;            (map (lambda (i)
;                   (map (lambda (j) (list i j))
;                        (enumerate-interval 1 (- i 1))))
;                 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;define prime
(define (prime? n)
  (define (next x) (if (= x 2) 3 (+ x 2)))
  
  (define (divides? a b)
    (= (remainder b a) 0))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n))) 

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(permutations (list 1 2 3))

;Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;Exercise 2.41
(define (sums-to-s n s)
  (define (sums? x)
    (= (+ (car x) (cadr x)) s))
  
  (map make-pair-sum (filter sums? (unique-pairs n))))

(sums-to-s 10 5)
;waitaminute this is about triples

(define (unique-triples n)
  (flatmap (lambda (k)
             (map (lambda (ij) (cons k ij))
               (unique-pairs (- k 1))))
           (enumerate-interval 1 n)))

(define (sums-to-s n s)
  (define (sums? x)
    (= (+ (car x) (cadr x) (caddr x)) s))
  
  (filter sums? (unique-triples n)))


;Exercise 2.42.  

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (safe? k postions))

(define (adjoin-position r k positions rest-of-queens))

(define empty-board (list (list)))

;uuugh