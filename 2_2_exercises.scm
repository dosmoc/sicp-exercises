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

