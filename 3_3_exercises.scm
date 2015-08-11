;3.3 Modeling with Mutable Data

;3.3.1 Mutable List Structure
(define x '((a b) c d))
(define y '(e f))

(set-car! x y)

x
;((e f) c d)

(define z (cons y (cdr x)))

z
;((e f) c d)
(set-cdr! x y)
x
;((e f) e f)
y
;(e f)
z
;((e f) c d)

;Exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
;(a b c d)
(cdr x)
;(b)
(define w (append! x y))
w
(cdr x)
;(b c d)
;without box and pointer, append makes a completely new copy of 
;the list with y at the end... the original x is left untouched
;which z names

;append! mutates the cdr of the original x t y... w is like an alias
;for the structure that x is

;Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
;the cdr of the last pair points to from of x, or 
;the car of x
;Try to evaluate (last-pair z) will
;result in a not terminating recursion because there is no
;pointer to nil; an infinite loop

;Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;Mystery returns a reversed x, destroying
;x in the process 
(define v (list 'a 'b 'c 'd))
(define w (mystery v))
w
;(d c b a)
v
;(a)
;huuuh
;(x             y)
;(a b c d)    ()
;(b c d)      (a)
;(c d)        (b a)
;(d)          (c b a)
;()           (d c b a)
;why is v (a)?

;The first (set! x y) sets the cdr of x at the very top
;environment's to the empty list, which makes it (a) ...
;The calls to set! happen in the environment set up
;by the loop procedure, essentially making
;new frames that only effect the x bound locally

;Sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))
z1
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
;((a b) a b)
(set-to-wow! z1)
;((wow b) wow b)
z2
;((a b) a b)
(set-to-wow! z2)
;((wow b) a b)

;Exercise 3.15 
;More drawing

;Exercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;3
(define x3 (list 1 2 3))
(count-pairs x3)
;3

(define x4 '((1) 3))
(set-cdr! (car x4) (cdr x4))
(count-pairs x4)
;4
;[|] ---> 
; |      |
;[1|]->[3|]

(define x7 '((1) 3))
(set-cdr! (car x7) (cdr x7))
(set-car! (car x7) (cdr x7))
(set-cdr! x7 (car x7))
(count-pairs x7)
;7
;[ | ]  
; | |   
;[ | ]
; | |
;[3| ]

;Never return at all:
(define x-cycle (list 1 2 3))
(set-cdr! x-cycle x-cycle)
;(count-pairs x-cycle) does not return

;Exercise 3.17 
(define x (cons 1 2))
(define y x)
(pair? x)
(eq? x y)
;#t

(define (inc x) (+ x 1))

(define (count-pairs x)  
  (define (element-of-set? x set)
  	(cond ((null? set) false)
    	    ((eq? x (car set)) true)
        	(else (element-of-set? x (cdr set)))))
  
  (define loop
    (let ((already-counted '()))
     (lambda (x)
      (cond ((not (pair? x)) 0)
            ((element-of-set? x already-counted) 
             (+ (loop (car x)) (loop (cdr x))))
            (else
             (set! already-counted (cons x already-counted))
             (+ (loop (car x)) (loop (cdr x)) 1))))))
  
  (loop x))

(count-pairs x3)
(count-pairs x4)
;This doesn't deal with cycles

;Exercise 3.18
;hmmm
;you can't simply say, if this pair was encountered
;before, it has a cycle, since a pair can be pointed
;to by an arbitrary number of other pairs,
;but if you encounter the pointing pair
;and the pair you're currently on more than once, then
;you've got a cycle;

(define (contains-cycle? data)
  ())

(define x '((1) 2))
(eq? x x)

(define cycle1 
  (let ((x (list 'a 'b 'c)))
    (set-cdr! x x)
    x))

(define prev (cons cycle1 (cdr cycle1)))

(eq? cycle1 (cdr cycle1))

(define pairs (list prev))

(define (element-of-set? x set)
	(cond ((null? set) false)
	    ((eq? x (car set)) true)
    	(else (element-of-set? x (cdr set)))))

(element-of-set? prev pairs)

(define list-contains-cycle? 
  (let ((prev '()))
     (lambda (x)
       (newline)
       (cond ((not (pair? x)) #f)
             ((element-of-set? x prev) #t)
              (else
               (set! prev (cons x prev))
               (list-contains-cycle? (cdr x)))))))

(list-contains-cycle? cycle1)

(define cycle2 
  (let ((x (list 'a 'b 'c)))
    (set-cdr! (last-pair x) x)
    x))

(list-contains-cycle? cycle2)

(list-contains-cycle? (list 'a 'b 'c))

;but this is just for lists... trees are harder,
;since you can get to the end of a list in the
;car or cdr, but there will be a cycle in the
;cdr or car

;you have to test all of them, but somehow ignore
;branches where there is no cycle... this is
;tough because all procedures in scheme return
;a value, and I'm not sure how to do this 
;without recursion

;lets try this idea
(define (contains-cycle? x)
  (define terminates?
   (let ((prev '()))
     (lambda (x)
       (cond ((not (pair? x)) #t)
             ((element-of-set? x prev) #f)
              (else
               (set! prev (cons x prev))
               ;we're depending on short circuit
               ;evaluation here to avoid an
               ;infinite loop; If terminates? is false
               ;the "and" operator doesn't evaluate
               ;and the entire expression is false
               (and (terminates? (car x))
                    (terminates? (cdr x))))))))
  (not (terminates? x)))

(contains-cycle? cycle2)
;#t
(contains-cycle? (list 'a 'b 'c))
;#f
(define tree-cycle 
  (let ((x (list (list 1 2 3) 4 5 6)))
    (set-cdr! (last-pair (car x)) x)
    x))

(contains-cycle? tree-cycle)
;#t 
;cool
(contains-cycle? (list (list 'a 'b 'c) 'd 'e 'f))
;#f

;After banging my head on this for a while, I get
;why allowing mutation is troublesome... I find
;it more difficult having to think about the order of 
;when things happen beyond definitions

;Exercise 3.19
;Reading a little bit about cycle detection
;wikipedia is useful here:
;http://en.wikipedia.org/wiki/Cycle_detection

(define (floyd-cycle? f x0 stop-fn)
  ;we're ignoring where the cycle occurs
  ;as in the wiki example... we're just interested
  ;in there occurence of a cycle
  (define floyd-loop
   (let ((tortoise (f x0))
         (hare     (f (f x0))))
    (lambda ()
     (cond ((stop-fn (f hare)) #f) 
           ((eq? tortoise hare) #t)
           (else (set! tortoise (f tortoise))
                 (set! hare     (f (f hare)))
                 (floyd-loop))))))
  (floyd-loop))

(define (contains-cycle? x)
  (floyd-cycle? cdr x (lambda (x) (not (pair? x)))))

(contains-cycle? (list 'a 'b 'c))
;f
(contains-cycle? cycle1)
;#t
(contains-cycle? cycle2)
;#t

;Need to read about depth first search

;Mutation is just assignment
;cons, car, and cdr as procedures
(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

;same for set-car! and set-cdr! 
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;Exercise 3.20
;I swear I'll do this

;3.3.2  Representing Queues

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

;Exercise 3.21
(define q1 (make-queue))
(insert-queue! q1 'a)
;((a) a)
(insert-queue! q1 'b)
;((a b) b)
(delete-queue! q1)
;((b) b)
(delete-queue! q1)
;(() b)

;interesting, what's printed when we do this:
(insert-queue! q1 'c)
;b isn't inserted twice. The queue representation is
;a cons of a reference to the front and rear of 
;a list... in this case, the cdr of the queue points 
;to the last item in the list, that's why we see 'b
;twice printed 
;that's equivalent to
(cons '(b) '(b))

;when deleting the last 'b, the front pointer is
;pointing to the empty list, because it has been
;set to the cdr of b
(cdr '(b)) 
;()
;but the rear pointer still points to the cdr of the
;list ... so you get a structure like
(cons () '(b))
;I wonder if that has implications for garbage collection
;since there's still a pointer to the last item in the
;list...but maybe it's all the same, because an empty
;queue retains pointers to empty lists...

;anyways, printing a queue should be easy:

(define (print-queue queue)
  (display "queue: ")
  (display (car queue)))

(print-queue q1)
;queue: (c)
(insert-queue! q1 'a)

(print-queue q1)
;queue: (c a)

;Huh, functional queues?
;http://www.randomhacks.net/2007/02/08/haskell-queues-without-pointers/

;I'm sure I've misunderstood the article:
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (car (car queue)))

(define (insert-queue queue item)
  (if (null? (car queue))
      (cons (reverse (cons item (cdr queue))) '())
      (cons (car queue) (cons item (cdr queue)))))

(define (delete-queue queue)
  (if (null? (car queue))
      (cons (reverse (cdr queue)) '())
      (cons (cdr (car queue)) (cdr queue))))

(define (print-queue queue)
  (display (append (car queue) (reverse (cdr queue)))))

(define q2 (insert-queue (insert-queue q1 'a) 'b))
(print-queue q2)

(define q3 (delete-queue (insert-queue (insert-queue q1 'a) 'b)))
(print-queue q3)

;but queues inherently appear to operate on a variable
;in place:
;(define x (make-queue))
;(insert-queue! x 'a)
;x => (a) 
;this is pretty imperative
;Haskell has something called the state Monad... hmmm

;Exercise 3.22
;Well, let's take some inspiration from that and add some mutation
;I'm pretty sure that's not what the assignment intended...

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    
    (define (empty-queue?)
      (and (null? front-ptr) (null? rear-ptr)))
    
	(define (print-queue)
	  (display (append front-ptr (reverse rear-ptr))))
        
	(define (front-queue)
		(if (empty-queue?)
      		(error "FRONT called with an empty queue")
        	(car front-ptr)))

	(define (insert-queue! item)
	  (begin 
     	(if (null? front-ptr)
	        (begin (set! front-ptr (reverse (cons item rear-ptr)))
         	       (set! rear-ptr '()))
	        (set! rear-ptr (cons item rear-ptr)))
        (print-queue)))
 
	(define (delete-queue!) 
   	  (cond ((empty-queue?) 
             (error "DELETE! called with an empty queue"))
	        ((null? front-ptr)
	         (set! front-ptr (cdr (reverse rear-ptr)))
	         (set! rear-ptr '())
             (print-queue))
            (else (set! front-ptr (cdr front-ptr))
                  (print-queue))))

    (define (dispatch m) 
      (cond ((eq? m 'front) (front-queue!))
            ((eq? m 'insert) insert-queue!)
            ((eq? m 'delete) (delete-queue!))
            ((eq? m 'print) (print-queue))
            ((eq? m 'empty?) (empty-queue?))
            (else (error "Undefined operation -- QUEUE" m))))
    
    dispatch))

(define (front-queue queue)
  (queue 'front))

(define (delete-queue! queue)
  (queue 'delete))

(define (insert-queue! queue item)
  ((queue 'insert) item))

(define (print-queue queue)
  (queue 'print))

(define (empty-queue? queue)
  (queue 'empty?))

(define q4 (make-queue))

(print-queue q4)

(insert-queue! q4 'a)
(insert-queue! q4 'b)
(insert-queue! q4 'c)

(print-queue q4)
;(a b c)
(delete-queue! q4)
;(b c)
(empty-queue? q4)
;#f
(delete-queue! q4)
;(c)
(print-queue q4)
;(c)
(delete-queue! q4)
;()
(delete-queue! q4)
;DELETE! called with an empty queue

;Exercise 3.23
;We appear to need a doubly linked list

(define (make-dl-node data prev next)
  (define (set-prev-ptr! item)
    (set! prev item))
  
  (define (set-next-ptr! item)
    (set! next item))

  (define (dispatch m) 
    (cond ((eq? m 'data)  data)
          ((eq? m 'prev)  prev)
          ((eq? m 'next)  next)
          ((eq? m 'set-next!)  set-next-ptr!)
          ((eq? m 'set-prev!)  set-prev-ptr!)
          ((eq? m 'print) print)
          (else (error "Unknown operation -- make-list-node" m))))
  dispatch)

(define (dl-cons x y)
  ((x 'set-next!) y)
  ((y 'set-prev!) x)
  x)

(define (dl-snoc y x)
  ((x 'set-next!) y)
  ((y 'set-prev!) x)
  y)

(define (dl-car x)
  (x 'data))

(define (dl-cdr x)
  (x 'next))

(define (dl-prev x)
  (x 'prev))

;points to null on both ends
(define x (make-dl-node 1 '() '()))

(x 'data)
;1
(x 'prev)
;()
(x 'next)
;()

(define y (make-dl-node 2 '() '()))

(define z (dl-cons x y))

((z 'next) 'data)
;2
(((z 'next) 'prev) 'data)
;1
(dl-car z)
;1
(dl-car (dl-cdr z))
;2
(define w (dl-cdr z))

(dl-car (dl-prev w))

(define (inc x) (+ 1 x))

(define (dl-print dl)
  (display "dl-list: (")
  
  (define (loop items i)
   (if (null? items)
      (display ")")
      (begin (if (not (= i 0))
                 (display " ")
                 (display ""))
             (display (dl-car items))
             (loop (dl-cdr items) (inc i)))))
  
  (loop dl 0))

(dl-print z)

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (dl-car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (dl-car (rear-ptr deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (make-dl-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (((rear-ptr deque) 'set-next!) new-pair)
           ((new-pair 'set-prev!) (rear-ptr deque))
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (dl-cdr (front-ptr deque)))
         deque))) 

;these look really similar to the operations on queue

(define (front-insert-deque! deque item)
  (let ((new-pair (make-dl-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (((front-ptr deque) 'set-prev!) new-pair)
           ((new-pair 'set-next!) (front-ptr deque))
           (set-front-ptr! deque new-pair)
           deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (dl-prev (rear-ptr deque)))
         (if (null? (rear-ptr deque))
             (set-front-ptr! deque (rear-ptr deque))
             (((rear-ptr deque) 'set-next!) '()))
         deque))) 

;probably can make generic cons/car/cdr operations that work
;on both ordinary lists and a doubly linked list, with
;an operation that gets the previous item in the list
;that's just slower on regular lists

(define (print-deque deque) 
  (dl-print (front-ptr deque)))

(define test-deque (make-deque))

(rear-insert-deque! test-deque 1)
;(#[compound-procedure 15 dispatch] . #[compound-procedure 15 dispatch])
(front-deque test-deque)
;1
(rear-deque test-deque)
;1
(rear-insert-deque! test-deque 2)
;(#[compound-procedure 15 dispatch] . #[compound-procedure 18 dispatch])
(rear-deque test-deque)
;2
(rear-insert-deque! test-deque 3)

(rear-deque test-deque)
;3
(front-deque test-deque)
;1
(front-delete-deque! test-deque)

(front-deque test-deque)
;2

(front-delete-deque! test-deque)
(front-deque test-deque)
;3
(front-delete-deque! test-deque)
(front-deque test-deque)
;FRONT called with an empty deque...

(front-insert-deque! test-deque 1)

(front-deque test-deque)
;1
(front-insert-deque! test-deque 2)
(front-deque test-deque)
;2
(front-insert-deque! test-deque 3)
(front-deque test-deque)
;3
(rear-deque test-deque)
;1
(rear-delete-deque! test-deque)
(rear-deque test-deque)
;2
(rear-delete-deque! test-deque)
(rear-deque test-deque)
;3
(rear-delete-deque! test-deque)
(rear-deque test-deque)
;REAR called with an empty deque (())
(front-deque test-deque)

(front-insert-deque! test-deque 1)
(rear-insert-deque! test-deque 9)
(front-deque test-deque)
;1
(rear-deque test-deque)
;9s

;3.3.3 Representing Tables
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;;more like upsert
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define test-table (make-table))

(lookup 'pie test-table)
;#f
(insert! 'pie 25 test-table)
;ok
(lookup 'pie test-table)
;25
(insert! 'pie 65 test-table)
(lookup 'pie test-table)
;66
test-table


;Two-dimensional tables

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
    		  (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;Creating local tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			            (else (error "Unknown operation -- TABLE" m))))
			    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;Exercise 3.24
(define (make-table same-key?)
  (define (assoc key records)
	  (cond ((null? records) false)
	        ((same-key? key (caar records)) (car records))
	        (else (assoc key (cdr records)))))
  
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			            (else (error "Unknown operation -- TABLE" m))))
			    dispatch))

;Exercise 3.25
;(*table* (pie (cherry . 3) (peach . 3)) (pie2 (apple2 . 500)))

;lets think through the two key insert!:
;if it doesn't find a subtable for the key, set the cdr of the
;higher table to (cons (list key value) (cdr table))
;
;so you start with (*table* . ()) or just (*table*)
;then you get (*table* . (cons (cons 'pie 'cherry) '()))
;which is (*table* (pie cherry)) or (cons '*table* (cons (list 'pie 'cherry) '()))

(define (not-null? xlist) (not (null? xlist)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (list '*table*))


(define (lookup key-list table)
  (let ((subtable (assoc (car key-list) (cdr table)))
        (rest-keys (cdr key-list)))
    (cond ((and subtable (not-null? rest-keys))
           (lookup rest-keys subtable))
          ((and subtable (null? rest-keys))
           (cdr subtable))
          (else false))))

(define (insert! key-list value table)
  (define (rest-subtable key-list value)
    (let ((current-key (car key-list))) 
      (if (null? (cdr key-list))
          (cons current-key value)
          (list current-key (rest-subtable (cdr key-list)
                                           value)))))
  
  (let ((current-key (car key-list))
        (rest-keys (cdr key-list)))
   (let ((subtable (assoc current-key (cdr table))))
    (cond ((and subtable (null? (cdr rest-keys)))
           (set-cdr! subtable 
                     (cons (rest-subtable rest-keys value)
                           (cdr subtable))))
          ((and subtable (not-null? rest-keys))
           (insert! rest-keys value subtable))
          ((and subtable (null? rest-keys))
           (set-cdr! subtable value))
          (else (set-cdr! table
                          (cons (rest-subtable key-list value)
                                (cdr table)))))))
  'ok)

(pie) (cherry bing)
(cherry) (bing)

(define test-table (make-table))


(insert! (list 'pie2 'apple2) 500 test-table)
;ok
(lookup (list 'pie2 'apple2) test-table)
;500
(insert! (list 'pie 'peach) 3 test-table)
;0k
(lookup (list 'pie 'peach) test-table)
;3
(insert! (list 'pie 'cherry) 3 test-table)
;ok
(lookup (list 'pie 'cherry) test-table)
;3
(insert! (list 'pie 'cherry) 4 test-table)
;(*table* (pie (cherry . 4)) (pie3 (cherry . 4)))

(insert! (list 'pie 'cherry 'bing) 3 test-table)
;(*table* (pie (cherry (bing . 3))) (pie3 (cherry . 4)))

(insert! (list 'pie 'cherry 'sour) 9 test-table)
;(*table* (pie (cherry (bing . 3))) (pie3 (cherry . 4)))

(insert! (list 'pie 'apple) 4 test-table)

;hmmm.... should nodes carry their own value for insert!, or once a new level
;is added, everything is obliterated?
;so looking up ('pie 'cherry) would get you 4, but looking up ('pie 'cherry 'bing) gets you 3?
;this is different from how most hash maps work, but hashmaps aren't tables
;let's do this as a nested table representation then.

test-table

(lookup (list 'pie 'cherry 'sour) test-table)
;I'll think about this one later

;Exercise 3.26
;Let's stick to numerics for now, since we don't know about
;how to compare symbols alphabetically

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;;more like upsert
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;Exercise 3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
	(lambda (x)
	      (let ((previously-computed-result (lookup x table)))
	        (or previously-computed-result
	            (let ((result (f x)))
	              (insert! x result table)
	              result))))))

;3.3.4 A Simulator for Digital Circuits

;;For Section 3.3.4, used by and-gate
;;Note: logical-and should test for valid signals, as logical-not does
(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))

(define (logical-or x y)
  (if (or (= x 1) (= y 1))
      1
      0))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;Exercise 3.29

;needs to do this:
;(define (o a b) (not (and (not a) (not b))))
;(o #f #f) => #f
;(o #t #t) => #t
;(o #f #t) => #t
;(o #t #f) => #t

(define (or-gate a1 a2 output)
  (let ((wa (make-wire))
        (wb (make-wire))
        (wc (make-wire))
        (wd (make-wire)))
    (not-gate a1 wa)
    (not-gate a2 wb)
    (and-gate wa wb wc)
    (not-gate wc output)
    'ok))

;or-gate delay is (+ and-gate-delay (* 3 inverter-delay)

