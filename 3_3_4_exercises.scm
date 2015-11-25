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

;I originally hit on this by just messing 
;around, but here's a truth table:

; p | q | (not (not p) and (not q))
; --+---+------------------
; T | T |  F
; T | F |  F
; F | T |  F
; F | F |  T

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
;it was very weird working on this, since you're not feeding
;the input of a function to another function... the wires
;are connecting functions in the way it's modeled, which
;I guess mimics actual wiring
;Like having an IN/OUT parameter in PL/SQL, a pointer to 
;a location that's to be mutated; the wires are mutated
;each time they are passed to a procedure

;Exercise 3.30
(define (ripple-carry-adder a-wires b-wires s-wires c)
  (if (not (null? a-wires))
      (let ((c-out (make-wire)))
        (full-adder (car a-wires) (car b-wires) c (car s-wires) c-out)
        (ripple-carry-adder (cdr a-wires) (cdr b-wires) (cdr s-wires) c-out)
        'ok)))

;a full adder is two half adders and an or gate delay
;a half adder is two and gates, and or gate, and an inverter gate delay
; so (define half-adder-delay (+ (* 2 and-gate-delay) inverter-delay or-gate-delay))
; (define full-adder-delay (+ (* 2 half-adder-delay) or-gate-delay))
; so:
; (* n (+ (* 4 and-gate delays) (* 2 inverter-delays) (* 3 or-gate delays)))

;Representing wires
(define (make-wire)
   (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value) 
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;The agenda
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;A sample simulation
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
;(probe 'sum sum)
;sum 0  New-value = 0
;(probe 'carry carry)
;carry 0  New-value = 0
(half-adder input-1 input-2 sum carry)
;ok
(set-signal! input-1 1)
;done
(propagate)
;sum 8  New-value = 1
;done

(set-signal! input-2 1)
;done
(propagate)
;carry 11  New-value = 1
;sum 16  New-value = 0
;done

;Exercise 3.31
;what would happen if it were defined as:
; (define (accept-action-procedure! proc)
;   (set! action-procedures (cons proc action-procedures)))
;
;vs the correct implementation:
; (define (accept-action-procedure! proc)
;   (set! action-procedures (cons proc action-procedures))
;   (proc))
;
; instinct is that we need to set the initial state of the
; wire as soon as they are "connected" so that the ordering
; of subsequent set-signal! are correct

;queue representation from earlier:
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

;Implementing the agenda
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;Exercise 3.32
