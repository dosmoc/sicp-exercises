;3.2 The Environmental Model of Evaluation

;3.2.2  Applying Simple Procedures

;Exercise 3.9

;Make sure to turn off word wrap for thiiiisss
;        +---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
; Global |                                                                                                                                                                     |
; Env    |                                                                                                                                                                     |
;        |                                                                                                                                                                     |
;        |                                                                                                                                                                     |
;        +----^----------------------------^----------------------------^----------------------------^----------------------------^----------------------------+---------------+
;             |                            |                            |                            |                            |                            |
;        +----+---+                   +----+---+                   +----+---+                   +----+---+                   +----+---+                   +----+---+
;        |        |                   |        |                   |        |                   |        |                   |        |                   |        |
;   E1+--> n : 6  |              E2+--> n : 5  |              E3+--> n : 4  |              E4+--> n : 3  |              E5+--> n : 2  |              E6+--> n : 1  |
;        |        |                   |        |                   |        |                   |        |                   |        |                   |        |
;        +--------+                   +--------+                   +--------+                   +--------+                   +--------+                   +--------+
; 
;  (if (= n 1)                  (if (= n 1)                  (if (= n 1)                  (if (= n 1)                  (if (= n 1)                  (if (= n 1)
;    (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))


; Global +-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
; Env    |                                                                                                                                                                                                                                                             |
;        |                                                                                                                                                                                                                                                             |
;        |                                                                                                                                                                                                                                                             |
;        +----^------------------------^----------------------------------^----------------------------------^----------------------------------^----------------------------------^----------------------------------^----------------------------------^-------------+
;             |                        |                                  |                                  |                                  |                                  |                                  |                                  |
;        +----+---+           +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+
;        |        |           |               |                  |               |                  |               |                  |               |                  |               |                  |               |                  |               |
;   E1+--> n : 6  |      E2+--> product : 1   |             E3+--> product : 1   |             E4+--> product : 2   |             E5+--> product : 6   |             E6+--> product : 24  |             E7+--> product : 120 |             E8+--> product : 720 |
;        |        |           | counter : 1   |                  | counter : 2   |                  | counter : 3   |                  | counter : 4   |                  | counter : 5   |                  | counter : 6   |                  | counter : 7   |
;        +--------+           | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |
;                             |               |                  |               |                  |               |                  |               |                  |               |                  |               |                  |               |
;      (fact-iter 1 1 n)      +---------------+                  +---------------+                  +---------------+                  +---------------+                  +---------------+                  +---------------+                  +---------------+
; 
;                        (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)
;                            product                            product                            product                            product                            product                            product                            product
;                            (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)
;                                       (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)
;                                       max-count))                        max-count))                        max-count))                        max-count))                        max-count))                        max-count))                        max-count))

;3.2.3  Frames as the Repository of Local State

;Exercise 3.10
;The expression
(let ((balance initial-amount))
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;is equivalent to:
((lambda (balance)
   (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
 initial-amount)

;This introduces an extra frame binding the initial-amount to 100 compared to the first version of make-withdraw. 

;Issuing (define W1 (make-withdraw 100)) creates an environment, E1, with intial-amount bound to 100. The body of
; make-withdraw is evaluated. Since the body of make withdraw is equivalent to:
;((lambda (balance)
;   (lambda (amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance)
;        "Insufficient funds")))
; initial-amount)

;We set up a new environment E2, binding balance to 100. A procedure object is returned to W1:
;(lambda (amount)
;  (if (>= balance amount)
;      (begin (set! balance (- balance amount))
;             balance)
;      "Insufficient funds"))

;The procedure object points to code with argument and argument of amount and the following body:
;(if (>= balance amount)
;   (begin (set! balance (- balance amount))
;          balance)
;   "Insufficient funds")

;The procedure's environment part points to E2
;Evaluating
(W1 50)
;Mutates the balance in E2

(define W2 (make-withdraw 100))
;Sets up new environments, E3 with initial-amount bound to 100, and E4, with balance bound to 100. W2's procedure
;object code part points to the same code as W1; it's environment part points to E4.

;Exercise 3.11
(define acc (make-account 50))
;             +----------------------------------------------------------------+
;     Global-->                                                                |
;     Env     | acc: +                                                         |
;             |      |                                                         |
;             |      |                                                         |
;             |      |                                                         |
;             |      |                                                         |
;             +---------------------^------------------------------------------+
;                    |              |
;                    |        +-----+----------+
;                    |   E1+-->                |
;                    |        | balance: 50    |
;                    |        | withdraw: +---------------------+
;                    |        | deposit: +------------+         |
;                    |        | dispatch: |    |      |         v
;                    |        |           |    |      |   parameters: amount
;                    |        +-----^---------++      |   body: (begin (set! balance ...)
;                    v              |     |   ^       v
;  +---------------+(|)+------------+     v   |   parameters: amount
;  |      +-----------------------------+(|)+-+   body: (set! balance ...)
;  v      v
; parameters: m
; body: (cond ((eq? m 'withdraw) withdraw)
;             ((eq? m 'deposit) deposit)
;             (else (error ("Unknown request -- MAKE-ACCOUNT"
;                           m))))
((acc 'deposit) 40)
; The environment where dispatch was evaluated is E1. The deposit procedure is looked up in E1.
; Evaluating deposit sets up E2. Since the "new frame has as its enclosing environment the environment part
; of the procedure object being applied.", E2 points to E1. 
;
;            +-------------------------------------------------------------------------------------------+
;    Global-->                                                                                           |
;    Env     | acc: +                                                                                    |
;            |      |                                                                                    |
;            |      |                                                                                    |
;            |      |                                                                                    |
;            |      |                                                                                    |
;            +---------------------^---------------------------------------------------------------------+
;                   |              |
;                   |        +-----+----------+
;                   |   E1+-->                <------------------------------------------------------+
;                   |        | balance: 90    |                                                      |
;                   |        | withdraw: +---------------------+                              +------+-----+
;                   |        | deposit: +------------+         |                         E2+-->            |
;                   |        | dispatch: |    |      |         v                              | amount: 40 |
;                   |        |           |    |      |   parameters: amount                   |            |
;                   |        +-----^---------++      |   body: (begin (set! balance ...)      |            |
;                   v              |     |   ^       v                                        +------------+
; +---------------+(|)+------------+     v   |   parameters: amount                           call to deposit
; |      +-----------------------------+(|)+-+   body: (set! balance ...)
; v      v
;parameters: m
;body: (cond ((eq? m 'withdraw) withdraw)
;            ((eq? m 'deposit) deposit)
;            (else (error ("Unknown request -- MAKE-ACCOUNT"
;                          m))))
;
; In E2, amount is bound to 40. The body of deposit is evaluated, finding balance in E1 and setting it to 
; the sum of it and the amount. Balance is returned from E2, which is returned from E1, and to the global
; environment.
; Pretty much the same thing happens for withdraw outside of what it does with balance. Presumably
; E2 is forgotten between each evaluation. As such, evaluating:
(define acc2 (make-account 100))
;Sets up a new E2 identical to E1 in structure, where balance is bound to 100. Only code is shared between them.

