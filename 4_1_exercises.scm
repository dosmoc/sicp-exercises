;4.1 The Metacircular Evaluator

; The metacircular evaluator is essentially a Scheme formulation of 
; the environment model of evaluation described in section 3.2. 

;1. To evaluate a combination (a compound expression other than a special form), 
;   evaluate the subexpressions and then apply the value of the operator subexpression to the values of the operand subexpressions.

;2. To apply a compound procedure to a set of arguments, evaluate the body of the procedure in a new environment.
;   To construct this environment, extend the environment part of the procedure object by a frame in which
;   the formal parameters of the procedure are bound to the arguments to which the procedure is applied.

; Procedures define the syntax of expressions
; data abstraction to make the evaluator independent of the representation of the language
; for example: assignment? provides a test for an assignment
; abstract selectors: assigment-variable and assignment-value

;4.1.1 The Core of the Evaluator

;Eval takes an expression and and environment
;  classifies the expression
;  directs its evaluation
;  case analysis works on abstract syntax
;
; Primitive expressions
;   - for self evaluating expressions, eval returns the expression itself
;   - eval looks up variables in the environment to find values
;
; Special forms
;   - quotes returns the expression
;   - assigment recursively calls eval to compute the new value. The env must be modfied to change or create the binding
;   - if requires special processing
;   - lambda must package parameters and body with the environment of the evaluation
;   - begin evaluates expressions in order
;   - case is transformed into nested if
;
; Combinations 
;
;   - eval recursively evaluats the operator and operands, then passes them to apply

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; Apply takes a procedure and a list of arguments
; it classifies procedures into primitives and compound procedures
; compound procedures sequentially evaluated
; the enviroment for the body is constructed by extending the 
; base environment with a frame binding params to the args

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;Procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;Conditionals

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;We see that if relies on the implementation language
; "The metacircular representation of truth might not be the same as that of the underlying Scheme."

;Sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; Exercise 4.1
; Write list-of-values that evaluates left to right
; then right to left

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


;left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((the-first-operand (eval (first-operand exps) env)))
        (cons the-first-operand (list-of-values (rest-operands exps) env)))))

;or maybe:
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((the-operands (cons nil nil)))
        (begin
          (set-car! the-operands (eval (first-operand exps) env)))
          (set-cdr! (list-of-values (rest-operands exps) env))
          the-operands)))

;right to left is just
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((the-operands (cons nil nil)))
        (begin
          (set-cdr! (list-of-values (rest-operands exps) env))
          (set-car! the-operands (eval (first-operand exps) env))
          the-operands))))
 
;right to left without set
;left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-of-operands (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) rest-of-operands))))


;4.1.2  Representing Expressions

;The only self-evaluating items are numbers and strings:
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;Variables are represented by symbols:
(define (variable? exp) (symbol? exp))

;Quotations have the form (quote <text-of-quotation>):
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;Quoted? is defined in terms of the procedure tagged-list?,
; which identifies lists beginning with a designated symbol:

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;Assignments have the form (set! <var> <value>)

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; Definitions have the form (define <var> <value>)
; or the form
; (define (<var> <parameter1> ... <parameterₙ>)
;   <body>)

; (define <var>
;   (lambda (<parameter1> ... <parametern>)
;     <body>))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) 
      (caadr exp))) ;the syntactic sugar 
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      ;this is where the syntatic sugar gets re-written as a lambda
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;Lambda expressions are lists that begin with the symbol lambda:
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;We also provide a constructor for lambda expressions, which is used by definition-value, above:
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Conditionals begin with if and have a predicate, a consequent, and an (optional) alternative.
; If the expression has no alternative part, we provide false as the alternative.
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;We also provide a constructor for if expressions, to be used by cond->if to transform cond expressions into if expressions:

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Begin packages a sequence of expressions into a single expression.
; We include syntax operations on begin expressions to extract the actual
; sequence from the begin expression, as well as selectors that return the 
; first expression and the rest of the expressions in the sequence.

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;come back later and explain note 11

; We also include a constructor sequence->exp (for use by cond->if) that transforms
; a sequence into a single expression, using begin if necessary:

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; A procedure application is any compound expression that is not one of the above 
; expression types. The car of the expression is the operator, and the cdr is the list of operands:

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;Derived expressions
;I forgot (or never learned) that cond can evaluate multiple expressions without a begin

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;Exercise 4.2.

;a.
; The classification of an expression in eval is dependant on the ordering of the cond
; clause. Louis's evaluator would see (define x 3) as an application, and would attempt to
; eval 'define and treat x and 3 as the operands

;b. to fix it

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;Exercise 4.3

;the old one
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;assuming we have the local table representation from 3.3.3
(define (type exp) (car exp))
(define (expression-body exp) (cdr exp))

;we just ignore the environment here
;alternatively, we could make this as a special
;case in the cond clause
(put 'eval 'quote  (lambda (exp _) (text-of-quotation exp)))
(put 'eval 'set    eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if     eval-if)
;these ones get special definitions because exp is 
;transformed
(put 'eval 'lambda 
     (lambda (exp env)
        (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                         env)))
(put 'eval 'begin  
     (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond   
     (lambda (exp env) (eval (cond->if exp) env)))

;first attempt
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else ((get 'eval (type exp)) (expression-body exp) 
                                      env))))

;unfortunately, there's no way to tell what an application is,
;since an application can have any symbol at the beginning of 
;the list 
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;this might work... get returns the function or else returns false
        ;so goes to next case. We have to get the proper operation again
        ;to apply it
        ((get 'eval (type exp)) ((get 'eval (type exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;illustration of the behavior of cond
(cond ((lambda (x) x) 'was-true) (else 'was-false))
;was-true

;maybe more illustrative:
(define (some-function y) y)
(cond (some-function some-function) (else 'was-false))
;Value 33: #[compound-procedure 33 some-function]

;Exercise 4.3

;let me think...
;
; for and, you eval the first expression. If its true, you eval the next expression. If its not true, then return false
; if all the expresions are true, return the value of the last one
; if no expressions return true?

(and)
;Value: #t
;huh

; for or, you eval the first expression. If its true, return true. If not true, eval the next expression. If not
; of the expressions or there are no expressions, return false

; I was wondering if we had to worry about normal vs applicative order here, but since we're evaling each in 
; turn, it's not an issue

; it seems that and can be implemented as nested ifs, and or can be implemented as cond (which gets turned into
; nested ifs)

(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exp env)
  (define (and-loop exp)
    (let ((current-val (eval (car exp) env))
          (next-exp    (cdr exp)))
      (cond ((and (true? current-val) (null? next-exp)) current-val)
            ((false? current-val) 'false)
            (else (and-loop exp)))))
  
  (if (null? (cdr exp))
      'true
      (and-loop (cdr exp))))

(define (or? exp) (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (or-loop exp)
    (let ((current-val (eval (car exp) env))
          (next-exp    (cdr exp)))
      (cond ((true? current-val) current-val)
            ((null? next-exp) 'false)
            (else (or-loop exp)))))
  
  (if (null? (cdr exp))
      'false
      (or-loop (cdr exp))))

;Read Scheme's definition of and / or using syntax rules, and it inspires way to simplify this
(define (eval-and conjuncts env)
  (cond ((null? conjuncts) 'true)
        ;if all values are true, and = the last expression. We don't need to test if its true
        ;because we're on the last expression and evaling it will offer a boolean (true or false), or
        ;itself, which is considered true if its not explicitly 'false 
        ((null? (cdr conjuncts)) (eval (car conjuncts) env)) 
        ;if it's false, false, otherwise keep on evaling
        (else (if (eval (car conjuncts)) (eval-and (cdr conjuncts) env) 'false)))) 

(define (eval-or disjuncts env)
  (cond ((null? disjuncts) 'false)
        ;if all values so far have been false, we can just return the value of the last expression
        ;with the same reasoning as and
        ((null? (cdr conjuncts)) (eval (car disjuncts) env)) 
        ;if its true, return the value
        (else (if (eval (car disjuncts) env) (eval (car disjuncts) env) (eval-or (cdr disjuncts) env))))) 

;or working with the whole expression
(define (eval-and exp env)
  
  (define (test-and conjuncts)
   (cond ((null? conjuncts) 'true)
        ;if all values are true, and = the last expression. We don't need to test if its true
        ;because we're on the last expression and evaling it will offer a boolean (true or false), or
        ;itself, which is considered true if its not explicitly 'false 
        ((null? (cdr conjuncts)) (eval (car conjuncts) env)) 
        ;if it's false, false, otherwise keep on evaling
        (else (if (eval (car conjuncts)) (test-and (cdr conjuncts) env) 'false))))
  
  (test-and (cdr exp)))

(define (eval-or exp env)
  (define (test-or disjuncts)
   (cond ((null? disjuncts) 'false)
        ;if all values so far have been false, we can just return the value of the last expression
        ;with the same reasoning as and
        ((null? (cdr conjuncts)) (eval (car disjuncts) env)) 
        ;if its true, return the value
        (else (if (eval (car disjuncts) env) (eval (car disjuncts) env) (eval-or (cdr disjuncts) env)))))
  
  (test-or (cdr exp))) 


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;Exercise 4.5

;check if the clause is arrow-cond
(define (arrow-clause? clause)
  (eq? '=> (cadr clause)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (cond-recipient clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (arrow-clause? first)
                (make-if (cond-predicate first)
                         (list (cond-recipient first) (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))


(define example-clause '((assoc 'b '((a 1) (b 2))) => cadr))
(cond-predicate example-clause)
;(assoc (quote b) (quote ((a 1) (b 2))))
(cond-recipient example-clause)
;cadr

;lets check the expansion
(define example-cond
  '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
         (else false)))

(cond->if example-cond)
;(if (assoc (quote b) (quote ((a 1) (b 2)))) (cadr (assoc (quote b) (quote ((a 1) (b 2))))) false)

;but maybe we want something more like so <test> is evaluated only once12t:
'(let ((test-val (assoc (quote b) (quote ((a 1) (b 2))))))
   (if test-val (cadr test-val) false))
;write something to do that later

;Exercise 4.6

(define test-vars '((x 5) (y 6)))

(define (binding-vars bindings)
  (define (accumulate-vars vars bindings)
    (if (null? bindings)
        vars
        (cons (caar bindings) (accumulate-vars vars (cdr bindings)))))
  
  (accumulate-vars '() bindings))

(caar test-vars)

(binding-vars test-vars)
;(x y)
;there should be a test to see that the names are unique
;possibly in make-procudure

(define (binding-exprs bindings)
  (define (accumulate-exprs exps bindings)
    (if (null? bindings)
        exps
        (cons (cadar bindings) (accumulate-exprs exps (cdr bindings)))))
  
  (accumulate-exprs '() bindings))

(binding-exprs test-vars)
;(5 6)

(define test-let '(let ((x 1) (y 2)) 5))

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(binding-vars (let-bindings test-let))

(define (let->combination exp)
  (cons (make-lambda (binding-vars (let-bindings exp))   
                     (let-body exp))
        (binding-exprs (let-bindings exp))))

(let->combination test-let)
;((lambda (y x) 5) 2 1)

(define (let? expr) (tagged-list? expr 'let))

;4.7

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

;should expand to:

(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 4)))
      (* x z))))

(define test-let* 
  '(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
     (* x z)))

(define (let*->nested-lets exp)
  (define (accumulate-lets lets bindings body)
    (if (null? bindings)
        body
        (cons 'let (list (list (car bindings)) 
                         (accumulate-lets lets (cdr bindings) body)))))
  
  (accumulate-lets '() (cadr exp) (caddr exp)))

(caddr test-let*)

(let*->nested-lets test-let*)

;(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
;which is:

(let ((x 3)) 
  (let ((y (+ x 2))) 
    (let ((z (+ x y 5))) 
      (* x z))))

;the second part of the question makes me think: If we have a let*? test,
;then we are essentially doing:
;(eval '(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z)))) env)

(define (let*? exp) (tagged-list? exp 'let*))

;the eval would then expand the let expression recursively to lambdas
;if there is a let? test in the cond 

;4.8

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

;var is bound within body to a procedure whos body is body and whos parameters are teh variables in bindings

'(let ((fib-iter (lambda (a b count) 
                  (if (= count 0)
                      b
                      (fib-iter (+ a b) a (- count 1))))))
   (fib-iter 1 0 n))

;this doesn't quite work because the fib-iter in the lambda doesn't know what the fib-iter is

;heres let->combination
(define (let->combination exp)
  (cons (make-lambda (binding-vars (let-bindings exp))   
                     (let-body exp))
        (binding-exprs (let-bindings exp))))

(let->combination test-let)
;((lambda (y x) 5) 2 1)

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

;maybe a transformation in thos
(define (fib n)
  (let ((fib-iter (lambda (a b count) 
                  (if (= count 0)
                      b
                      (fib-iter (+ a b) a (- count 1))))))
    (fib-iter 1 0 n)))

;doesn't work -- it doesn't know fib-iter is not bount when
;it is evaluated

;we probably want a tranformation into this:
'(let () 
  (define (fib-iter) 
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define test-named-let
  '(let fib-iter ((a 1)
                  (b 0)
                  (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

(define (named-let? expr) 
  (and (let? expr)
       (symbol? (cadr expr))))

(define (let-name expr)
  (cadr expr))

(define (named-let-bindings expr)
  (caddr expr))

(define (named-let-body expr)
  (cadddr expr))


(let-name test-named-let)

(named-let-bindings test-named-let)
; ((a 1) (b 0) (count n))

(named-let-body test-named-let)


(list
 'let '()
 (list 'define 
      (cons (let-name test-named-let) (binding-vars (named-let-bindings test-named-let)))
      (named-let-body test-named-let))
 (cons (let-name test-named-let) (binding-exprs (named-let-bindings test-named-let))))

(define (internal-define-let name bindings body)
  (list 'let '() 
        (list 'define
              (cons name (binding-vars bindings))
              body)
        (cons name (binding-exprs bindings))))

(define (named-let->let expr)
  (internal-define-let (let-name expr)
                       (named-let-bindings expr)
                       (named-let-body expr)))


(define (let->combination expr)
  (if (named-let? expr)
   ;this feels really goofy
   (let->combination (named-let->let expr))
   (cons (make-lambda (binding-vars (let-bindings expr))   
                      (let-body expr))
        (binding-exprs (let-bindings expr)))))

(let->combination test-named-let)

(let->combination '(let ((x 5)) (+ x x)))

(equal?
 (let->combination test-named-let)
 '((lambda () (define (fib-iter count b a) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))) (fib-iter n 0 1))))

(equal? 
  (let->combination '(let ((x 5)) (+ x x)))
  '((lambda (x) (+ x x)) 5))

;Section 4.1.6 on internal definitions might have a better solution:
(define (fib n)
  (let ((fib-iter '*unassigned*))
    (set! fib-iter
      (lambda (a b count) 
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1)))))
    (fib-iter 1 0 n)))

(fib 0)
;0
(fib 1)
;1
(fib 2)
;1
(fib 3)
;2
(fib 4)
;3

(define (internal-set-let name bindings body)
  (list 'let (list (list name (quote (quote *unassigned*))))
        (list 'set! name
              (make-lambda (binding-vars bindings)
                           body))
        (cons name (binding-exprs bindings))))

(internal-set-let 'fib-iter 
                  '((a 1) (b 0) (count n))
                  '((if (= count 0)
                        b
                        (fib-iter (+ a b) a (- count 1)))))

(define (named-let->let expr)
  (internal-set-let (let-name expr)
                    (named-let-bindings expr)
                    (list (named-let-body expr))))

(binding-vars (named-let-bindings test-named-let))

;((lambda (fib-iter) (set! fib-iter (lambda (count b a) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter n 0 1)) (quote *unassigned*))

(equal?
 (let->combination test-named-let)
 '((lambda (fib-iter) (set! fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n)) (quote *unassigned*)))

;Exercise 4.9

;let's implement do and while

;Guile documentationon do:

; syntax: do ((variable init [step]) …) (test expr …) body …
; Bind variables and evaluate body until test is true. 
; The return value is the last expr after test, if given. 

; (do ((i 1 (1+ i))
;      (p 3 (* 3 p)))
;     ((> i 4)
;      p)
;   (format #t "3**~s is ~s\n" i p))
; -|
; 3**1 is 3
; 3**2 is 9
; 3**3 is 27
; 3**4 is 81
; ⇒
; 789

(define test-do 
  '(do ((i 1 (1+ i))
        (p 3 (* 3 p)))
       ((> i 4) p)
     (format #t "3**~s is ~s\n" i p))
)

;what's the if test look like?
'(if (> i 4)
    (begin p)
    (begin (format #t "3**~s is ~s\n" i p)
           (do-it (1+ i) (* 3 p))))

;it's just a named let with some begins
'(let do-it ((i 1)
             (p 3))
     (if (> i 4)
         (begin p)
         (begin (format #t "3**~s is ~s\n" i p)
                (do-it (1+ i) (* 3 p)))))

; this is interesting...
; my thought process:
; what's the specification?
; looked it up in Guile
; there's a test in here, so let's write that part
; okay, so how do you get the iteration to work?
; its a let!

(define (step-exprs bindings)
  (define (accumulate-exprs exps bindings)
    (if (null? bindings)
        exps
        (cons (caddar bindings) (accumulate-exprs exps (cdr bindings)))))
  
  (accumulate-exprs '() bindings))

(step-exprs (cadr test-do))
;((1+ i) (* 3 p))

(define (do-bindings expr) (cadr expr))
(define (do-test expr) (caaddr expr))
(define (do-test-exprs expr) (cdaddr expr))
(define (do-body expr) (cdddr expr))

(do-test test-do)
;(> i 4)
(do-test-exprs test-do)
;(p)
(do-body test-do)
;((format #t "3**~s is ~s\n" i p))

;we don't even need  named let, just need to make  body:
(internal-set-let 'do-it 
                  (do-bindings test-do)
                  (list (list 'if (do-test test-do)
                        (cons 'begin (do-test-exprs test-do))
                        (cons 'begin (append (do-body test-do) 
                                             (list (cons 'do-it (step-exprs (cadr test-do)))))))))

(quote
(let ((do-it (quote *unassigned*))) 
  (set! do-it 
        (lambda (i p) 
          (if (> i 4) 
              (begin p) 
              (begin (format #t "3**~s is ~s\n" i p) (do-it (1+ i) (* 3 p)))))) 
  (do-it 1 3))  
)

;thus
(define (do->let expr)
  (internal-set-let 
    'do-it 
    (do-bindings expr)
    (list (list 'if (do-test expr)
          (cons 'begin (do-test-exprs expr))
          (cons 'begin (append (do-body expr) 
                               (list (cons 'do-it (step-exprs (cadr expr))))))))))