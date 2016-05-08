#lang planet neil/sicp

; Eval

(define (mc-eval exp env)
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
  ((cond? exp) (mc-eval (cond->if exp) env))
  ((application? exp)
    (mc-apply (mc-eval (operator exp) env)
      (list-of-values (operands exp) env)))
  (else
    (error "Unknown expression type -- EVAL" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; (mc-eval 3 '()) => 3
; (mc-eval "Hello world!" '()) => "Hello world!"

(define (variable? exp)
  (symbol? exp))

; (variable? sample) => #t

; Environments
(define the-empty-environment '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

; Frames
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; Extending environment
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Lookup variable value
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; (define sample-env (extend-environment '(a b) '(3 4) the-empty-environment))
; (lookup-variable-value 'a sample-env) => 3
; (lookup-variable-value 'b sample-env) => 4
; (lookup-variable-value 'c sample-env) => Unbound variable c

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; (quoted? '(quote (some text here))) => #t
; (mc-eval '(quote (some text here)) the-empty-environment) => (mcons 'some (mcons 'text (mcons 'here '())))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; (define sample-env (extend-environment '(a b) '(3 4) the-empty-environment))
; (set-variable-value! 'a 6)
; (lookup-variable-value 'a sample-env) => 6

; Assignments

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

; (define sample-env (extend-environment '(a b) '(3 4) the-empty-environment))
; (eval-assignment '(set! a 6) sample-env)
; (lookup-variable-value 'a sample-env) => 6

; Definitions

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

; (define exp '(define (var parameter1 ... parametern) body))
; (symbol? (cadr exp)) => #f
; (caadr exp) => 'var

; (define exp '(define var (lambda (parameter1 ... parametern) body)))
; (symbol? (cadr exp)) => #t
; (cadr exp) => 'var

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

; IF Statement

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; Because we didn't implement whole mc-eval procedure, we can test with basic condition.
; (eval-if '(if "hello" "john" "doe") the-empty-environment) => "john"

; Lambda

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

; (mc-eval '(lambda (a b c) (5)) the-empty-environment) =>
;   (mcons 'procedure (mcons (mcons 'a (mcons 'b (mcons 'c '()))) (mcons (mcons (mcons 5 '()) '()) (mcons '() '()))))

; Evaluating Sequences

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; Conditions

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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

; Procedure Application

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
        (list-of-values (rest-operands exps) env))))

; Apply

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ;((compound-procedure? procedure)
        ; (eval-sequence
        ;   (procedure-body procedure)
        ;   (extend-environment
        ;     (procedure-parameters procedure)
        ;     arguments
        ;     (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; (mc-eval (+ 4 5) the-empty-environment) => 9