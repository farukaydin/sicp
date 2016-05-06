#lang planet neil/sicp

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
  ((variable? exp) (lookup-variable-value exp env))
  ((quoted? exp) (text-of-quotation exp))
;  ((assignment? exp) (eval-assignment exp env))
;  ((definition? exp) (eval-definition exp env))
;  ((if? exp) (eval-if exp env))
;  ((lambda? exp)
;    (make-procedure (lambda-parameters exp)
;      (lambda-body exp)
;      env))
;  ((begin? exp) 
;  (eval-sequence (begin-actions exp) env))
;  ((cond? exp) (mc-eval (cond->if exp) env))
;  ((application? exp)
;    (mc-apply (mc-eval (operator exp) env)
;      (list-of-values (operands exp) env)))
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



