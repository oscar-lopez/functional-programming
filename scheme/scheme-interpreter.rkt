#lang r5rs

;;; Task 3, a) & b)

;; 1.

; See the implementations below.

;; 2.

; No special type of runtime value is needed to represent philosophers,
; it's just syntactic sugar for ordinary procedures because the `philo`
; special form can be transformed into an application of the Y combinator.

; We need a special type of runtime value to represent dlambda, because
; it requires a different evaluation rule, given that the environment is
; extended in a distinct way, not available in ordinary procedures.

;; 3.

; The call to `philo->y-combinator` performs a computation: a syntactical
; transformation from the special form `philo` into a clever set of lambdas;
; after that the call to `eval2` just creates a standard procedure object.
; The rest of the magic (environment extension, procedure application, etc.)
; happens when the philospher is actually called, after all, it's now
; an ordinary procedure that gets executed.

; The evaluation of a dlambda is simple, we just need to capture its parameters
; and body into a new type of procedure object. The real magic happens when the
; procedure is applied, that's the moment when the current environment is extended
; giving rise to a dynamical scope.

(define (error msg . exps)
  (display msg)
  (display " ")
  (for-each (lambda (x) (display x) (display " ")) exps)
  (newline))

; part a) `philo` special form

(define (philo? exp) (tagged-list? exp 'philo))
(define (philo-parameters exp) (cadr exp))
(define (philo-body exp) (cddr exp))

(define (philo->y-combinator exp)
  `((lambda (proc)
      (proc proc))
    (lambda (proc)
      ((lambda (this)
         (lambda ,(philo-parameters exp) ,@(philo-body exp)))
       (lambda (arg)
         ((proc proc) arg))))))

; part b) `dlambda` special form

(define (dlambda? exp) (tagged-list? exp 'dlambda))
(define (dlambda-parameters exp) (cadr exp))
(define (dlambda-body exp) (cddr exp))

(define (make-dprocedure parameters body)
  (list 'dprocedure parameters body))
(define (dynamic-procedure? p)
  (tagged-list? p 'dprocedure))
(define (dprocedure-parameters p) (cadr p))
(define (dprocedure-body p) (caddr p))

; `eval2` modified to support both `philo` and `dlambda`

(define (eval2 exp env)
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
        ((dlambda? exp)
         (make-dprocedure (dlambda-parameters exp)
                          (dlambda-body exp)))
        ((philo? exp)
         (eval2 (philo->y-combinator exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval2 (cond->if exp) env))
        ((application? exp)
         (apply2 (eval2 (operator exp) env)
                 (list-of-values (operands exp) env)
                 env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval2 (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (apply2 procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        ((dynamic-procedure? procedure)
         (eval-sequence
          (dprocedure-body procedure)
          (extend-environment
           (dprocedure-parameters procedure)
           arguments
           env)))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (eval-if exp env)
  (if (true? (eval2 (if-predicate exp) env))
      (eval2 (if-consequent exp) env)
      (eval2 (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval2 (first-exp exps) env))
        (else (eval2 (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval2 (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval2 (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
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
      'false ; no else clause
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

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied"  vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'display display)
        (list 'newline newline)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true  #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt  ";;; M-Eval:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval2 input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     'procedure-environment))
      (display object)))

(driver-loop)
