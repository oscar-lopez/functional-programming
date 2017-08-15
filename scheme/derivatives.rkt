#lang r5rs

;;; Task 2, c)

; this is the part relevant to the exercise:

(define (make-sin x)
  (cond ((number? x) (sin x))
        (else (list 'sin x))))

(define (sin? x)
  (and (pair? x) (eq? (car x) 'sin)))

(define (make-cos x)
  (cond ((number? x) (cos x))
        (else (list 'cos x))))

(define (cos? x)
  (and (pair? x) (eq? (car x) 'cos)))

(define (fun-arg s)
  (cadr s))

(define (deriv exp var)
  (cond
    ((number? exp) 0)
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
    ((sin? exp)
     (make-product (deriv (fun-arg exp) var) ; chain rule
                   (make-cos (fun-arg exp))))
    ((cos? exp)
     (make-product (deriv (fun-arg exp) var) ; chain rule
                   (make-product -1 (make-sin (fun-arg exp)))))
    (else
     (error "unknown expression type -- deriv" exp))))

; rest of the code:

(define (error msg . exps)
  (display msg)
  (display " ")
  (for-each (lambda (x) (display x) (display " ")) exps)
  (newline))

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

; tests

(deriv '(sin x) 'x)
(deriv '(cos x) 'x)
(deriv '(sin (* 2 x)) 'x)
(deriv '(cos (+ (* x x) (* 2 x))) 'x)
