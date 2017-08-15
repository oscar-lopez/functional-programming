#lang r5rs

;;; Task 1, b)

; We used R5RS in class, so we have to implement our own
; version of `accumulate`. In more advanced languages, we
; could have used `foldr` instead of `accumulate`.

(define (accumulate proc init lst)
  (if (null? lst)
      init
      (proc (car lst)
            (accumulate proc init (cdr lst)))))

(define (multi-list-ref lst idxs)
  (accumulate (lambda (i acc) (cons (list-ref lst i) acc))
              '()
              idxs))

; tests

(multi-list-ref '(a b c d e f g h i) '())
(multi-list-ref '(a b c d e f g h i) '(8))
(multi-list-ref '(a b c d e f g h i) '(3 6 7))
