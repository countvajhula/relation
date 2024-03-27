#lang racket/base

(provide make-eval-for-docs)

(require racket/sandbox
         scribble/example)

(define (make-eval-for-docs . exprs)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
    (apply make-base-eval
           '(require relation)
           '(define (sqr x)
              (* x x))
           exprs)))
