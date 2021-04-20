#lang racket/base

(require racket/hash
         mischief/shorthand
         syntax/parse/define
         (only-in kw-utils/kw-hash
                  apply/kw-hash)
         data/maybe
         (for-syntax racket/base))

(require relation/function/types
         relation/function/composition
         "util.rkt")

(provide lambda/function
         lambda/f
         λ/f
         define/function
         define/f
         lambda.
         λ.
         app)

(define-syntax-rule (lambda/function kw-formals body ...)
  (f (lambda kw-formals body ...)))

(define-alias lambda/f lambda/function)

(define-alias λ/f lambda/function)

(define-simple-macro (define/function (id:id kw-formals ... . rest-args)
                       body ...)
  (define id
    (lambda/function (kw-formals ... . rest-args)
                     body ...)))

(define-alias define/f define/function)

(define-simple-macro (lambda. v ...
                              (~or* (~datum ->) (~datum →))
                              body:expr ...)
  (lambda/f (v ...)
            body ...))

(define-alias λ. lambda.)

(define-syntax-parser app-positional-parser
  [(_) #'null]
  [(_ k:keyword _ vs ...)
   #'(app-positional-parser vs ...)]
  [(_ (~datum _) vs ...)
   #'(append (list nothing)
             (app-positional-parser vs ...))]
  [(_ v vs ...)
   #'(append (list (just v))
             (app-positional-parser vs ...))])

(define-syntax-parser app-keyword-parser
  [(_) #'(hash)]
  [(_ k:keyword (~datum _) vs ...)
   #'(hash-union (hash 'k nothing)
                 (app-keyword-parser vs ...))]
  [(_ k:keyword v:expr vs ...)
   #'(hash-union (hash 'k (just v))
                 (app-keyword-parser vs ...))]
  [(_ v vs ...)
   #'(app-keyword-parser vs ...)])

(define-syntax-parser app
  [(_ func:expr vs ...)
   #'(apply/kw-hash partial/template
                    (app-keyword-parser vs ...)
                    func
                    (app-positional-parser vs ...))])
