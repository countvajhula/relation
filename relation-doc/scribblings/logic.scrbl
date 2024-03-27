#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         "eval.rkt"
         @for-label[relation/logic
                    (only-in relation/composition fold)
                    racket/undefined
                    racket]]

@(define eval-for-docs
  (make-eval-for-docs '(require (except-in data/collection
                                           append
                                           index-of
                                           foldl
                                           foldl/steps)
                                racket/undefined
                                racket/set
                                racket/stream)))

@title{Logical Relations}

@defmodule[relation/logic]

Logical primitives and predicates.

@defproc[(undefined? [v any/c]
                     ...)
         any/c]{

 A predicate to check whether a value is @racket[eq?] to @racket[undefined]. The value @racket[undefined] could be used as a placeholder default value for a function argument, in lieu of the Lisp convention of using @racket[#f], especially in cases where @racket[#f] could be a legitimate value for the argument, precluding its use as a placeholder.

@examples[
    #:eval eval-for-docs
    (undefined? #f)
    (undefined? (void))
    (undefined? undefined)
  ]
}

@deftogether[(
 @defproc[(orf [v any/c]
               ...)
          any/c]
 @defproc[(any? [vs sequence?])
          any/c]
    )]{

 Similar to @racket[or] but a function rather than a macro, so that it can be used in functional combinators such as @racket[fold].
 @racket[any?] is a predicate to check if any of a sequence of values are truthy, equivalent to @racket[(apply orf vs)].

@examples[
    #:eval eval-for-docs
    (orf #f #t #t)
    (orf #f #f #f)
    (orf 1 2 3)
    (any? '(#f #t #t))
    (any? '(#f #f #f))
    (any? '(1 2 3))
  ]
}

@deftogether[(
 @defproc[(andf [v any/c]
                ...)
          any/c]
 @defproc[(all? [vs sequence?])
          any/c]
    )]{

 Similar to @racket[and] but a function rather than a macro, so that it can be used in functional combinators such as @racket[fold].
 @racket[all?] is a predicate to check if all of a sequence of values are truthy, equivalent to @racket[(apply andf vs)].

@examples[
    #:eval eval-for-docs
    (andf #f #t #t)
    (andf #t #t #t)
    (andf 1 2 3)
    (all? '(#f #t #t))
    (all? '(#t #t #t))
    (all? '(1 2 3))
  ]
}

@defproc[(none? [v sequence?])
         any/c]{

 Equivalent to @racket[(compose not any?)], a predicate to check if none of the inputs are truthy.

@examples[
    #:eval eval-for-docs
    (none? '(#f #t #t))
    (none? '(#f #f #f))
    (none? '(1 2 3))
  ]
}

@defform[(:= expr ...)]{
  A convenient alias for @racket[define].
}

@defform[(=! expr ...)]{
  A convenient alias for @racket[set!].
}
