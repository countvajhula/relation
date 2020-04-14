#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[relation/function
                    (prefix-in b: racket)]]

@title{Functional Primitives}

@defmodule[relation/function]

Elementary types and utilities to simplify the use and manipulation of functions.

This module provides a @racket[function] type intended as a drop-in alternative to built-in Racket functions. The advantage of using it is that it implements some high-level generic interfaces that make it easy to treat functions as sequences (that is, for the purposes of composition order) and as monoids (for the purpose of composing them). In addition, several general-purpose elementary functional utilities are provided to make working with functions more convenient.

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require relation)
                                 '(require racket/set)
                                 '(require racket/generator)
                                 '(require racket/stream))))

@section[#:tag "function:types"]{Types}

@defthing[function struct?]{
 The elementary type that represents any procedure, whether elementary or composed. It is curried by default.
}

@deftogether[(
  @defproc[(make-function [g procedure?]
                          ...)
           any/c?]
  @defproc[(f [g procedure?]
              ...)
           any/c?]
  @defproc[(f> [g procedure?]
              ...)
           any/c?]
  )]{
  A constructor for creating functions from other functions. @racket[f] functions are left-curried (the default), while @racket[f>] functions are right-curried.

  @examples[
      #:eval eval-for-docs
      (f add1)
      (f add1 ->number)
      ((f ->string add1 ->number) "12")
      (define (str-append x y z) (string-append x y z))
      ((((f str-append) "hello") "there") "friend")
      ((((f> str-append) "hello") "there") "friend")
    ]
}

@defproc[(function-components [f function?])
         list?]{

  A list of functions that comprise the composite function @racket[f].

@examples[
    #:eval eval-for-docs
    (function-components (f add1 ->number))
  ]
}

@defproc[(function-side [f function?])
         symbol?]{

  The currying direction.

@examples[
    #:eval eval-for-docs
    (function-side (f add1 ->number))
    (function-side (f> add1 ->number))
  ]
}

@defproc[(function-args [f function?])
         arguments?]{

  Arguments that have already been supplied to the function.

@examples[
    #:eval eval-for-docs
    (define (str-append x y z) (string-append x y z))
    (function-args ((f str-append) "hello" "there"))
  ]
}

@defproc[(function? [v any/c?])
         boolean?]{

  A predicate to check if a value is a @racket[function].

@examples[
    #:eval eval-for-docs
    (function? (f add1 ->number))
    (function? add1)
  ]
}

@section[#:tag "function:utilities"]{Utilities}

@defproc[(unthunk [g procedure?]
                  ...)
         procedure?]{

 Converts a procedure accepting no arguments to one accepting an arbitrary number of arguments (which are all ignored upon invocation).

@examples[
    #:eval eval-for-docs
    (define gen (unthunk (sequence->generator '(1 2 3))))
    (gen "some")
    (gen 'ignored)
    (gen "arguments")
  ]
}

@defproc[(andf [v any/c?]
               ...)
         any/c?]{

 Similar to @racketlink[b:and]{and} but a function rather than a macro, so that it can be used in functional combinators such as @racket[fold].

@examples[
    #:eval eval-for-docs
    (andf #f #t #t)
    (andf #t #t #t)
    (andf 1 2 3)
  ]
}

@defproc[(orf [v any/c?]
              ...)
         any/c?]{

 Similar to @racketlink[b:or]{or} but a function rather than a macro, so that it can be used in functional combinators such as @racket[fold].

@examples[
    #:eval eval-for-docs
    (orf #f #t #t)
    (orf #f #f #f)
    (orf 1 2 3)
  ]
}

@defproc[(iff [pred (-> any/c boolean?)]
              [f procedure?]
              [g procedure?])
         procedure?]{

 Analogous to @racketlink[b:if]{if}, checks the predicate @racket[pred] against an input value and applies either @racket[f] or @racket[g] to it depending on the result.

@examples[
    #:eval eval-for-docs
    ((iff positive? add1 sub1) 3)
    ((iff positive? add1 sub1) -3)
  ]
}

@deftogether[(
  @defproc[(flip [f procedure?])
           procedure?]
  @defproc[(flip$ [f procedure?])
           procedure?]
  @defproc[(flip* [f procedure?])
           procedure?])]{

 @racket[flip] yields a function identical to the one passed in, but with the first two argument positions swapped, @racket[flip$] swaps the first and last argument positions, while @racket[flip*] reverses the entire list of arguments.

@examples[
    #:eval eval-for-docs
    ((flip string-append) "my" "hello" "friend")
    ((flip$ string-append) "friend" "hello" "my")
    ((flip* string-append) "friend" "my" "hello")
  ]
}

@deftogether[(
  @defproc[(function-cons [v function?] [w function?])
           function?]
  @defthing[function-null function?]
  )]{
 Constructors for the @racket[function] type analogous to @racketlink[b:cons]{cons} and @racketlink[b:null]{null} for lists. @racket[function-null] also serves as the identity value for composition.

@examples[
    #:eval eval-for-docs
    function-null
    (function-cons add1 (f ->number))
  ]
}

@defproc[(compose [f procedure?]
                  ...)
         function?]{

 Analogous to @racketlink[b:compose]{compose}, but yields a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")].

@examples[
    #:eval eval-for-docs
    (compose add1 ->string)
    (compose (f add1) (f ->string))
  ]
}

@deftogether[(
 @defproc[(curry [f procedure?]
                 [v unconstrained-domain->]
                 ...)
          function?]
 @defproc[(curryr [f procedure?]
                  [v unconstrained-domain->]
                  ...)
          function?]
 )]{

 Analogous to @racketlink[b:curry]{curry} and @racketlink[b:curryr]{curryr}, but these yield a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")]. Since @racketlink[function]{functions} are curried by default, explicitly invoking curry is usually not necessary, but can be useful in cases where evaluation needs to be delayed until additional arguments are received. An explicit call to curry will not immediately evaluate to a result even if sufficient arguments have been provided for the invocation to produce a result.

@examples[
    #:eval eval-for-docs
    (curry + 2)
    (curry + 2 3)
    ((curryr < 5) 3)
  ]
}
