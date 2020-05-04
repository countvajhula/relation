#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[relation/function
                    relation/transform
                    data/maybe
                    (rename-in racket (compose b:compose)
                                      (curry b:curry)
                                      (curryr b:curryr)
                                      (conjoin b:conjoin)
                                      (disjoin b:disjoin)
                                      (negate b:negate))
                    (only-in racket/generator sequence->generator)
                    (prefix-in b: racket/function)
                    (prefix-in f: data/functor)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require data/maybe)
                                 '(require relation)
                                 '(require racket/set)
                                 '(require racket/generator)
                                 '(require racket/stream))))

@title{Functional Primitives}

@defmodule[relation/function]

Elementary types and utilities to simplify the use and manipulation of functions.

This module provides a @racket[function] type intended as a drop-in alternative to built-in Racket functions. The advantage of using it is that it implements some high-level generic interfaces that make it easy to treat functions as sequences (that is, for the purposes of composition order) and as monoids (for the purpose of composing them). In addition, several general-purpose elementary functional utilities are provided to make working with functions more convenient.

@section[#:tag "function:types"]{Types}

@defthing[function struct?]{
 The elementary type that represents any procedure, whether elementary or composed. It is curried by default.
}

@deftogether[(
  @defproc[(make-function [g procedure?]
                          ...)
           any/c]
  @defproc[(f [g procedure?]
              ...)
           any/c]
  @defproc[(make-right-function [g procedure?]
                                ...)
           any/c]
  @defproc[(f> [g procedure?]
              ...)
           any/c]
  )]{
  A constructor for creating functions from other functions. @racket[f] functions are left-curried (the default), while @racket[f>] functions are right-curried.

  @examples[
      #:eval eval-for-docs
      (f add1)
      (f add1 ->number)
      ((f ->string add1 ->number) "12")
      (define (str-append x y z) (string-append x y z))
      ((f str-append) "hello")
      ((((f str-append) "hello") "there") "friend")
      ((((f> str-append) "hello") "there") "friend")
    ]
}

@defproc[(function-components [g function?])
         list?]{

  An accessor to get the list of functions that comprise the composite function @racket[g].

@examples[
    #:eval eval-for-docs
    (function-components (f add1 ->number))
  ]
}

@defproc[(function-composer [g function?])
         list?]{

  An accessor to get the function used for composing the component functions in @racket[g]. By default this is the usual function composition, @racketlink[b:compose]{compose}, but it could be any @hyperlink["https://en.wikipedia.org/wiki/Higher-order_function"]{higher-order} @hyperlink["https://en.wikipedia.org/wiki/Binary_function"]{binary function}, i.e. a function taking in two functions and producing a single one.

@examples[
    #:eval eval-for-docs
    (function-composer (f add1 ->number))
    (function-composer (conjoin positive? integer?))
    (function-composer (disjoin positive? integer?))
  ]
}

@defproc[(function-identity [g function?])
         list?]{

  An accessor to get the @hyperlink["https://en.wikipedia.org/wiki/Identity_element"]{identity function} to be used in the composition specified in @racket[g]. This should be specified in tandem with, rather than independently of, the @racket[composer] attribute, since any definition of composition must simultaneously specify both the composition procedure as well as the identity value for the composition (if any). By default the composition function @racketlink[b:compose]{compose} is used together with @racket[values] as the identity function, but it should be whatever function is appropriate as an identity for the chosen composition. For instance, for @racketlink[conjoin]{conjoining} functions, a function returning @racket[false] is the appropriate identity.

@examples[
    #:eval eval-for-docs
    (function-identity (f add1 ->number))
    (function-identity (conjoin positive? integer?))
    (function-identity (disjoin positive? integer?))
  ]
}

@defproc[(function-side [g function?])
         symbol?]{

  An accessor to get the side on which the function is curried.

@examples[
    #:eval eval-for-docs
    (function-side (f add1 ->number))
    (function-side (f> add1 ->number))
  ]
}

@defproc[(function-args [g function?])
         arguments?]{

  An accessor to get the arguments that have already been supplied to the function.

@examples[
    #:eval eval-for-docs
    (define (str-append x y z) (string-append x y z))
    (function-args ((f str-append) "hello" "there"))
  ]
}

@defproc[(function? [v any/c])
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
    (gen "arguments" 'a 'b 42)
  ]
}

@defproc[(iff [pred (-> any/c boolean?)]
              [f procedure?]
              [g procedure?])
         procedure?]{

 Analogous to @racket[if], checks the predicate @racket[pred] against an input value and applies either @racket[f] or @racket[g] to it depending on the result.

@examples[
    #:eval eval-for-docs
    ((iff positive? add1 sub1) 3)
    (map (iff positive? add1 sub1) (list 3 -3))
  ]
}

@deftogether[(
  @defproc[(flip [g procedure?])
           procedure?]
  @defproc[(flip$ [g procedure?])
           procedure?]
  @defproc[(flip* [g procedure?])
           procedure?])]{

 @racket[flip] yields a function identical to the one passed in, but with the first two argument positions swapped, @racket[flip$] swaps the first and last argument positions, while @racket[flip*] reverses the entire list of arguments.

@examples[
    #:eval eval-for-docs
    ((flip string-append) "my" "hello" "friend")
    ((flip$ string-append) "friend" "hello" "my")
    ((flip* string-append) "friend" "my" "hello")
  ]
}

@defproc[(lift [g procedure?])
         procedure?]{

 "Lifts" a function operating on ordinary values to a function operating on a functor (for instance, a list of such values) in the natural way. This is a thin wrapper around @racketlink[f:map]{map}, and may lend clarity in cases where you want to derive such a function but not necessarily apply it immediately.

@examples[
    #:eval eval-for-docs
    (define list-add1 (lift add1))
    (->list (list-add1 (list 1 2 3)))
    (->list ((lift ->string) (list 1 2 3)))
    ((lift add1) (just 3))
  ]
}

@deftogether[(
  @defproc[(function-cons [v function?] [w function?])
           function?]
  @defthing[function-null function?]
  )]{
 Constructors for the @racket[function] type analogous to @racket[cons] and @racket[null] for lists. @racket[function-null] also serves as the identity value for composition.

@examples[
    #:eval eval-for-docs
    function-null
    (function-cons add1 (f ->number))
  ]
}

@defproc[(apply/steps [g function?]
                      [v any/c] ... [lst list?]
                      [#:<kw> kw-arg any/c] ...)
         any]{

 Similar to @racket[apply], but yields a sequence corresponding to the values at each stage of application of the function @racket[g].

@examples[
    #:eval eval-for-docs
    (->list (apply/steps (f add1 sub1 add1) (list 3)))
    (->list (apply/steps (f ->string add1 ->number) (list "1")))
  ]
}

@defproc[(compose [g function?]
                  ...)
         function?]{

 Analogous to @racketlink[b:compose]{compose}, but yields a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")].

@examples[
    #:eval eval-for-docs
    (compose add1 ->string)
  ]
}

@defproc[(power [n integer?]
                [g function?])
         function?]{

 Composes a function with itself @racket[n] times.

@examples[
    #:eval eval-for-docs
    (power 2 add1)
    ((power 2 add1) 3)
    ((power 10 add1) 3)
  ]
}

@deftogether[(
 @defproc[(curry [g procedure?]
                 [v any/c]
                 ...)
          function?]
 @defproc[(curryr [g procedure?]
                  [v any/c]
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

@deftogether[(
 @defproc[(conjoin [g procedure?]
                   ...)
          function?]
 @defproc[(&& [g procedure?]
              ...)
          function?]
 )]{

 Analogous to @racketlink[b:conjoin]{conjoin}, this yields a @racket[function] whose composition method is @racketlink[b:conjoin]{conjoin} rather than @racketlink[b:compose]{compose}.

@examples[
    #:eval eval-for-docs
    (&& positive? integer?)
    ((&& positive? integer?) -5)
    ((&& positive? integer?) 5.3)
    ((&& positive? integer?) 5)
  ]
}

@deftogether[(
 @defproc[(disjoin [g procedure?]
                   ...)
          function?]
 @defproc[(|| [g procedure?]
              ...)
          function?]
 )]{

 Analogous to @racketlink[b:disjoin]{disjoin}, this yields a @racket[function] whose composition method is @racketlink[b:disjoin]{disjoin} rather than @racketlink[b:compose]{compose}.

@examples[
    #:eval eval-for-docs
    (|| positive? integer?)
    ((|| positive? integer?) -5)
    ((|| positive? integer?) 5.3)
    ((|| positive? integer?) 5)
    ((|| positive? integer?) -5.3)
  ]
}

@deftogether[(
 @defproc[(negate [g procedure?]
                   ...)
          function?]
 @defproc[(!! [g procedure?]
              ...)
          function?]
 )]{

 Analogous to @racketlink[b:negate]{negate}, this yields a @racket[function] whose result is the boolean negation of the result of applying @racket[g].

@examples[
    #:eval eval-for-docs
    (!! positive?)
    ((!! positive?) -5)
    ((!! positive?) 5)
  ]
}
