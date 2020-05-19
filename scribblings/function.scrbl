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
                    arguments
                    (prefix-in f: data/functor)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require data/maybe
                                           arguments
                                           relation
                                           racket/set
                                           racket/generator
                                           racket/stream))))

@title{Functional Primitives}

@defmodule[relation/function]

Elementary types and utilities to simplify the use and manipulation of functions.

This module provides a @racket[function] type intended as a drop-in alternative to built-in Racket functions. This function type is usually no different from using normal functions, but as a higher-level entity, it provides greater visibility of the make-up of the function, allowing more flexibility in customizing the nature of composition, supporting natural semantics when used with standard sequence utilities, and more seamless use of currying. In addition, several general-purpose utilities are provided to support programming in the @hyperlink["https://en.wikipedia.org/wiki/Functional_programming"]{functional style}.

@section[#:tag "function:types"]{Types}

@defstruct[function ([components list?]
                     [composer monoid?]
                     [side symbol?]
                     [args arguments?])
                    #:omit-constructor]{
  The elementary type that represents any procedure, whether elementary or composed. It is inherently @hyperlink["https://en.wikipedia.org/wiki/Currying"]{curried}, meaning that partially supplying arguments results in a new function parametrized by these already-provided arguments.
@itemlist[
@item{@racket[components] - A list of functions that comprise this one.}
@item{@racket[composer] - The definition of composition for this function. By default (when constructed using @racket[make-function]), this is the usual function composition, i.e. @racketlink[b:compose]{compose} together with @racketlink[values]{values} as the identity.}
@item{@racket[side] - The side on which the function is curried.}
@item{@racket[args] - The arguments that parametrize (i.e. have already been passed to) this function.}]
}

@defstruct[monoid ([f (-> procedure? procedure? procedure?)]
                   [id procedure?])
                  #:omit-constructor]{
 A composer of functions, generalizing "normal" function composition to support any definition of composition. Any suitable notion of function composition (and hence instances of this @racket[monoid] type) must include:

 @itemlist[
   @item{@racketid[f] - A @hyperlink["https://en.wikipedia.org/wiki/Higher-order_function"]{higher-order} @hyperlink["https://en.wikipedia.org/wiki/Closure_(mathematics)"]{closed} @hyperlink["https://en.wikipedia.org/wiki/Binary_function"]{binary function}, i.e. a function taking in two functions and producing a single one}
   @item{@racket[id] - An @hyperlink["https://en.wikipedia.org/wiki/Identity_element"]{identity} function appropriate for the composition.}]
}

@deftogether[(
  @defproc[(make-function [g procedure?]
                          ...)
           any/c]
  @defproc[(f [g procedure?]
              ...)
           any/c]
  @defproc[(make-threading-function [g procedure?]
                                    ...)
           any/c]
  @defproc[(f> [g procedure?]
               ...)
           any/c]
  )]{
  A constructor for creating functions from other functions. @racket[f] functions compose right-to-left (the default), while @racket[f>] functions compose left-to-right (like @other-doc['(lib "scribblings/threading.scrbl")]), which some consider more intuitive. @racket[f] is an alias for the more verbose @racket[make-function], and likewise, @racket[f>] is an alias for @racket[make-threading-function].

  @examples[
      #:eval eval-for-docs
      (f add1)
      (f add1 ->number)
      ((f ->string add1 ->number) "12")
      ((f> ->number add1 ->string) "12")
      (define (str-append x y z) (string-append x y z))
      ((f str-append) "hello")
      ((((f str-append) "hello") "there") "friend")
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

@defproc[(if-f [pred (-> any/c boolean?)]
               [f procedure?]
               [g procedure?])
         procedure?]{

 Analogous to @racket[if], checks the predicate @racket[pred] against an input value and applies either @racket[f] or @racket[g] to it depending on the result.

@examples[
    #:eval eval-for-docs
    ((if-f positive? add1 sub1) 3)
    (map (if-f positive? add1 sub1) (list 3 -3))
  ]
}

@deftogether[(
  @defproc[(flip [g procedure?])
           procedure?]
  @defproc[(flip$ [g procedure?])
           procedure?]
  @defproc[(flip* [g procedure?])
           procedure?])]{

 @racket[flip] yields a function identical to the one passed in, but with the first two argument positions swapped, @racket[flip$] passes the first argument in the last argument position (leaving other arguments in the original relative positions), while @racket[flip*] reverses the entire list of arguments.

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

 Analogous to @racketlink[b:compose]{compose}, but yields a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")]. This is simply an alias for @racket[f].

@examples[
    #:eval eval-for-docs
    (compose add1 ->string)
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

 Analogous to @racketlink[b:curry]{curry} and @racketlink[b:curryr]{curryr}, but these yield a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")]. Since @racketlink[function]{functions} are inherently curried, explicitly invoking curry is usually not necessary, but can be useful in cases where evaluation needs to be delayed until additional arguments are received. An explicit call to curry will not immediately evaluate to a result even if sufficient arguments have been provided for the invocation to produce a result.

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

 Analogous to @racketlink[b:conjoin]{conjoin}, this yields a @racket[function] whose composition method is @racketlink[b:conjoin]{conjoin} rather than @racketlink[b:compose]{compose}. @racket[&&] is provided as a convenient alias, following the convention in @other-doc['(lib "algebraic/scribblings/algebraic.scrbl")].

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

 Analogous to @racketlink[b:disjoin]{disjoin}, this yields a @racket[function] whose composition method is @racketlink[b:disjoin]{disjoin} rather than @racketlink[b:compose]{compose}. @racket[||] is provided as a convenient alias, following the convention in @other-doc['(lib "algebraic/scribblings/algebraic.scrbl")].

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
