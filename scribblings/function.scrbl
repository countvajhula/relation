#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[relation/function
                    relation/type
                    data/maybe
					(only-in data/collection sequence?)
                    (rename-in racket (compose b:compose)
                                      (curry b:curry)
                                      (curryr b:curryr)
                                      (conjoin b:conjoin)
                                      (disjoin b:disjoin)
                                      (negate b:negate)
									  (sequence? b:sequence?))
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
                                           (only-in racket/list range)
                                           (only-in racket/math sqr)
                                           racket/generator
                                           racket/stream))))

@title{Functional Primitives}

@defmodule[relation/function]

Elementary types and utilities to simplify the use and manipulation of functions.

This module provides general-purpose utilities to support programming in the @hyperlink["https://en.wikipedia.org/wiki/Functional_programming"]{functional style}. As part of its operation, this module defines and provides a "rich" @racket[function] type intended as a drop-in alternative to built-in Racket functions. This function type is usually no different from using normal functions, but as a higher-level entity, it provides greater visibility of the make-up of the function, allowing more flexibility in customizing the nature of composition, supporting natural semantics when used with standard sequence utilities, and more seamless use of currying.

@section[#:tag "function:types"]{Types}

@defstruct[function ([components list?]
                     [composer monoid?]
                     [side symbol?]
                     [left-args list?]
                     [right-args list?]
                     [kw-args hash?])
                    #:omit-constructor]{
  A type that represents any procedure, whether elementary or composed. It is inherently @hyperlink["https://en.wikipedia.org/wiki/Currying"]{curried}, meaning that partially supplying arguments results in a new function parametrized by these already-provided arguments.
@itemlist[
@item{@racket[components] - A list of functions that comprise this one.}
@item{@racket[composer] - The definition of composition for this function. By default (when constructed using @racket[make-function]), this is the usual function composition, i.e. @racketlink[b:compose]{@racket[compose]} together with @racket[values] as the identity.}
@item{@racket[side] - The side on which the function is curried.}
@item{@racket[left-args] - The arguments that parametrize this function on the left (e.g. passed in by left-currying).}
@item{@racket[right-args] - The arguments that parametrize this function on the right (e.g. passed in by right-currying).}
@item{@racket[kw-args] - The keyword arguments that parametrize (i.e. have already been passed to) this function.}]
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
  @defproc[(make-function [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)]
                          [#:curry-on side symbol? 'left]
                          [g procedure?]
                          ...)
           function?]
  @defproc[(f [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)]
              [#:curry-on side symbol? 'left]
              [g procedure?]
              ...)
           function?]
  @defproc[(make-threading-function [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)]
                                    [#:curry-on side symbol? 'left]
                                    [g procedure?]
                                    ...)
           function?]
  @defproc[(f> [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)]
               [#:curry-on side symbol? 'left]
               [g procedure?]
               ...)
           function?]
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

@section[#:tag "function:syntax"]{Syntax}

@deftogether[(
  @defform[(lambda/function kw-formals body ...)]
  @defform[(lambda/f kw-formals body ...)]
  @defform[(λ/f kw-formals body ...)]
)]{
  Identical to @racket[lambda] except that it produces a @racket[function] rather than a primitive Racket function. @racket[lambda/f] and @racket[λ/f] are aliases for @racket[lambda/function].
}

@deftogether[(
  @defform[(define/function (id kw-formals) body ...)]
  @defform[(define/f kw-formals body ...)]
)]{
  Identical to the function form of @racket[define] except that it produces a @racket[function] rather than a primitive Racket function. @racket[define/f] is an alias for @racket[define/function].
}

@section[#:tag "function:utilities"]{Utilities}

@deftogether[(
  @defproc[(function-cons [v procedure?] [w function?])
           function?]
  @defproc[(function-null [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)]
                          [#:curry-on side symbol? 'left])
           function?]
  )]{
 Constructors for the @racket[function] type analogous to @racket[cons] and @racket[null] for lists. @racket[function-null] also serves as the identity value for composition.

@examples[
    #:eval eval-for-docs
    (function-cons add1 (f ->number))
    ((function-cons add1 (function-null)) 3)
  ]
}

@defproc[(function-arguments [g function?]
                             ...)
         arguments?]{

 Returns an @racketlink[arguments]{arguments} structure representing the arguments that parameterize (i.e. have already been passed to) the function @racket[g].

@examples[
    #:eval eval-for-docs
    (function-arguments (curry + 1 2 3))
    (function-arguments (curry = #:key string-upcase "apple"))
  ]
}

@defproc[(apply/steps [g function?]
                      [v any/c] ... [lst list?]
                      [#:<kw> kw-arg any/c] ...)
         sequence?]{

 Similar to @racket[apply], but yields a sequence corresponding to the values at each stage of application of the function @racket[g].

@examples[
    #:eval eval-for-docs
    (->list (apply/steps (f add1 sub1 add1) (list 3)))
    (->list (apply/steps (f> ->number add1 ->string) (list "1")))
  ]
}

@defproc[(compose [g function?]
                  ...)
         function?]{

 Analogous to @racketlink[b:compose]{@racket[compose]}, but yields a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")]. This is simply an alias for @racket[f].

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

 Analogous to @racketlink[b:curry]{@racket[curry]} and @racketlink[b:curryr]{@racket[curryr]}, but these yield a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")]. Since @racketlink[function]{functions} are inherently curried, explicitly invoking curry is usually not necessary, but can be useful in cases where evaluation needs to be delayed until additional arguments are received. An explicit call to curry will not immediately evaluate to a result even if sufficient arguments have been provided for the invocation to produce a result.

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

 Analogous to @racketlink[b:conjoin]{@racket[conjoin]}, this yields a @racket[function] whose composition method is @racketlink[b:conjoin]{@racket[conjoin]} rather than @racketlink[b:compose]{@racket[compose]}. @racket[&&] is provided as a convenient alias, following the convention in @other-doc['(lib "algebraic/scribblings/algebraic.scrbl")].

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

 Analogous to @racketlink[b:disjoin]{@racket[disjoin]}, this yields a @racket[function] whose composition method is @racketlink[b:disjoin]{@racket[disjoin]} rather than @racketlink[b:compose]{@racket[compose]}. @racket[||] is provided as a convenient alias, following the convention in @other-doc['(lib "algebraic/scribblings/algebraic.scrbl")].

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
 @defproc[(negate [g procedure?])
          function?]
 @defproc[(!! [g procedure?])
          function?]
 )]{

 Analogous to @racketlink[b:negate]{@racket[negate]}, this yields a @racket[function] whose result is the boolean negation of the result of applying @racket[g].

@examples[
    #:eval eval-for-docs
    (!! positive?)
    ((!! positive?) -5)
    ((!! positive?) 5)
  ]
}

@defproc[(unthunk [g procedure?]
                  [v any/c]
                  ...)
         procedure?]{

 Converts a procedure accepting no arguments to one accepting an arbitrary number of arguments (which are all ignored upon invocation). In other words, this converts a @racket[thunk] into a @racket[thunk*].

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
  @defproc[(true. [v any])
           boolean?]
  @defproc[(false. [v any])
           boolean?]
  )]{

  @racket[true.] is an agreeable function that always returns @racket[#t], while @racket[false.] is a contrarian that always returns @racket[#f]. Both accept an arbitrary number of arguments (disregarding all of them).

@examples[
    #:eval eval-for-docs
	(true.)
	(true. 3 1 #:key 'hi)
	(false.)
	(false. 3 1 #:key 'hi)
  ]
}

@defproc[(arg [n exact-nonnegative-integer?])
         procedure?]{

 Produces a function whose value is simply its @racket[n]th argument.

@examples[
    #:eval eval-for-docs
    ((arg 0) "hi" "there")
    ((arg 2) "hi" "there" 'abc 'pqr)
    ((arg 3) -2 -1 0 1 2 3)
    (apply (arg 3) (range 10))
    (regexp-replace* #rx"\\[\\[(cat|dog)\\]\\]"
                     "The [[cat]] and the [[dog]] in the hat."
                     (arg 1))
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

 "Lifts" a function operating on ordinary values to a function operating on a functor (for instance, a list of such values) in the natural way. This is a thin wrapper around @racketlink[f:map]{@racket[map]}, and may lend clarity in cases where you want to derive such a function but not necessarily apply it immediately.

@examples[
    #:eval eval-for-docs
    (define list-add1 (lift add1))
    (->list (list-add1 (list 1 2 3)))
    (->list ((lift ->string) (list 1 2 3)))
    ((lift add1) (just 3))
  ]
}

@defproc[(pack [g procedure?]
               [v any/c]
               ...)
         procedure?]{

 "Pack" the provided arguments into a list and map them individually under @racket[g]. While @racket[map] allows a function operating on individual arguments to operate on such arguments provided as a list, @racket[pack] analogously allows the function to operate on such arguments provided directly as multiple arguments.

@examples[
    #:eval eval-for-docs
    (pack sqr 1 2 3 4)
    (pack ->string 1 2 3)
  ]
}
