#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         "eval.rkt"
         @for-label[relation/function
                    relation/type
                    (only-in relation/equivalence member? =)
                    data/maybe
                    (only-in data/collection sequence?)
                    (rename-in racket (compose b:compose)
                                      (= b:=)
                                      (curry b:curry)
                                      (curryr b:curryr)
                                      (conjoin b:conjoin)
                                      (disjoin b:disjoin)
                                      (negate b:negate)
                                      (procedure? b:procedure?)
                                      (sequence? b:sequence?))
                    (only-in racket/generator sequence->generator)
                    (prefix-in b: racket/function)
                    arguments
                    (prefix-in f: data/functor)]]

@(define eval-for-docs
  (make-eval-for-docs '(require data/maybe
                                arguments
                                racket/set
                                (only-in racket/list range)
                                (only-in racket/math sqr)
                                racket/generator
                                racket/stream)))

@title{Functional Primitives}

@defmodule[relation/function]

Elementary types and utilities to simplify the use and manipulation of functions.

This module provides general-purpose utilities to support programming in the @hyperlink["https://en.wikipedia.org/wiki/Functional_programming"]{functional style}. As part of its operation, this module defines and provides a suite of "rich" @racket[function] types that interoperate seamlessly with, and may be used as an alternative to, Racket's built-in @seclink["procedures" #:doc '(lib "scribblings/reference/reference.scrbl")]{procedure} type. These function types are usually no different from normal functions, but as higher-level entities, they provide greater visibility of the make-up of the function, allowing more flexibility in customizing the nature of composition, supporting natural semantics when used with standard sequence utilities, and more seamless use of currying and partial application.

@table-of-contents[]

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

@deftogether[(
  @defform[(lambda. kw-formals ... -> body ...)]
  @defform[(λ. kw-formals ... -> body ...)]
)]{
  A lightweight way to define anonymous functions (lambdas) inspired by @hyperlink["https://wiki.haskell.org/Anonymous_function"]{Haskell's syntax} for lambdas. Equivalent to @racket[lambda/f] except that it does not support a @seclink["rest-args" #:doc '(lib "scribblings/guide/guide.scrbl")]{rest argument}, so it is best suited for simple cases like inline lambdas, rather than more complex cases such as a @racket[prop:procedure] specification. In any case, if a rest argument is needed, use @racket[lambda/f] directly.

 Either @racket[->] or @racket[→] may be used as the syntactic separator between the arguments and the body of the function.

@examples[
    #:eval eval-for-docs
    ((λ. x -> (sqr x)) 5)
    (map ((λ. x y -> (expt x y)) 2) (range 10))
    ((λ. -> 10))
    ((λ. x y -> (+ x y)) 5 10)
    ((λ. x y #:key [key #f] -> (= #:key key x y)) 5 "5")
    ((λ. x y #:key [key #f] -> (= #:key key x y)) #:key ->string 5 "5")
  ]
}

@defform[(app fn template-args ...)]{
  Syntactic sugar on the @racket[partial/template] interface, inspired by and greatly resembling @other-doc['(lib "fancy-app/main.scrbl")], this enables applying a function to arguments with reference to a template specified in advance. The template indicates the expected arguments and their positions or keywords.

@examples[
    #:eval eval-for-docs
    (app + 2 _)
    ((app + 2 _) 3)
    (map (app * 2 _) (list 1 2 3))
    ((app string-append _ "-" _) "seam" "less")
    (app = #:key string-upcase "apple" _)
    ((app = #:key string-upcase _ "apple") "APPLE")
    (eval:error ((app = #:key _ "apple" _) "APPLE"))
    ((app = #:key _ "apple" _) #:key string-upcase "APPLE")
  ]
}

@section[#:tag "function:representation"]{Representation}

 The printed representation of a @racket[function] has some features worthy of note. Let's look at an example.

@examples[
    #:eval eval-for-docs
    #:label #f
    (f add1 sqr)
  ]

 The first thing to note is that the printed representation is almost itself valid code to reproduce the function it represents. A prominent maxim of programming in the functional style is to write complex functions in terms of small, simple functions that can be composed together. The transparency of this representation is intended to support this habit, by enabling the makeup of such functions, whether simple or complex, to be easily scrutinized and manipulated. Specific clues encoded in the representation are as follows:
 @codeblock{(λ (args ···) ...)}
 In general, the arguments portion of the representation indicates the @tech{application scheme}. Here, it indicates that the function is @emph{left-curried} (the default), while @codeblock{(λ (··· args) ...)} indicates that it is @emph{right-curried} (the @racket[···] indicates where fresh arguments will be placed in relation to the existing arguments). If arguments have been supplied on both sides, either via currying or a @racketlink[template-function]{template}, the argument position(s) between the already-supplied arguments will be indicated by @racket[···] (for curried functions), or @racket[_] (for templates), or @racket[__] (for partially applied, uncurried, functions).
 @codeblock{(.. fn ...)} indicates that the method of composition is the usual one, i.e. @racket[compose],
 @codeblock{(&& fn ...)} means the method of composition is @racket[conjoin],
 @codeblock{(|| fn ...)} means @racket[disjoin], and
 @codeblock{(?? fn ...)} indicates that the method of composition is not a standard one but a custom @racket[monoid]. Note that in the case of composed functions, the order of composition is depicted @emph{left-to-right}, i.e. the leftmost function is the leading one in the composition and is applied first, and the rightmost one is applied last.

@examples[
    #:eval eval-for-docs
    #:label "More examples:"
	(f add1 sqr)
    ((f expt) 2)
    (partial expt 2)
    (curry = #:key string-upcase "apple")
	(curryr member? (list 1 2 3))
	(curryr (curry string-append "ichi") "san")
	(app string-append "ichi" _ "san" _ "go")
	(&& positive? odd?)
	(|| positive? odd?)
	(f #:compose-with (monoid (λ (f g) g) values) add1 sub1)
  ]


@section[#:tag "function:utilities"]{Utilities}

@defproc[(apply/steps [g function?]
                      [v any/c] ... [lst list?]
                      [#:<kw> kw-arg any/c] ...)
         sequence?]{

 Similar to @racket[apply], but yields a sequence corresponding to the values at each stage of application of the function @racket[g].

@examples[
    #:eval eval-for-docs
    (->list (apply/steps (f add1 sub1 sqr) (list 3)))
    (->list (apply/steps (f #:thread? #t ->number add1 ->string) (list "1")))
  ]
}

@defproc[(compose [g procedure?]
                  ...)
         function?]{

 Analogous to @racketlink[b:compose]{@racket[compose]}, but yields a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")].

In general, the composition is performed "naively" by simply wrapping the component functions with a new @racket[function]. In the case where none of the component functions are wrapped with an application scheme, however, and where their @racketlink[monoid]{composition methods} are the same, the functions are composed "at the same level." Further study of the interaction between application schemes and composition may suggest some simplification rules here, but that is left for future consideration.

@examples[
    #:eval eval-for-docs
    (compose ->string +)
    (compose ->string (f +))
    (compose ->string (curry + 2))
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

 Similar to @racket[compose] and equivalent to @racketlink[b:conjoin]{@racket[conjoin]}, this yields a @racket[function] whose composition method is @racketlink[b:conjoin]{@racket[conjoin]} rather than @racketlink[b:compose]{@racket[compose]}, that is, it composes predicates such that it evaluates to true when @emph{all} components evaluate to true and not otherwise. @racket[&&] is provided as a convenient alias, following the convention in @other-doc['(lib "algebraic/scribblings/algebraic.scrbl")].

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

 Similar to @racket[compose] and equivalent to @racketlink[b:disjoin]{@racket[disjoin]}, this yields a @racket[function] whose composition method is @racketlink[b:disjoin]{@racket[disjoin]} rather than @racketlink[b:compose]{@racket[compose]}, that is, it composes predicates such that it evaluates to true when @emph{any} component evaluates to true. @racket[||] is provided as a convenient alias, following the convention in @other-doc['(lib "algebraic/scribblings/algebraic.scrbl")].

 @bold{N.B.}: It turns out that Racket considers the symbol @racket[||] to be the @seclink["default-readtable-dispatch" #:doc '(lib "scribblings/reference/reference.scrbl")]{empty symbol}, because of the special handling of @racket[\|] by the reader. This may cause problems in certain (perhaps rare) cases -- for instance, if you do a @racket[symbol->string] conversion, you may expect to find @racket["||"] but you would find @racket[""] instead. In cases where you are doing any kind of parsing or serialization of your code, or if you are using these in macros, it would be advisable to just use the written form of the function instead, i.e. @racket[disjoin] rather than @racket[||], to steer clear of possible issues here.

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

 Analogous to @racketlink[b:curry]{@racket[curry]} and @racketlink[b:curryr]{@racket[curryr]}, but these yield a @racket[function] rather than a primitive Racket @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")]. Since @racketlink[function]{functions} are curried by default, explicitly invoking curry is usually not necessary, but can be useful in cases where evaluation needs to be delayed until additional arguments are received. An explicit call to curry will not immediately evaluate to a result even if sufficient arguments have been provided for the invocation to produce a result.

@examples[
    #:eval eval-for-docs
    (curry + 2)
    (curry + 2 3)
    ((curryr < 5) 3)
    (curry (curryr (curry string-append "ichi") "san") "ni")
    ((curryr (curry string-append "ichi" "-") "-" "san") "ni")
  ]
}

@defproc[(uncurry [g procedure?])
         function?]{

 Convert a curried function @racket[g] accepting single arguments in succession to an equivalent one accepting an arbitrary number of arguments at once. This is typically not needed since both @racket[curry] as well as Racket's built-in currying interfaces support partial application with an arbitrary number of arguments, but it can be useful with naively curried functions not created using one of these interfaces.

@examples[
    #:eval eval-for-docs
    (define (curried-add-3 x)
      (λ (y)
        (λ (z)
          (+ x y z))))
    (eval:error (curried-add-3 1 4 7))
    ((uncurry curried-add-3) 1 4 7)
  ]
}

@deftogether[(
@defproc[(partial [g procedure?] [v any/c] ...)
         function?]
@defproc[(partialr [g procedure?] [v any/c] ...)
         function?]
 )]{

 Partially apply the function @racket[g] using the provided arguments. The result is a function with a flat set of these pre-supplied arguments which must be invoked with all of the remaining expected arguments when the time comes, i.e. it is @emph{not} curried. @racket[partial] supplies the arguments on the left, while @racket[partialr] supplies them on the right.

@examples[
    #:eval eval-for-docs
    (partial + 2)
    ((partial + 2) 3 4)
    (partialr string-append "c")
    ((partialr string-append "c") "a" "b")
  ]
}

@defproc[(partial/template [g procedure?] [v maybe/c] ...)
         function?]{

 Partially apply the function @racket[g] using the specified argument template. This template takes the form of a series of @tech[#:doc '(lib "scribblings/data/functional.scrbl")]{optional values}, provided directly, either as positional or keyword arguments. The result is a function that expects precisely those arguments that are indicated as "missing" in the template. Note that this function is @emph{not} curried. Typically this would be used via the convenient @racket[app] syntax, rather than directly.

@examples[
    #:eval eval-for-docs
    (partial/template = #:key (just string-upcase) (just "apple") nothing)
    ((partial/template = #:key (just string-upcase) nothing (just "apple")) "APPLE")
    ((partial/template = #:key nothing (just "apple") nothing) #:key string-upcase "APPLE")
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

@defproc[(call [g procedure?]
               [v any/c]
               ...)
         procedure?]{

 Reprovided from @hyperlink["https://docs.racket-lang.org/mischief/function.html#%28def._%28%28lib._mischief%2Ffunction..rkt%29._call%29%29"]{@racket[call]}. This simply makes standard function invocation available as a function, for use in cases where we cannot directly (i.e. syntactically) invoke the function.
 This function is in some respects similar to the @hyperlink["https://typeclasses.com/featured/dollar"]{@racket[$] operator} in Haskell.

@examples[
    #:eval eval-for-docs
    (call + 1 2 3 4)
    (call = #:key string-upcase "Apple" "APPLE")
    (map call (list add1 sqr) (list 2 3))
  ]
}

@deftogether[(
@defproc[(pack [g procedure?]
               [v any/c]
               ...)
         any/c]
@defproc[(pack-map [g procedure?]
                   [v any/c]
                   ...)
         list?]
)]{

 @racket[pack] packs the provided arguments into a list and gives that list to @racket[g] as an argument. @racket[pack-map] packs the provided arguments into a list and maps them individually under @racket[g].

While @racket[apply] allows a function operating on provided arguments to operate on such arguments provided as a list, @racket[pack] enables the opposite, allowing a function expecting a list to operate on multiple arguments instead. While @racket[map] allows a function operating on individual arguments to operate on such arguments provided as a list, @racket[pack-map] analogously allows the function to operate on such arguments provided directly as multiple arguments.

@examples[
    #:eval eval-for-docs
    (pack (curry apply +) 1 2 3 4)
    (pack length "apple" 23 'banana)
    (pack-map sqr 1 2 3 4)
    (pack-map ->string 1 2 3)
  ]
}

@deftogether[(
@defproc[(map-values [g procedure?]
                     [v any/c]
                     ...)
         any]
@defproc[(filter-values [g procedure?]
                        [v any/c]
                        ...)
         any]
)]{

 Similar to @racket[map] and @racket[filter] but these accept and return multiple @racket[values] instead of lists.

@examples[
    #:eval eval-for-docs
	(map-values add1 3 5)
	(filter-values positive? 1 -2 3)
  ]
}

@defproc[(unwrap [v list?])
         any]{

 Unwrap the contents of a list, returning them as @racket[values].

@examples[
    #:eval eval-for-docs
    (unwrap (list 3))
    (unwrap (list 1 2 3))
  ]
}

@section[#:tag "function:types"]{Types}

This module defines an interface, @racket[gen:procedure], to encode the idea of a function. Racket's built-in procedures answer to this interface, as do the "rich" @racket[function] types provided by this module. This rich type is usable as a drop-in alternative to built-in Racket functions, but in addition, provides various high-level conveniences.

@subsection{Interface}

@defthing[gen:procedure any/c]{

 A @tech/reference{generic interface} that represents any object which behaves like a function. The built-in @seclink["procedures" "procedure" #:doc '(lib "scribblings/reference/reference.scrbl")] type has an implementation for @racket[gen:procedure].

@defproc[(procedure? [v any/c])
         boolean?]{

 Predicate to check if a value is a procedure. This is in practice identical to Racket's built-in @racketlink[b:procedure?]{@racket[procedure?]}.

@examples[
    #:eval eval-for-docs
    (procedure? 3)
    (procedure? add1)
    (procedure? (f add1))
  ]
}

 To implement this interface for custom types, the following methods need to be implemented, unless the type already contains a @racket[prop:procedure] specification (meaning it counts as a built-in procedure), and more specific handling is not needed.

@defproc[(keywords [proc procedure?])
          (values (listof keyword?) (or/c (listof keyword?) #f))]{

 A generic version of @racket[procedure-keywords]. This function takes a single argument and returns information about the keyword arguments accepted by the procedure. The return values should take the same form as those of @racket[procedure-keywords]. The argument is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

@defproc[(arity [proc procedure?])
          normalized-arity?]{

 A generic version of @racket[procedure-arity]. This function takes a single argument and returns information about the arity of the arguments accepted by the procedure. The return value should take the same form as that of @racket[procedure-arity]. The argument is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

@defproc[(procedure-apply [proc procedure?]
                          [args arguments?])
          any]{

 This function specifies the rule for application of the function. It is expected to actually invoke the function on the provided arguments and return the result.

 This function takes two arguments. The first is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type). The second will be an @tech[#:doc '(lib "arguments/main.scrbl") #:key "arguments-struct"]{arguments structure} representing the arguments provided to the function.
 }

}

@subsection{Functions and Composition}

@defstruct[function ()
                    #:omit-constructor]{
  An "abstract" base type that represents any function, whether atomic or composed. All of the rich function types provided by this module are subtypes of this type. It is @hyperlink["https://en.wikipedia.org/wiki/Currying"]{curried} by default, meaning that partially supplying arguments results in a new function parametrized by these already-provided arguments.
}

  If you'd like to define a custom rich function type, it must implement @racket[gen:procedure] as well as use @racket[function] as its base type.

@defstruct[base-composed-function ([composer monoid?])
                                  #:omit-constructor]{
  An "abstract" base type that represents a composed function. This is a subtype of @racket[function].
@itemlist[
@item{@racket[composer] - The definition of composition for this function. By default (when constructed using @racket[make-function]), this is the usual function composition, i.e. @racketlink[b:compose]{@racket[compose]} together with @racket[values] as the identity.}]
}

@defstruct[composed-function ([composer monoid?]
                              [components list?])
                             #:omit-constructor]{
  A type that represents a composed function. This is a subtype of @racket[base-composed-function] and therefore includes a composer.

@itemlist[
@item{@racket[components] - A list of functions that comprise this one.}
]
}

@defstruct[power-function ([composer monoid?]
                           [f procedure?]
                           [n number?])
                          #:omit-constructor]{
  A type that represents a function composed with itself a certain number of times, i.e. a "power" of the function under the indicated composition method. This is a subtype of @racket[base-composed-function] and therefore includes a composer.

@itemlist[
@item{@racket[f] - The underlying function wrapped by the instance.}
@item{@racket[n] - The exponent, or number of times @racket[f] is composed with itself.}]
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
  @defproc[(function-cons [v procedure?] [w base-composed-function?])
           base-composed-function?]
  @defproc[(function-null [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)])
           composed-function?]
  )]{
 Constructors for the @racket[base-composed-function] type analogous to @racket[cons] and @racket[null] for lists. @racket[function-null] also serves as the identity value for composition.

@examples[
    #:eval eval-for-docs
    (function-cons add1 (f sqr ->number))
    ((function-cons add1 (function-null)) 3)
  ]
}

@deftogether[(
  @defproc[(make-function [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)]
                          [#:thread? thread? boolean? #f]
                          [g procedure?]
                          ...)
           function?]
  @defproc[(f [#:compose-with composer monoid? (monoid #, @racketlink[b:compose]{@racket[compose]} values)]
              [#:thread? thread? boolean? #f]
              [g procedure?]
              ...)
           function?]
  )]{
  A variadic constructor for creating functions from other functions. The argument @racket[thread?] configures whether the resulting function composes right-to-left (the default) or left-to-right (like @other-doc['(lib "scribblings/threading.scrbl")]). In either case, the print representation of the resulting function always denotes the order of composition from left to right (i.e. corresponding to the threading direction). @racket[f] is an alias for the more verbose @racket[make-function].

  @examples[
      #:eval eval-for-docs
      (f add1)
      (f add1 ->number)
      (f add1 add1 add1)
      ((f ->string add1 ->number) "12")
      ((f #:thread? #t ->number add1 ->string) "12")
      (define (str-append x y z) (string-append x y z))
      ((f str-append) "hello")
      ((((f str-append) "hello") "there") "friend")
    ]
}

@subsection{Function Application}

 An @deftech{application scheme} is in essence simply a function that calls another function. It represents a definition of function application, entailing how arguments are to be ordered and compiled, what arguments are expected and whether they may be passed in incrementally, and what happens when the underlying function is actually invoked.

 The default application scheme is curried partial application, meaning the function takes an arbitrary number of positional and keyword arguments at a time and evaluates to a result when sufficient arguments have been provided, or to a new function accepting more arguments otherwise. Other schemes provided include partial application without currying, and template-based partial application (resembling the scheme in @other-doc['(lib "fancy-app/main.scrbl")]). As application schemes are themselves functions, they implement @racket[gen:procedure] as well, in order to gracefully "pass through" the rich functionality provided by @racket[function] types to those underlying functions, which would otherwise be rendered opaque by the wrapping functions.

 Application schemes compose naturally, so that, for example, a function could expect arguments to match a @racketlink[template-function]{template}, and could receive those arguments incrementally via @racketlink[curried-function]{curried partial application}. The examples below illustrate this.

@defthing[gen:application-scheme any/c]{

 A @tech/reference{generic interface} representing an @tech{application scheme}. Any application scheme must also implement @racket[gen:procedure].

@examples[
    #:eval eval-for-docs
    ((partial + 1) 2)
    ((curry expt 2) 5)
    ((curryr expt 2) 5)
    (app string-append _ ", " _ ", " _ " " "and " _ ".")
    (eval:error ((app string-append _ ", " _ ", " _ " " "and " _ ".") "parsley" "sage"))
    (curryr (app string-append _ ", " _ ", " _ " " "and " _ ".") "thyme")
    (curry (curryr (app string-append _ ", " _ ", " _ " " "and " _ ".") "thyme") "parsley" "sage")
    ((curry (curryr (app string-append _ ", " _ ", " _ " " "and " _ ".") "thyme") "parsley" "sage") "rosemary")
  ]
}

@defproc[(application-scheme? [v any/c])
         boolean?]{

 Predicate to check if a value is an application scheme.

@examples[
    #:eval eval-for-docs
    (application-scheme? (curry append 'left (list 1 2 3) (list 4 5) (hash '#:key number->string)))
    (application-scheme? (app string-append _ "-" _))
  ]

 To define custom application schemes, the following methods need to be implemented.

 @defproc[(pass [application-scheme application-scheme?]
                [args arguments?])
          application-scheme?]{

 Incorporate fresh @racket[args] into the @racket[application-scheme]. This defines what happens when a function with the given application scheme is applied to fresh arguments. The result of this function is expected to be an updated application scheme.
 }

 @defproc[(flat-arguments [application-scheme application-scheme?])
          arguments?]{

 Produce a flat @tech[#:doc '(lib "arguments/main.scrbl") #:key "arguments-struct"]{arguments structure} representing the arguments that will be passed in a single invocation of the underlying function. The application scheme may compile the arguments in whatever manner it sees fit; the produced arguments structure represents the result of its operation.

@examples[
    #:eval eval-for-docs
    (flat-arguments (curry + 1 2 3))
    (flat-arguments (curry = #:key string-upcase "apple"))
    (flat-arguments (curry (curryr (curry string-append "hello") "friend") "there"))
    (flat-arguments (app + 3 _))
  ]

 }

 @defproc[(unwrap-application [application-scheme application-scheme?])
          procedure?]{

 Return the underlying function that is to be applied by the scheme.
 }

}

@defstruct[curried-function ([f procedure?]
                             [chirality (one-of/c 'left 'right)]
                             [left list?]
                             [right list?]
                             [kw hash?])
                             #:omit-constructor]{

 An @tech{application scheme} representing a curried function, including the arguments that parametrize (i.e. have already been supplied to) the function. This includes all arguments that have been supplied by either left- or right-currying. The @racket[chirality] indicates whether the function is left- or right-curried.

 @itemlist[
    @item{@racket[f] - The function to be applied.}
    @item{@racket[chirality] - The direction (@racket[left]-to-right or @racket[right]-to-left) in which provided arguments will be incorporated.}
    @item{@racket[left] - The positional arguments that parametrize this function on the left (e.g. passed in by left-currying).}
    @item{@racket[right] - The positional arguments that parametrize this function on the right (e.g. passed in by right-currying).}
    @item{@racket[kw] - The keyword arguments that parametrize this function.}
   ]
}

@defstruct[partial-function ([f procedure?]
                             [chirality (one-of/c 'left 'right)]
                             [left list?]
                             [right list?]
                             [kw hash?])
                             #:omit-constructor]{

 An @tech{application scheme} representing a partially applied function (not to be confused with the mathematical notion of partial function, which is a function defined on a subset of its domain), including the arguments that parametrize (i.e. have already been supplied to) the function. This includes all arguments that have been supplied by either left- or right-partial application. The @racket[chirality] indicates the side on which fresh arguments will be incorporated.

 @itemlist[
    @item{@racket[f] - The function to be applied.}
    @item{@racket[chirality] - The direction (@racket[left]-to-right or @racket[right]-to-left) in which provided arguments will be incorporated.}
    @item{@racket[left] - The positional arguments that parametrize this function on the left (e.g. passed in by left-currying).}
    @item{@racket[right] - The positional arguments that parametrize this function on the right (e.g. passed in by right-currying).}
    @item{@racket[kw] - The keyword arguments that parametrize this function.}
   ]
}

@defstruct[template-function ([f procedure?]
                              [pos list?]
                              [kw hash?])
                             #:omit-constructor]{
 An @tech{application scheme} encoding a template expressing the expected arguments -- whether positional or keyword -- to a function. The values of positional or keyword arguments are expected to be @tech[#:doc '(lib "scribblings/data/functional.scrbl")]{optional values}. Typically, template-based partial application would be used via the @racket[app] macro, so that there is no need to muck about with optional values in normal usage.

 @itemlist[
    @item{@racket[f] - The function to be applied.}
    @item{@racket[pos] - The positional arguments that parametrize this function, which may be actual values or blanks expected to be filled at invocation time.}
    @item{@racket[kw] - The keyword arguments that parametrize this function, which may be actual values or blanks expected to be filled at invocation time.}
   ]
}
