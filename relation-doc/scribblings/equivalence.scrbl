#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         "eval.rkt"
         @for-label[relation/equivalence
                    relation/type
                    racket/generic
                    (except-in racket = equal? group-by drop length assoc)
                    (only-in racket (= b:=) (equal? b:equal?) (assoc b:assoc) (group-by b:group-by))
                    (only-in racket/function identity)
					(only-in rackjure/egal egal?)
                    (only-in data/collection drop length sequenceof)]]

@(define eval-for-docs
  (make-eval-for-docs '(require (except-in data/collection
                                                      append
                                                      index-of
                                                      foldl
                                                      foldl/steps)
                                           (only-in racket/function identity)
                                           racket/set
                                           racket/stream)))

@title{Equivalence Relations}

@defmodule[relation/equivalence]

A unified equality relation @racket[=] together with derived utilities for comparing data.

Traditionally, in Lisp, a distinction is made between object "identity" and object "equality." The former represents that two references indicate the @emph{same} object, while the latter means that the references indicate objects that are "indistinguishable," without necessarily asserting that they refer to a singular object.

Racket provides @racket[eq?] to check for object identity, but for equality, there are several interfaces to choose from: we have @racketlink[b:=]{=} (for numbers), @racket[eqv?] (for numbers and other types), and @racketlink[b:equal?]{equal?} (the broadest notion of equality covering most cases). None of these can be used exclusively to represent the idea of "equality" in practice, since cases exist where each is preferable to the others. For instance, @racket[=] reports that @racket[1] is equal to @racket[1.0] -- a behavior that is desirable in many cases -- whereas @racketlink[b:equal?]{equal?} sees them as unequal.

Yet, mathematically, any notion of equality may be represented using only (1) a minimally expressive elementary notion of equality and (2) a "projection" function mapping the inputs to a form amenable to elementary comparison. In Racket, this could be represented by the simple interface @racket[(= #:key ...)], where the projection function is provided via the @racket[key] argument.

The present module therefore provides such an interface, overriding the standard @racketlink[b:=]{=} operator to allow its use with any type, delegating to built-in equality checks to perform the most appropriate comparison depending on the type of the values being compared, leveraging the @racket[key] argument to express @hyperlink["https://en.wikipedia.org/wiki/Equivalence_relation"]{broader notions of equivalence} than simple equality -- since, indeed, any idea of equality presupposes a definition in relation to which two objects are indistinguishable.

The @racket[=] relation provided in this module is intended to express the notion of "equality" in all cases, but, at least for the moment, does not usually differentiate between mutable and immutable versions of a data type. For that, consider using @racket[egal?].

@section[#:tag "equivalence:interface"]{Interface}

@defthing[gen:comparable any/c]{

 A @tech/reference{generic interface} that represents any object that can be compared with other objects of the same type in terms of equivalence, that is, in cases where we seek to know, "are these values equal, for some definition of equality?" All built-in as well as @seclink["define-struct" #:doc '(lib "scribblings/guide/guide.scrbl")]{custom} types are @racket[gen:comparable]. For all custom types, the implementation defers to the built-in @racketlink[b:equal?]{equal?}.

 @examples[
    #:eval eval-for-docs
    (= 1 1)
    (= 1 2)
    (= 1 (void))
    (= "apple" "APPLE")
    (= #:key string-upcase "apple" "APPLE")
    (= #:key ->number "42.0" "42/1")
  ]

@defproc[(comparable? [v any/c])
         boolean?]{

 Predicate to check if a value is comparable via the generic equivalence operators @racket[=] and @racket[/=].

@examples[
    #:eval eval-for-docs
    (comparable? 3)
    (comparable? #\a)
    (comparable? "cherry")
    (comparable? (set))
    (comparable? (hash))
  ]
}

@deftogether[(
@defproc[(hash-code [v comparable?])
         fixnum?]
@defproc[(secondary-hash-code [v comparable?])
         fixnum?])]{

 Similar to @racket[equal-hash-code] and @racket[equal-secondary-hash-code], but these yield the hash code reported by the underlying operation used to perform the equality check. For example, for numbers, @racket[hash-code] is equivalent to the @racket[eqv-hash-code] of the @tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "inexact number"]{inexact} representation of the number, while for strings, it evaluates to the hash code reported by @racket[equal-hash-code]. Likewise, for symbols, it evaluates to the hash code reported by @racket[eq-hash-code] since @racket[eq?] is the check employed for symbol comparisons.

@examples[
    #:eval eval-for-docs
    (hash-code 3)
    (hash-code #\a)
    (hash-code 'abc)
    (hash-code "cherry")
  ]
}

 In order to implement this interface in custom types, all that is needed is to implement the @racket[gen:equal+hash] interface. @racket[gen:comparable] itself @emph{should not be implemented directly}, since there is never a case where both of these interfaces would need to be implemented. To avoid any possibility of conflicting notions of equality, @racket[gen:comparable] simply defers to the built-in @racketlink[b:equal?]{equal?} for the definition of equality for custom types.

 All Racket types are @racket[gen:comparable], so @racket[=] may be treated as a drop-in replacement for @racketlink[b:equal? "equal?"].

}

@section[#:tag "equivalence:utilities"]{Utilities}

 The following equivalence-related utilities are universally applicable, since all types are @racket[gen:comparable].

@defproc[(= [#:key key (-> comparable? comparable?) #f]
            [v comparable?] ...)
         boolean?]{

True if the v's are equal. This uses the most appropriate equality check for the type. For instance, it uses the built-in @racketlink[b:=]{=} operator for numeric data, and @racketlink[b:equal?]{equal?} for some other types such as @tech/reference{structures}. If a transformation is provided via the @racket[#:key] argument, then this transformation is applied to the input values first, prior to performing the equality check. For @seclink["Sequences" #:doc '(lib "scribblings/data/collection/collections.scrbl")]{sequences}, the types of the values are compared prior to recursively applying the equality relation on the contents.

@examples[
    #:eval eval-for-docs
    (= 1 1 1)
    (= 1 2)
    (= "apple" "apple" "apple")
    (= 3/2 1.5)
    (= (list 1 3/2 2.0) (list 1.0 1.5 2))
    (= (list 1 3/2 2.0) #(1.0 1.5 2))
    (= #:key string-upcase "apple" "Apple" "APPLE")
    (= #:key ->number "42.0" "42/1" "42")
    (= #:key ->number "42" "42.1")
    (= #:key even? 12 20)
    (= #:key odd? 12 20)
    (= #:key first (list 1.5 4 7) (list 3/2 2 3))
    (= #:key (~ even? ->number) "12" "20")
  ]
}

@deftogether[(@defproc[(/= [#:key key (-> comparable? comparable?) #f]
                           [v comparable?]
                           ...)
                       boolean?]
              @defproc[(≠ [#:key key (-> comparable? comparable?) #f]
                          [v comparable?]
                          ...)
                       boolean?]
              @defproc[(!= [#:key key (-> comparable? comparable?) #f]
                           [v comparable?]
                           ...)
                       boolean?])]{

 True if the v's are not equal. This is simply a negation of the generic @racket[=]. If a transformation is provided via the @racket[#:key] argument, then it is applied to the arguments prior to comparing them.

@examples[
    #:eval eval-for-docs
    (≠ 1 1 2)
    (≠ 1 1)
    (≠ "apple" "Apple")
    (≠ 3/2 1.5)
    (≠ #:key length "cherry" "banana" "avocado")
  ]
}

@deftogether[(@defproc[(group-by [key (-> comparable? comparable?)]
                                 [vs (listof comparable?)])
                       (listof list?)]
              @defproc[(=/classes [key (-> comparable? comparable?)]
                                  [vs (listof comparable?)])
                       (listof list?)])]{

 Like @racketlink[b:group-by]{group-by}, groups input values into equivalence classes under the equivalence relation induced by @racket[key]. Values that are @racket[=] after the application of @racket[key] are grouped together to form the classes.

@examples[
    #:eval eval-for-docs
    (group-by identity (list 1 1 2 2 3 3 3))
    (group-by identity (list 1 1 1))
    (group-by odd? (list 1 1 2 3 4 8 12))
    (group-by identity (list "cherry" "banana" "apple"))
    (group-by length (list "apple" "banana" "cherry"))
  ]
}

@defproc[(generic-set [#:key key (-> comparable? comparable?) #f]
                      [v comparable?]
                      ...)
         list?]{

 Returns a @tech/reference{set} containing deduplicated input values using the provided equivalence relation as the test for equality (by default, @racket[=] is applied directly unless a key is specified).

@examples[
    #:eval eval-for-docs
    (generic-set 1 2 3)
    (generic-set 1 1 2 2 3 3 3)
    (generic-set "cherry" "banana" "apple")
    (generic-set #:key odd? 1 2 3 4 5)
    (generic-set #:key string-upcase "apple" "Apple" "APPLE" "banana" "Banana" "cherry")
    (define my-set (generic-set #:key string-upcase "cherry" "banana" "apple"))
	(set-add my-set "APPLE")
  ]
}

@defproc[(tail [#:key key (-> comparable? comparable?) #f]
               [elem comparable?]
               [col sequence?])
         sequence?]{

 A generic version of @racket[member] that operates on any sequence rather than lists specifically, and employs the generic @racket[=] relation rather than the built-in @racketlink[b:equal?]{equal?}. The result of invocation is the tail of the sequence beginning at the value that is @racket[=] to @racket[elem], under the transformation @racket[key] (if provided), or the empty list if @racket[elem] isn't found. If a boolean value is desired, use @racket[member?] instead. If a tail @italic{by position} is desired, use @racket[drop].

@examples[
    #:eval eval-for-docs
    (tail 4 (list 1 2 3))
    (tail 4 (list 1 4 3))
    (->list (tail "cherry" (stream "apple" "banana" "cherry")))
    (tail "BANANA" (list "apple" "banana" "cherry"))
    (tail #:key string-upcase "BANANA" (list "apple" "banana" "cherry"))
  ]
}

@deftogether[(
@defproc[(member? [#:key key (-> comparable? comparable?) #f]
                  [elem comparable?]
                  [col sequence?])
         boolean?]
@defproc[(in? [#:key key (-> comparable? comparable?) #f]
              [elem comparable?]
              [col sequence?])
         boolean?]
 )]{

 A generic version of @racket[member] similar to @racket[tail] that checks if a value is present in a collection. Unlike @racket[tail], this returns a boolean value and supports non-ordered collections like @racketlink[set]{sets} where a tail is not well-defined. In the special case where @racket[col] is a @racket[generic-set], the @racket[key] provided to @racket[member?], if any, is ignored as it may conflict with the existing @racket[key] defining the equivalence relation in the generic set.

 @racket[in?] is a right-curried version of @racket[member?], useful in cases where we may want to pre-specify the collection and apply the predicate to candidate elements.

@examples[
    #:eval eval-for-docs
    (member? 4 (list 1 2 3))
    (member? 4 (list 1 4 3))
    (member? "cherry" (stream "apple" "banana" "cherry"))
    (member? "BANANA" (list "apple" "banana" "cherry"))
    (member? #:key string-upcase "BANANA" (list "apple" "banana" "cherry"))
    (member? "BANANA" (generic-set #:key string-upcase "apple" "banana" "cherry"))
    (member? "tomato" (generic-set #:key length "apple" "banana" "grape"))
	((in? (list 1 2 3)) 3)
	((in? (list 1 2 3)) "2")
	((in? #:key ->number (list 1 2 3)) "2")
  ]
}

@defproc[(assoc [#:key key (-> comparable? comparable?) #f]
                [assoc-key comparable?]
                [col (sequenceof pair?)])
         pair?]{

 A generic version of @racketlink[b:assoc]{assoc}, this checks if a value keyed by @racket[assoc-key] is present in a collection structured as an @tech/reference{association list}. The @racket[first] item in each element of @racket[col] is compared against @racket[assoc-key] using @racket[=] after first applying the transformation @racket[key] to both values.

@examples[
    #:eval eval-for-docs
    (assoc 'b (list '(a 1) '(b 2)))
    (assoc 2 (list '(1 a) '(2 b) '(3 c)))
    (assoc 2 (list '(1 a) '(2.0 b) '(3 c)))
    (assoc #:key string-upcase "cherry" (list '("Apple" a) '("Banana" b) '("Cherry" c)))
  ]
}
