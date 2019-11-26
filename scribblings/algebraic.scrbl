#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
		 racket/sandbox
         @for-label[relation/algebraic
		            racket/generic
                    (except-in racket +)]]

@title{Algebraic Operations}

@defmodule[relation/algebraic]

Generic algebraic operations. The built-in algebraic operators @racket[+] and @racket[*] operate on numbers specifically. This module enables use of these operations to achieve the "canonical" operation for a given type. The operator @racket[+] performs the canonical @hyperlink["https://en.wikipedia.org/wiki/Group_(mathematics)"]{group} operation (e.g. addition, for numbers), while @racket[..] or @racket[∘] performs the canonical @hyperlink["https://en.wikipedia.org/wiki/Monoid"]{monoid} operation (e.g. concatenation, for strings and lists).

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
				                 '(require relation)
								 '(require racket/set)
								 '(require racket/stream))))

@defthing[gen:group any/c]{

 A @tech/reference{generic interface} that represents any object for which an addition-like (group) operation can be defined. The following built-in types have implementations for @racket[gen:group]:

@itemlist[
 @item{@tech/reference{numbers}}
 @item{@tech/reference{vectors}}]

@examples[
    #:eval eval-for-docs
    (+ 1 2 3)
    (->list (+ #(1 2 3) #(1 2 3) #(1 2 3)))
  ]
}

@defthing[gen:monoid any/c]{

 A @tech/reference{generic interface} that represents any object for which a multiplication-like (monoid) operation can be defined. The following built-in types have implementations for @racket[gen:monoid]:

@itemlist[
 @item{@tech/reference{numbers}}
 @item{@tech/reference{strings}}
 @item{@tech/reference{byte strings}}
 @item{@tech/reference{lists}}
 @item{@tech/reference{vectors}}
 @item{@tech/reference{sets}}
 @item{@tech/reference{sequences}}
 @item{@tech/reference{procedures}}
]

@examples[
    #:eval eval-for-docs
    (.. 1 2 3)
    (.. "hi" " " "there")
    (.. '(1 2 3) '(4 5 6))
  ]
}

@defproc[(+ [v group?] ...)
         group?]{

 Performs the canonical "addition-like" operation on the data, based on its type. This operation is the natural operation on the data type that forms an algebraic group.

@examples[
    #:eval eval-for-docs
    (+ 1 2 3)
    (->list (+ #(1 2 3) #(1 2 3) #(1 2 3)))
  ]
}

@deftogether[(@defproc[(.. [v monoid?] ...)
              monoid?]
			  @defproc[(∘ [v monoid?] ...)
              monoid?])]{

 Performs the canonical "multiplication-like" operation on the data, based on its type. This operation is the natural operation on the data type that forms an algebraic monoid.

@examples[
    #:eval eval-for-docs
    (.. 1 2 3)
    (.. "hi" " " "there")
    (.. '(1 2 3) '(4 5 6))
	((∘ ->string +) 3 4)
  ]
}
