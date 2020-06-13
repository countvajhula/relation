#lang racket/base

(module+ test
  (require rackunit
           (except-in data/collection
                      foldl
                      foldl/steps
                      append
                      index-of)
           relation
           (prefix-in b: racket/base)
           racket/set
           racket/stream
           (only-in racket/function
                    identity
                    thunk))

  ;; append
  (check-equal? (.. "hi" " " "there") "hi there")
  (check-equal? (.. #"hi" #" " #"there") #"hi there")
  (check-equal? (.. '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
  (check-equal? (.. #(1 2 3) #(4 5 6)) #(1 2 3 4 5 6))
  (check-equal? (set-count (.. (set 1 2 3) (set 3 4 5))) 5)
  ;; TODO: why doesn't this work?
  ;; (check-equal? (.. (hash 'a 1 'b 2) (hash 'c 3)) (hash 'a 1 'b 2 'c 3))
  (check-equal? (->list (.. (stream 1 2 3) (stream 4 5 6))) (list 1 2 3 4 5 6))
  (check-equal? ((.. ->string +) 3 4) "7")
  (check-equal? (..) ID)
  ;; id
  (check-equal? ((id +) 3) 0)
  (check-equal? ((id +) -3) 0)
  (check-equal? ((id *) 3) 1)
  (check-equal? ((id *) -3) 1)
  (check-equal? ((id +) #(1 -2)) #(0 0))
  (check-equal? ((id ..) #(1 -2)) #())
  (check-equal? ((id ..) +) values)
  (check-equal? ((id ..) "hello") "")
  (check-equal? ((id ..) #"hello") #"")
  (check-equal? ((id ..) '(1 2 3)) '())
  (check-equal? ((id ..) (set 1 2 3)) (set))
  (check-equal? ((id ..) (hash 'a 1 'b 2 'c 3)) (hash))
  (check-equal? ((id ..) (stream 1 2 3)) (stream))
  (check-exn exn:fail:contract?
             (thunk
              (id add1)))
  ;; multiplication
  (check-equal? (* 3 4) 12)
  (check-equal? (* 3 -4) -12)
  (check-equal? (*) ID)
  ;; addition
  (check-equal? (+ 97 3) 100)
  (check-equal? (->vector (+ #(1 2 3) #(1 2 3) #(1 2 3))) #(3 6 9))
  (check-equal? (+) ID)
  ;; inverse
  (check-equal? ((inverse +) 3) -3)
  (check-equal? ((inverse +) #(1 2)) #(-1 -2))
  (check-equal? ((inverse +) #(1 -2 3)) #(-1 2 -3))
  (let ([x 3])
    (check-equal? (+ x ((inverse +) x))
                  ((id +) x)))
  (let ([x #(1 -2 3)])
    (check-equal? (+ x ((inverse +) x))
                  ((id +) x)))
  (let ([x 3])
    (check-equal? (* x ((inverse *) x))
                  ((id *) x)))
  (check-exn exn:fail:contract?
             (thunk
              (inverse add1)))
  ;; "subtraction"
  (check-equal? (- 4 3) 1)
  (check-equal? (- 4 6) -2)
  (check-equal? (- #(1 2) #(1 2)) #(0 0))
  (check-equal? (- #(1 2) #(-1 3)) #(2 -1))
  (check-equal? (- #(1 2 3) #(1 0 0) #(0 2 1)) #(0 0 2))
  (check-equal? (- 1) -1 "subtraction with single arg returns inverse")
  (check-equal? (- #(1 -2)) #(-1 2) "subtraction with single arg returns inverse")
  ;; "division"
  (check-equal? (/ 4 3) (b:/ 4 3))
  (check-equal? (/ 4 6) (b:/ 4 6))
  (check-equal? (/ 3) (b:/ 1 3) "division with single arg returns inverse")
  ;; fold
  (check-equal? (foldr + '(1 2 3 4)) 10)
  (check-equal? (foldr * '(1 2 3 4)) 24)
  (check-equal? (foldl + '(1 2 3 4)) 10)
  (check-equal? (foldl * '(1 2 3 4)) 24)
  (check-equal? (fold + '(1 2 3 4)) 10)
  (check-equal? (fold * '(1 2 3 4)) 24)
  (check-equal? (foldr cons '(1 2 3) #:into '()) '(1 2 3))
  (check-equal? (foldl cons '(1 2 3) #:into '()) '(3 2 1))
  (check-equal? (foldr cons '(1 2 3) #:into '() #:order 'bab) '(((() . 3) . 2) . 1))
  (check-equal? (foldl cons '(1 2 3) #:into '() #:order 'bab) '(((() . 1) . 2) . 3))
  (check-equal? (fold + (stream 1 2 3 4)) 10)
  (check-equal? (fold + '() #:into 0) 0 "empty input with base value")
  (check-equal? (fold + '()) ID "empty input without base value")
  (check-equal? (fold + (list 1 2 3) (list 1 2 3)) 12 "fold over parallel sequences")
  (check-equal? (fold #:into '() (λ (x y xs) (cons (list x y) xs)) (list 1 2 3) (list 1 2 3)) '((1 1) (2 2) (3 3)) "fold over parallel sequences")
  (check-equal? (fold #:into '() #:order 'bab (λ (xs x y) (cons (list x y) xs)) (list 1 2 3) (list 1 2 3)) '((1 1) (2 2) (3 3)) "fold over parallel sequences")
  (check-equal? (->list (foldl/steps + '(1 2 3 4))) '(0 1 3 6 10))
  (check-equal? (->list (foldr/steps + '(1 2 3 4))) '(0 4 7 9 10))
  (check-equal? (fold (λ (a b)
                        (and a b))
                      '(#t #t #t)
                      #:into #f)
                #f
                "boolean #f base value")
  ;; join
  (check-equal? (join '()) ID)
  (check-equal? (join '(1 2 3 4)) 10)
  (check-equal? (join '(#(1 2 3) #(1 2 3))) #(1 2 3 1 2 3))
  (check-equal? (join '("hi" "there")) "hithere")
  (check-equal? ((join (list add1 sub1)) 1) 1)
  (check-exn exn:fail:contract? (thunk (join '(hi there))))
  ;; sum
  (check-equal? (sum '()) ID)
  (check-equal? (sum '(1 2 3 4)) 10)
  (check-equal? (sum '(#(1 2 3) #(1 2 3))) #(2 4 6))
  (check-exn exn:fail:contract? (thunk (sum '("hi" "there"))))
  ;; product
  (check-equal? (product '()) ID)
  (check-equal? (product '(1 2 3 4)) 24)
  (check-exn exn:fail:contract? (thunk (product '("hi" "there"))))
  ;; power
  (check-equal? (power 0 3 *) 0)
  (check-equal? (power 0 3 +) 0)
  (check-equal? (power 1 3 +) 3)
  (check-equal? (power 1 3 *) 1)
  (check-equal? (power 1 -3 +) -3)
  (check-equal? (power 1 -3 *) 1)
  (check-equal? (power -1 -3 *) -1)
  (check-equal? (power -1 3 *) -1)
  (check-equal? (power -1 -4 *) 1)
  (check-equal? (power -1 4 *) 1)
  (check-equal? (power '() 3) '())
  (check-equal? (power '(1) 3) '(1 1 1))
  (check-equal? (power '(1 2) 3) '(1 2 1 2 1 2))
  (check-equal? (power '(1 2 3) 0) '())
  (check-equal? (power "abc" 2) "abcabc")
  (check-equal? ((power add1 3) 5) 8)
  (check-equal? ((power add1 1) 3) 4)
  (check-equal? ((power add1 0) 3) 3)
  (check-exn exn:fail:contract? (thunk (power '("hi" "there"))))
  ;; gather
  (check-equal? (->list (gather (list add1 sub1 ->string) 23)) '(24 22 "23"))
  (check-equal? (->list (gather (list add1) 23)) '(24))
  (check-equal? (->list (gather '() 23)) '())
  (check-equal? (->list (gather '() "hi" "there")) '())
  (check-equal? (->list (gather (list ..) "hi" "there")) '("hithere"))
  (check-equal? (->list (gather (list + *) 7 6)) '(13 42))
  ;; composition identity
  (check-equal? (.. ID) ID)
  (check-equal? (.. '(1 2 3) ID) '(1 2 3))
  (check-equal? (.. ID '(1 2 3)) '(1 2 3))
  (check-equal? (.. "hi" ID) "hi")
  (check-equal? (.. ID "hi") "hi")
  (check-equal? (* ID) ID)
  (check-equal? (* 3 ID) 3)
  (check-equal? (* ID 3) 3)
  (check-equal? (+ ID) ID)
  (check-equal? (+ 3 ID) 3)
  (check-equal? (+ ID 3) 3)
  (check-equal? (+ #(1 2 3) ID) #(1 2 3))
  (check-equal? (+ ID #(1 2 3)) #(1 2 3))
  (check-equal? (reverse ID) ID)
  ;; reify
  (check-equal? (reify 5 "") 5)
  (check-equal? (reify "hi" 5) "hi")
  (check-equal? (reify '(1 2 3) "") '(1 2 3))
  (check-equal? (reify ID "hi") "")
  (check-equal? (reify ID 5) 0)
  (check-equal? (reify ID 5 *) 1)
  (check-equal? (reify ID '(1 2)) '())
  (check-equal? (reify ID #(1)) #())
  (check-equal? (reify ID #(1) +) #(0))
  (check-equal? (reify ID (hash 'a 1)) (hash))
  (check-equal? (reify ID add1) values)
  (check-equal? (reify ID (stream 1)) (stream))
  (check-equal? (reify ID (set 1)) (set)))
