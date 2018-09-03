#lang racket

;Made by Brad Tully, 17 May 2018, Programming Languages: Assignment 5

(require test-engine/racket-tests)
(require 2htdp/planetcute)

#|
downseries: takes a step amount a high value and low value. It makes a list from including high decreasing in decrements of step,
            possibly includes low.
param: step- is a positive number that represents the amount of each decrement for the next list value
param: high- a number value that the list will start at
param: low- a number value that the function will stop decrementing and adding to the list once the value is <= low
return: this function returns a list of numbers from high to low (may or may not be included)
|#
(define (downseries step high low)
  (cond
    [(< high low) null]
    [(= high low) (cons low null)]
    [else (cons high (downseries step (- high step) low))]))

#|
meow-string-map: takes a list of strings and appends "meow" to the end of each string in the list
param: lst is a list of strings
return: the function returns a list of strings with "meow" appended to them
|#
(define (meow-string-map lst)
  (map (lambda (s1) (string-append s1 "meow")) lst))

#|
list-ref-div: takes a list and returns the ith element of the list where we count from zero and i is the
              quotient produced when dividing n by the length's list
param: lst is a list
param: n is a positive number that is divided by the list length to get i
return: this function returns a the ith list value, or an error if n is negative or the list is null
|#
(define (list-ref-div lst n)
  (cond
    [(negative? n) (error "list-ref-div: negative number")]
    [(null? lst) (error "list-ref-div: empty list")]
    [else (list-ref lst (quotient n (length lst)))]))

#|
stream-maker: function made by Chris in class to build streams, I'm just using it for his definition of nats for testing
              my other functions
param: fn is a function
param: arg is a numeric value
return: a stream
|#
(define (stream-maker fn arg)
  (letrec ([f (λ (x)
                (cons x (λ () (f (fn x arg)))))])
    (λ () (f arg))))

#|
nats: a stream made by Chris in class I'm also using this for tests, it takes no values and returns a stream of natural numbers
return: stream of natural numbers
|#
(define nats (stream-maker + 1))

#|
next-k-items: takes a stream and a number, it returns k values from the stream
param: s is a stream
param: k is a positive number
return: the first k stream outputs in a list
|#
(define (next-k-items s k)
  (letrec ([f (λ (str acc)
                (let ([pair (str)])
                  (if (= acc k)
                      (cons (car pair) null)
                      (cons (car pair) (f (cdr pair) (+ acc 1))))))])
    (f s 1)))

#|
kth-item: takes a stream and a number and returns the kth item from the stream
param: s is a stream
param: k is a positive number
return: this function returns the kth item from the stream s
|#
(define (kth-item s k)
  (letrec ([f (λ (l acc)
                (if (= acc k)
                    (car l)
                    (f (cdr l) (+ acc 1))))])
    (f (next-k-items s k) 1)))

#|
negate-3-and-7: a stream that returns natural numbers, but if the number is divisible by 3 or 7 it is negated
param: N/A
return: natural numbers, but if the number is divisible by 3 or 7 it is negated
|#
(define (negate-3-and-7)
  (letrec ([f (λ (x)
                (if (or (= (modulo x 3) 0) (= (modulo x 7) 0)) 
                    (cons (* x -1) (λ () (f (add1 x))))
                    (cons x (λ () (f (add1 x))))))])
    (f 1)))

#|
key-heart-star: a stream that returns pictures of a key followed by a heart followed by a star then repeats
param: N/A
return: pictures of a key followed by a heart followed by a star then repeats
|#
(define (key-heart-star)
  (letrec ([f (λ (x)
                (cond
                  [(= x 0) (cons key (λ () (f (add1 x))))]
                  [(= x 1) (cons heart (λ () (f (add1 x))))]
                  [(= x 2) (cons yellow-star (λ () (f 0)))]))])
    (f 0)))

#|
two-pairs-stream: takes a stream s and returns a new stream such that each element in the stream
                  is the pair '(2 . k), where k is the kth element returned from s
param: s is a stream
return: a stream of pairs with a car of 2 and the kth value of the stream such that '(2 . k)
|#
(define (two-pairs-stream s)
  (letrec ([f (λ (str)
                (let ([pair (str)])
                  (cons (cons 2 (car pair)) (λ () (f (cdr pair))))))])
    (λ () (f s))))

#|
spin-stream: takes two lists and makes a stream that pairs together one element from the first and one from the second list it'll rotate
             through them forever
param: xs is a list of items
param: ys is a list of items
return: a pair with the first item from xs and the second from ys
|#
(define (spin-stream xs ys)
  (define (helper acc lst lst2)
    (cons (cons (list-ref lst (modulo acc (length lst))) (list-ref lst2 (modulo acc (length lst2)))) (λ () (helper (add1 acc) lst lst2))))
  (λ () (helper 0 xs ys)))

#|
kvpv-lookup: finds a value inside of a vector of pairs, returns the pair if the value is there, #f if it isn't, if an item isn't a pair it's ignored
param: v is a value to be searched in the vector
param: vec is a vector
return: if the value is found in the front of a pair, the pair is returned, otherwise #f
|#
(define (kvpv-lookup v vec)
  (letrec ([f (λ (val vector pos)
                (cond
                  [(and (pair? (vector-ref vector pos)) (equal? (car (vector-ref vector pos)) val)) (vector-ref vector pos)]
                  [(= pos (- (vector-length vector) 1)) #f]
                  [else (f val vector (add1 pos))]))])
    (f v vec 0)))

#|
cached-lookup: takes two arguments, a list (called lst) of key-value pairs and a number n which will represent the cache size.
               When the function begins, a vector of size n is made that is initialized with #f in each element. cached-lookup
               returns a function that takes the same arguments as assoc. This function uses the previous arguments to find them by first checking
               the cache and then using assoc on the list. kvpv-lookup is used to search the cache, and if found it wil,
               return a pair (#t, p), where p is the pair returned by kvpv-lookup. If the pair was not found, search the list for the
               matching pair. If it's not in the list either, return #f. Otherwise, store the result in the cache using mutation
               in a round-robin fashion (by round-robin, we mean you store first in element 0, then 1, and so forth, and finally wrap back to 0
               when you exceed the size of the cache). Then return (#f, p) where p is the result that assoc gave you
param: lst is a list of key value pairs
param: n is a key value that we are searching for
return: this returns a function that takes a value and list of keyvalue pairs
|#
(define (cached-lookup lst n)
  (let (
        [cache (make-vector n #f)]
        [slot 0])
  (letrec ([f (λ (val kvlst)
                (cond
                  [(equal? (kvpv-lookup val cache) #f)
                   (if (equal? (assoc val kvlst) #f)
                       #f
                       (letrec ([g (λ ()
                                     (vector-set! cache slot (assoc val kvlst))
                                     (if (= slot (- (vector-length cache) 1))
                                         (set! slot 0)
                                         (set! slot (+ slot 1)))
                                     (cons #f (assoc val kvlst)))])
                         (g)))]
                  [(pair? (kvpv-lookup val cache)) (cons #t (kvpv-lookup val cache))]))])
    (λ (a b) (f a b)))))

#|
repeatit!: a macro, that takes the form (repeatit! e1 until e2), with the semantics that:
           It evaluates e1 at least once.
           It evaluates e2 after each execution of e1.
           It continues to evaluate e1 repeatedly while e2 returns #f, otherwise it completes and returns
param: e1 is an expression that does something
param: e2 is an expression that returns #t or #f
return: the values of e1 while e2 is #f
|#
(define-syntax repeatit!
  (syntax-rules (until)
    [(repeatit! e1 until e2)
     (letrec ([f (λ ()
                   e1
                   (cond
                     [(equal? e2 #f) (f)]))])
       (f))]))



