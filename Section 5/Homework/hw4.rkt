
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(define ones (lambda () (cons 1 ones)))

;; put your code below
;; PROBLEM 1
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null
      ))

;; PROBLEM 2
(define (string-append-map xs suffix)
  (map (lambda (xs-element) (string-append xs-element suffix)) xs))

;; PROBLEM 3
(define (list-nth-mod xs n)
  (cond [(< n 0) error "list-nth-mod: negative number"]
        [(null? xs) error "list-nth-mod: empty list"]
        [#t (let ([r (remainder n (length xs))])
              (car (list-tail xs r)))]))

;; PROBLEM 4
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s count)
                (let ([pr (s)])
                  (if (<= count n)
                      (cons (car pr) (f (cdr pr) (+ count 1)))
                      null)))])
    (f s 1)))

;; PROBLEM 5
(define funny-number-stream
  (letrec ([f (lambda (x)
            (if (= (modulo x 5) 0)
                (cons (- x) (lambda () (f (+ x 1))))
                (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; PROBLEM 6
(define dan-then-dog
  (letrec ([f (lambda (x)
            (if (= (modulo x 2) 0)
                (cons "dog.jpg" (lambda () (f (+ x 1))))
                (cons "dan.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; PROBLEM 7
(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (let ([pr (x)]) ;; Let pr equal to stream thunk for ease of reading
                      (lambda ()
                        (cons ;; Create new stream
                         (cons 0 (car pr)) ;; Append 0 to beginning of the list, call first number of the stream
                         (f (cdr pr)) ;; Append remainder of stream onto list
                         )
                        )))])
    (f s)))

;; PROBLEM 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (count) 
                (cons ;; Create new stream 
                  (cons (list-nth-mod xs count) (list-nth-mod ys count)) ;; Create pair based on list-mod
                  (lambda () (f (+ count 1)))))]) ;; Append rest of stream to end 
    (lambda () (f 0))))

;; PROBLEM 9
(define (vector-assoc v vec)
    (letrec ([total_length (vector-length vec)]
             [f (lambda (x)
                (cond [(= x total_length) #f]
                      [(pair? (vector-ref vec x))
                       (let ([current_pair (vector-ref vec x)])
                         (if (equal? v (car current_pair))
                             current_pair
                             (f (+ x 1))))]
                      [#t (f (+ x 1))]))])
    (f 0)))


;; PROBLEM 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [cache-count 0]
           [check-cache (lambda (x)
                            (let ([get-cache-v (vector-assoc x cache)])
                                  (if get-cache-v
                                      get-cache-v
                                      (let ([find-in-xs (assoc x xs)])
                                        (if find-in-xs
                                            (begin (vector-set! cache (remainder cache-count n) find-in-xs)
                                                    (set! cache-count (+ cache-count 1))
                                                    find-in-xs)
                                            #f)))))])
    check-cache))

;; PROBLEM 11
(define-syntax while-less
  (syntax-rules (do)
          [(while-less e1 do e2)
           (let ([a1 e1])
             (letrec ([f (lambda (a2)
                          (if (> a2 a1)
                              #t
                              (f (+ a2 1))))])
               (f e2)))]))