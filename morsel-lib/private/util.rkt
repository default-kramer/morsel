#lang racket

(provide def-struct)

(require racket/struct)

(define-syntax-rule (def-struct stuff ...
                      #:equality equal-stuff
                      #:write writer)
  (struct stuff ...
    #:methods gen:equal+hash
    [(define (equal-proc a b equal?)
       (equal? (equal-stuff a)
               (equal-stuff b)))
     (define (hash-proc a hasher)
       (hasher (equal-stuff a)))
     (define (hash2-proc a hasher)
       (hasher (equal-stuff a)))]
    #:methods gen:custom-write
    [(define (write-proc me port mode)
       (writer me port mode))]))
