#lang racket

(provide def-struct (rename-out [eq-shim make-eq-shim]))

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

; A wrapper that makes all equality use eq? equality
(struct eq-shim (content)
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?)
     (eq? (eq-shim-content a)
          (eq-shim-content b)))
   (define (hash-proc a hasher)
     (eq-hash-code (eq-shim-content a)))
   (define (hash2-proc a hasher)
     (eq-hash-code (eq-shim-content a)))])
