#lang racket

(require rackunit
         morsel-lib)

(struct my-queryable (item) #:transparent
  #:methods gen:queryable
  [(define (unwrap-queryable me)
     (my-queryable-item me))])

; Do not fully unwrap. Need to append here:
(check-equal? (from x (my-queryable (from x "X" 1 2 3)))
              (from x "X" 1 2 3))
