#lang racket

(provide sql-token<%> sql-token? sql-token-reduce)

(require "../_essence.rkt")

(define sql-token<%>
  (interface (token<%>)
    sql-token-reduce #;(-> sql-token? any/c)
    ))

(define sql-token? (is-a?/c sql-token<%>))

(define (sql-token-reduce x)
  (send x sql-token-reduce))
