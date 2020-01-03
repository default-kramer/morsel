#lang racket

(provide token<%> token? define-token-aspect-stuff
         token-kind token-content set-token-aspect! get-token-aspect

         gen:tuple tuple? tuple-query tuple-alias

         base-query<%> base-query? query?
         attached-joins query-clauses query-queryable query-alias query-tuple property-value
         query-clauses/printable

         join<%> join? join-target

         clause<%> clause? clause-apply

         queryable? gen:queryable get-queryable unwrap-queryable

         ; For more flexibility, we could make these methods of token<%> and base-query<%>,
         ; but for now we just infer them based on token-kind.
         join-on? group-by? scalar-injectable? aggregate-injectable?
         simple-join? grouped-join?
         )

(require racket/generic)

(define token<%>
  (interface ()
    token-kind
    token-content
    set-token-aspect!
    get-token-aspect
    ))

(define token? (is-a?/c token<%>))

(define (token-kind x)
  (send x token-kind))
(define (token-content x)
  (send x token-content))
(define (set-token-aspect! x key value)
  (send x set-token-aspect! key value))
(define (get-token-aspect x key [on-not-found #f])
  (send x get-token-aspect key on-not-found))

; To be used within a `class` definition
(define-syntax-rule (define-token-aspect-stuff)
  (begin
    (define dict (make-hash))
    (define/public (set-token-aspect! key value)
      (dict-set! dict key value))
    (define/public (get-token-aspect key value)
      (dict-ref dict key value))))

(define-generics tuple
  (tuple-query tuple)
  (tuple-alias tuple)
  (tuple-queryable tuple)
  #:fallbacks
  [(define/generic :tuple-query tuple-query)
   (define (tuple-alias t)
     (query-alias (:tuple-query t)))
   (define (tuple-queryable t)
     (query-queryable (:tuple-query t)))])

(define (base-query? x)
  (is-a? x base-query<%>))

(define base-query<%>
  (interface (token<%>)
    attached-joins
    query-clauses
    query-queryable
    query-alias
    query-tuple
    property-value
    query-clauses/printable ; return #f to indicate "just use `query-clauses` as is"
    ))

(define (attached-joins x)
  (send x attached-joins))
(define (query-clauses x)
  (send x query-clauses))
(define (query-queryable x)
  (send x query-queryable))
(define (query-alias x)
  (send x query-alias))
(define (query-tuple x)
  (send x query-tuple))
(define (property-value x prop)
  (send x property-value prop))
(define (query-clauses/printable x)
  (or (send x query-clauses/printable)
      (query-clauses x)))

(define (join? x)
  (is-a? x join<%>))

(define join<%>
  (interface (base-query<%>)
    join-link ; should return (or/c #f tuple? base-query?)
    ))

; In order to find the final target, we need to walk the links through simple joins.
; This should return (or/c #f query? non-simple-join?)
(define (join-target x)
  (define (chain x)
    (let ([next (next-link x)])
      (if next
          (chain next)
          x)))
  (chain (send x join-link)))

(define (next-link x)
  (cond
    [(simple-join? x)
     (send x join-link)]
    [(tuple? x)
     (tuple-query x)]
    [else #f]))

(define (query? x)
  (and (base-query? x)
       (not (join? x))))

(define-syntax-rule (define-token-preds [pred? symbol] ...)
  (begin
    (define (pred? x)
      (and (token? x)
           (equal? symbol (token-kind x))))
    ...))

(define-token-preds
  [join-on? 'join-on]
  [group-by? 'group-by]
  [scalar-injectable? 'scalar]
  [aggregate-injectable? 'aggregate])

(define (grouped-join? x)
  (and (join? x)
       (ormap group-by? (query-clauses x))))

(define (simple-join? x)
  (and (join? x)
       (andmap join-on? (query-clauses x))))


(define clause<%>
  (interface ()
    apply ; (-> this content? content?)
    ))

(define clause? (is-a?/c clause<%>))
(define (clause-apply x content)
  (send x apply content))


; Would be nice to make this available via an interface also...
(define-generics queryable
  (unwrap-queryable queryable))

(define (get-queryable x)
  (cond
    [(queryable? x)
     (get-queryable (unwrap-queryable x))]
    [(base-query? x)
     (query-queryable x)]
    [(tuple? x)
     (get-queryable (tuple-queryable x))]
    [else x]))
