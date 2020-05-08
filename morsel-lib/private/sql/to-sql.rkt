#lang racket

(provide to-sql)

(require (prefix-in r: "render.rkt")
         "../util.rkt"
         "../_essence.rkt"
         "clauses.rkt"
         "sql-token.rkt")

; When we see a query or join (a base-query), we will register its unique alias here.
(define alias-dict (make-parameter (hash)))

; increments counter until str is not found in values
(define (make-unique values str [counter #f])
  (let ([proposed (if counter
                      (format "~a~a" str counter)
                      str)])
    (if (member proposed values)
        (make-unique values str (add1 (or counter 0)))
        proposed)))

(define (add-unique-alias dict x)
  #;(-> dict? base-query? dict?)
  (let ([found (dict-ref dict x #f)])
    (if found
        dict
        (dict-set dict x (make-unique (dict-values dict) (query-alias x))))))

(define (add-unique-aliases dict lst)
  #;(-> dict? (listof base-query?) dict?)
  (match lst
    [(list a rest ...)
     (add-unique-aliases (add-unique-alias dict a) rest)]
    [(list) dict]))

(define-syntax-rule (with-unique-aliases x body ...)
  (parameterize ([alias-dict (add-unique-aliases (alias-dict) x)])
    body ...))

(define (transform-to-injection x)
  (let* ([inj (and (might-need-injection? x)
                   (inj-lookup x))])
    (and inj
         (not (inside-own-target? inj))
         (scalar (query-tuple (injection-grouped-join inj))
                 (format ".__INJECT~a" (injection-index inj))))))

(define (inside-own-target? inj)
  (let ([gj (injection-grouped-join inj)])
    (member gj (join-stack))))

(struct paren-wrapper (content))

; Puts the content into a paren-wrapper unless it is already wrapped.
; Should only be used during reduction, because it is not a token!
; This might be a footgun, so don't expose it thoughtlessly (maybe just make it a token)
(define (parens content)
  (match content
    [(list a)
     (parens a)]
    [else (if (paren-wrapper? content)
              content
              (paren-wrapper content))]))

(define (go1 x)
  (define option1 (transform-to-injection x))
  (cond
    [option1 (go1 option1)]
    [(paren-wrapper? x)
     (go "(" (paren-wrapper-content x) ")")]
    [(join? x) (join->renderable x)]
    [(query? x) (query->renderable x)]
    [(tuple? x) (tuple->renderable x)]
    [(sql-token? x) (go (sql-token-reduce x))]
    [(list? x) (apply go x)]
    [(string? x) x]
    ; We could allow the dialect or some other parameter to control
    ; how symbols are printed. For now, just go directly to string.
    [(symbol? x) (~a x)]
    [(number? x) (~a x)]
    ; We might have found a query in some nested content, like a subquery.
    ; If so, we know that it has already been reduced.
    [(or (r:query? x)
         (r:join? x))
     x]
    [else (error "cannot render" x)]))

(define (go . x)
  (cond
    [(empty? x) ""]
    [(symbol? (car x))
     (go1 (dispatch x))]
    [else (map go1 x)]))

(define (dispatch lst)
  (define sym (car lst))
  (case sym
    [(silence)
     (list)]
    [(subquery)
     (parens (cdr lst))]
    [(select where group-by having order-by join-on
             scalar aggregate bool subquery sql)
     (cdr lst)]
    [(+ - * /)
     (parens (interpose (format " ~a " sym) (cdr lst)))]
    [else (error "cannot dispatch fragment" lst)]))

(define (interpose x lst)
  (match lst
    [(list a b rest ...)
     (cons a (cons x (interpose x (cons b rest))))]
    [else lst]))

(define (tuple->renderable tuple)
  (let* ([q (tuple-query tuple)]
         [name (and q (dict-ref (alias-dict) q #f))])
    (or name
        ; We could have a global lookup, but is that really any better?
        ; Just use the alias that was given for now
        (tuple-alias tuple))))

(define (get key clauses)
  (map go1
       (filter (λ (clause) (equal? key (and (token? clause)
                                            (token-kind clause))))
               clauses)))

; Does not register the unique alias
(define (make-query bq injected-selects)
  #;(-> base-query? list? r:query?)
  (let ([joins (targeted-joins bq)]
        [clauses (query-clauses bq)])
    (r:make-query #:table (go1 (query-queryable bq))
                  #:alias (dict-ref (alias-dict) bq)
                  #:joins (map make-join joins)
                  #:selects (get 'select (append injected-selects clauses))
                  #:wheres (get 'where clauses)
                  #:group-bys (get 'group-by clauses)
                  #:havings (get 'having clauses)
                  #:order-bys (get 'order-by clauses)
                  #:limit (property-value bq :limit)
                  #:offset (property-value bq :offset)
                  #:distinct? (property-value bq :distinct))))

(define (query->renderable x)
  (with-unique-aliases (cons x (targeted-joins x))
    (make-query x (list))))

(define join-stack (make-parameter '()))

(define (make-join-raw j)
  #;(-> join? r:join?)
  (define join-ons
    (let* ([clauses (query-clauses j)]
           [clauses (get 'join-on clauses)])
      (if (empty? clauses)
          (list "1=1")
          clauses)))
  (define injections (inj-lookup j))
  (define (convert-to-selects injections index)
    (match injections
      [(list) (list)]
      [(list a rest ...)
       ; Don't put `a` into the result here, because then it could get replaced
       ; by an injection. We need to reduce!
       ; (Hmm, I wonder if `(go1 a)` would be more appropriate here?
       ;   Or would that double-reduce the entire content?)
       (cons (select (sql (sql-token-reduce a))
                     (format " as __INJECT~a" index))
             (convert-to-selects rest (add1 index)))]))
  ; We are only "inside the join" during make-query. The join-on clauses live outside.
  (define the-query (parameterize ([join-stack (cons j (join-stack))])
                      (make-query j (convert-to-selects injections 0))))
  (r:make-join the-query
               #:type (or (get-join-type j) 'inner)
               #:join-ons join-ons))

(define (make-join j)
  (with-unique-aliases (targeted-joins j)
    (make-join-raw j)))

(define (join->renderable x)
  (let ([found (dict-ref (alias-dict) x #f)])
    (if found
        found
        (error "morsel bug: found an inline join that was not registered" x))))

(define current-injection-lookup (make-parameter #f))

; Given a join, returns a list of tokens to be injected.
; Given anything else, returns (or/c #f injection?)
(define (inj-lookup x)
  (define proc (or (current-injection-lookup)
                   (error "morsel bug j3290v")))
  (proc x))

(define (to-sql x)
  (if (string? x)
      x
      (parameterize ([current-injection-lookup (calc-injections x)])
        (r:render-root (go1 x)))))
