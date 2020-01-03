#lang racket

; A "step" refers to a procedure that inspects some content.
; The return value of a step is ignored.
; A step should call `(next x)` when it wants to explore deeper into the content.
; One or more steps can be combined into a "pass" which wires up `next` correctly.
; A pass is a procedure that can be called like `(my-pass the-content)`
; Calling a step procedure directly is probably an error.

(provide next define-step steps->pass define-pass
         cache! get/cache!
         in-join-clause? in-grouped-join?
         context? context-stack context-base-query)

(require "model.rkt"
         racket/stxparam)

; As we traverse the query, we will keep a stack of
(struct context (base-query mode) #:transparent)
; where mode is one of
;   'join-clause    means "in a join-on clause"
;   'query-clause   means "in any other kind of clause"
;   'attach         means "in a join created via attach"
; Whenever we find a base-query, we will explicitly iterate over each piece of content
; and put the correct context onto the stack before recursing.
(define context-stack (make-parameter '()))

(define mode:join-clause 'join-clause)
(define mode:query-clause 'query-clause)
(define mode:attach 'attach)

; Adds to the context stack
(define-syntax-rule (with-context [query mode] body ...)
  (parameterize ([context-stack (cons (context query mode)
                                      (context-stack))])
    body ...))

; next : will be bound as a continuation within a step
(define-syntax-parameter next
  (λ(stx) (raise-syntax-error #f "used out of context" stx)))

; make-step creates a procedure that wants the `next` continuation. For example
#;(make-step (x) body ...)
; is transformed into something like
#;(lambda (next x) body ...)
; where `next` is a continuation that will eventually explore deeper into the query.
; This will allow disparate steps to be combined into one pass.
(define-syntax (make-step stx)
  (syntax-case stx ()
    [(_ (x) body ...)
     (with-syntax ([ooo (quote-syntax ...)])
       (syntax/loc stx
         (lambda (_next x)
           (syntax-parameterize ([next (λ(stx) (syntax-case stx ()
                                                 [(_ arg ooo)
                                                  (syntax/loc stx
                                                    (_next arg ooo))]
                                                 [id
                                                  (syntax/loc stx _next)]))])
             body ...))))]))

(define-syntax (define-step stx)
  (syntax-case stx ()
    [(_ (id x) body ...)
     (syntax/loc stx
       (define id (make-step (x) body ...)))]))

; This is appropriate to be the final step in the chain.
; It will explore deeper into the query.
(define-step (recurse x)
  (cond
    [(base-query? x)
     (begin
       (for/list ([join (attached-joins x)])
         (with-context [x mode:attach]
           (next join)))
       (for/list ([clause (query-clauses x)])
         (let ([mode (if (join-on? clause)
                         mode:join-clause
                         mode:query-clause)])
           (with-context [x mode]
             (next clause)))))]
    [(token? x)
     (next (token-content x))]
    [(list? x)
     (map (λ(a) (next a)) x)]
    [(pair? x)
     (begin (next (car x))
            (next (cdr x)))]
    [(box? x)
     (next (unbox x))]))

; Given (a b c), here's how we could leave it open for further composition
#;(λ (next x)
    (let ([c/curried (curry c next)])
      (let ([b/curried (curry b c/curried)])
        (let ([a/curried (curry a b/curried)])
          (a/curried x)))))

; If we have 3 steps like (a b c) we want to generate a list like
#;([a/curried (λ(x) (a b/curried x))]
   [b/curried (λ(x) (b c/curried x))]
   [c/curried (λ(x) (c a/curried x))])
; (the outermost parens are the caller's responsibility)
(define-for-syntax (build-letrec step-ids step-ids/curried)
  (define len (vector-length step-ids))
  (define (next-index i)
    (let ([i (add1 i)])
      (if (= i len) 0 i)))
  (for/list ([i (in-range len)])
    #`[#,(vector-ref step-ids/curried i)
       (λ(x) (#,(vector-ref step-ids i)
              #,(vector-ref step-ids/curried (next-index i))
              x))]))

(define-syntax (combine-steps stx)
  (syntax-case stx ()
    [(_ step-expr ...)
     (with-syntax ([(step-id ...)
                    (generate-temporaries (syntax->list #'(step-expr ...)))])
       (let* ([step-ids/vec
               (list->vector (syntax->list #'(step-id ...)))]
              [step-ids/curried
               (list->vector (generate-temporaries #'(step-id ...)))])
         (quasisyntax/loc stx
           (let ([step-id step-expr]
                 ...)
             (letrec (#,@(build-letrec step-ids/vec step-ids/curried))
               ; return the first curried pass
               #,(vector-ref step-ids/curried 0))))))]))

(define-syntax-rule (steps->pass step ...)
  (combine-steps step ... recurse))

; defines a pass consisting of one step
(define-syntax-rule (define-pass (pass-id x) body ...)
  (define pass-id (steps->pass (make-step (x) body ...))))

(define (in-join-clause?)
  (let* ([x (context-stack)]
         [x (and (pair? x) (car x))]
         [x (and x (context-mode x))])
    (equal? x mode:join-clause)))

(define (in-grouped-join?)
  (let* ([x (context-stack)]
         [x (and (pair? x) (car x))]
         [x (and x (context-base-query x))])
    (grouped-join? x)))


(define none (gensym 'none))

(define cache! set-token-aspect!)

; If a value is already cached, just return it.
; Otherwise execute the body.
; If a value has now been cached, return it.
; Otherwise, the #:default value is cached and returned.
(define-syntax-rule (get/cache! token key #:default default-expr #:body body ...)
  (let* ([cached (get-token-aspect token key none)]
         [cached? (not (equal? none cached))]
         [_ (when (not cached?)
              (begin body ...))]
         [cached (get-token-aspect token key none)]
         [cached? (not (equal? none cached))])
    (if cached?
        cached
        (let ([default default-expr])
          (cache! token key default)
          default))))
