#lang racket

(provide from (rename-out [:join join]) attach safe-write)

(module+ TODO-PRIVATE
  (provide flatten-lists?))

(require "../util.rkt"
         "model.rkt"
         "content.rkt"
         "base-query-printer.rkt"
         racket/struct
         racket/stxparam)

; The content of a query is flexible. These are the properties we recognize.
(define :clauses (make-property #:name 'clauses))
(define :joins (make-property #:name 'joins))
(define :print-clauses (make-property #:name 'print-clauses #:hidden #t))

(def-struct tuple (query alias queryable)
  #:property prop:custom-print-quotable 'never
  #:methods gen:tuple
  [(define (tuple-query me) (:tuple-query me))
   (define (tuple-alias me) (:tuple-alias me))
   (define (tuple-queryable me) (:tuple-queryable me))]
  #:equality tuple-stuff #:write write-tuple)

(define (tuple-stuff t)
  (list (:tuple-query t)
        (tuple-queryable t)))

(define :tuple-query tuple-query)
(define :tuple-alias tuple-alias)
(define :tuple-queryable tuple-queryable)

(define never-quote<%>
  (interface* () ([prop:custom-print-quotable 'never])))

(struct exn:fail:premature-query-access exn:fail () #:transparent)
(define msg
  ; As long as `tuple-query` is not made public, the "Likely cause"
  ; should be the *only* possible cause, I think.
  "Infinite loop averted. Likely cause: an equal or hash operation on a tuple.")

; This should not be equal? to anything else
(define (make-fail-content)
  (content-cons (new-content) :clauses (gensym 'morsel-fail-content-)))

(define safe-write? (make-parameter #f))

; Evaluates `expr` in a context where the exn:fail:premature-query-access
; will not be thrown. This is useful to avoid causing new errors while trying
; to produce an error message.
; It would be nice if we could guess when to safe-write and when to throw the
; exception, but I don't think it is possible. It's certainly safer to allow
; error message writers to opt-in to this exception-swallowing context.
; Warning! This macro cannot protect you from the fact that certain writing
; mechanisms (like pretty printing) are lazy.
; To be absolutely certain it will work, have `expr` return the string you want.
(define-syntax-rule (safe-write expr)
  (parameterize ([safe-write? #t])
    ; On error, we just try once more in case the latch wasn't set the first time
    (with-handlers ([exn:fail:premature-query-access?
                     (lambda (err) expr)])
      expr)))

(define base-query%
  (class* object% (base-query<%> equal<%> printable<%> never-quote<%>)
    ; content-builder is a function of (-> tuple? content?)
    ; This makes appending easy - we just call the original content-builder with our
    ; new tuple, then append to it.
    (init-field content-builder alias queryable
                [cycle-breaker (make-parameter #f)])
    (super-new)
    (define my-tuple (tuple this alias queryable))

    ; If we avert an infinite loop, we will set fail-content to some unique
    ; content before throwing the exception.
    ; This has acceptable equal+hash semantics because a query with an error
    ; should not be equal to anything else.
    (define fail-content #f)
    (define (get-fail-content)
      (and (safe-write?) fail-content))

    (define content #f)
    (define content-loop-detector #f)
    (define/public (query-content)
      (or (get-fail-content)
          content
          (begin
            (when content-loop-detector
              (set! fail-content (make-fail-content))
              ;(println (format " FAIL: ~a ~a" alias queryable))
              (raise (exn:fail:premature-query-access msg (current-continuation-marks))))
            ;(println (format "BEGIN: ~a ~a" alias queryable))
            (set! content-loop-detector #t)
            (set! content (content-builder my-tuple))
            ;(println (format "  END: ~a ~a" alias queryable))
            content)))

    (define (query-joins)
      (content-ref (query-content) :joins))

    ; token<%>
    (define/public (token-kind)
      (cond [(query? this) 'query]
            [(join? this) 'join]))
    (define/public (token-content)
      (list (query-joins this)
            (query-clauses this)))
    (define-token-aspect-stuff)

    ; base-query<%>
    (define/public (attached-joins)
      (query-joins))
    (define/public (query-clauses)
      (reverse (content-ref (query-content) :clauses)))
    (define/public (query-clauses/printable)
      (reverse (content-ref (query-content) :print-clauses)))
    (define/public (query-queryable) queryable)
    (define/public (query-alias) alias)
    (define/public (query-tuple) my-tuple)
    (define/public (property-value key)
      (content-ref (query-content) key))

    ; equal<%>
    (abstract equal+hash-content)
    (define (eq-shim)
      ; I think this is needed because the pretty printer explores using
      ; equal and or hash operations...
      ; But why isn't this same check in `query-content` enough? Hmm...
      (or (get-fail-content)
          (equal+hash-content)))
    (define/public (equal-to? other recur)
      ; This cycle-breaker strategy seems to be best.
      ; Let's say we are doing an equal operation on X1 and X2.
      ; This `equal-to?` method eventually gets called on two queries Q1 and Q2
      ; which are found within X1 and X2.
      ; When we encounter Q1 and Q2 for the first time, we mark them as
      ; "potentially equal" by setting their cycle-breakers to the same unique
      ; value before proceeding to explore their contents.
      ; When we encounter Q1 and Q2 a second time (via a cycle in the data
      ; structure), they are no longer "potentially equal" -- we can now say
      ; that they are definitely equal (or definitely not equal).
      ;
      ; Is it possible that some weird structure might mean that we encounter
      ; Q1 before Q2 even though they are otherwise equal?
      ; Maybe, but then X1 and X2 are not equal so it is OK to return false.
      ; (At least I think so... but all I can say is that is passes all the tests.)
      ;
      ; Hashing doesn't need any of these silly tricks.
      ; Racket's natural cycle detection works fine.
      (define cb1 cycle-breaker)
      (define cb2 (base-query-cycle-breaker other))
      (if (or (cb1) (cb2))
          (eq? (cb1) (cb2))
          (let ([cb-value (gensym)])
            (parameterize ([cb1 cb-value]
                           [cb2 cb-value])
              (recur (eq-shim) (send other equal+hash-content))))))
    (define/public (equal-hash-code-of hasher)
      (hasher (eq-shim)))
    (define/public (equal-secondary-hash-code-of hasher)
      (hasher (eq-shim)))

    ; printable<%>
    (define/public (build-print-content attached?)
      ; Return a list suitable for the second part of a constructor-style-printer.
      (let* ([target (and (not attached?)
                          (join? this)
                          (join-target this))]
             [target (if (base-query? target)
                         (send target query-tuple)
                         target)]
             [join-shims (map (λ (j) (join-shim j #t))
                              (attached-joins))]
             ; Build the list backwards:
             [lst (query-clauses/printable)]
             [lst (append join-shims lst)]
             [lst (if target
                      (cons (symbol-printer '#:to) (cons target lst))
                      lst)]
             [lst (cons queryable lst)]
             [lst (cons my-tuple lst)])
        lst))
    (abstract do-print)
    (define (do-print2 port mode)
      ; I'm not sure if it is possible to hit the true branch here,
      ; but better safe than sorry
      (if (get-fail-content)
          (write-string (format "#<broken query or join: ~a>" queryable)
                        port)
          (do-print port mode)))
    (define/public (custom-print port mode)
      (do-print2 port mode))
    (define/public (custom-write port)
      (do-print2 port #t))
    (define/public (custom-display port)
      (do-print2 port #f))))

(define base-query-content-builder (class-field-accessor base-query% content-builder))
(define base-query-cycle-breaker (class-field-accessor base-query% cycle-breaker))

(define query%
  (class* base-query% ()
    (super-new)
    (inherit-field queryable)
    (inherit query-content attached-joins)

    (define/override (equal+hash-content)
      (list (query-content)
            queryable))

    (define/override (do-print port mode)
      (with-write-scope (cons this (attached-joins))
        (query-printer this port mode)))))

(define query-printer
  (make-constructor-style-printer
   (λ (me) 'from)
   (λ (me) (send me build-print-content #f))))


; This is used to bypass a very specific kind of infinite loop.
; Part of a base-query's content is a *deduplicated* list of attached joins.
; So in order to build the content, we need to be able deduplicate, which
; implies that we need to be able to test attached joins for equality.
; Normal join equality uses `join-target` which can cycle back into the
; content we are trying to build.
; So only during this one specific operation, we do equality differently.
(define deduplicating-attached-joins? (make-parameter #f))

(define join%
  (class* base-query% (join<%>)
    (super-new)
    (init-field link)
    (inherit-field queryable)
    (inherit query-content attached-joins)

    (define/override (equal+hash-content)
      (list (query-content)
            queryable
            (if (deduplicating-attached-joins?)
                ; During deduplication, use `eq?` comparison. This is acceptable
                ; because we know that J1 and J2 belong to the same query.
                (make-eq-shim link)
                ; During a normal comparison, J1 and J2 probably belong to
                ; different queries Q1 and Q2. In this situation, J1 and J2 can
                ; only be equal if Q1 and Q2 are also equal.
                (join-target this))))

    (define/override (do-print port mode)
      (let ([found (write-scope-find this)])
        (if found
            (write-string found port)
            (with-write-scope (cons this (attached-joins))
              (let ([content (join-shim this #f)])
                (case mode
                  [(#t) (write content port)]
                  [(#f) (display content port)]
                  [else (print content port mode)]))))))

    ; join<%>
    (define/public (join-link) link)))

(define join-printer
  (make-constructor-style-printer
   (λ (me) (if (join-shim-attached? me)
               'attach
               'join))
   (λ (me) (let ([j (join-shim-join me)]
                 [attached? (join-shim-attached? me)])
             (send j build-print-content attached?)))))

(struct join-shim (join attached?)
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (let ([j (join-shim-join me)])
       (with-write-scope (send j attached-joins)
         (join-printer me port mode))))])

(define (write-tuple me port mode)
  (define (helper name)
    (write-string (or name
                      (format "#<tuple: ~v>" (tuple-queryable me)))
                  port))
  (with-handlers ([exn:fail:premature-query-access?
                   (lambda (err) (helper #f))])
    ; If this tuple refers to an enclosing query or join, then try to use that name.
    ; The enclosing query or join should have registered itself into the namescope earlier.
    (let* ([q (:tuple-query me)]
           [name (and q (write-scope-find q))])
      (helper name))))

(define flatten-lists? (make-parameter #f))

(define (add-clause content clause)
  #;(-> content? any/c content?)
  (define (add-to-print content)
    (content-cons content :print-clauses clause))
  (cond
    [(and (list? clause)
          (flatten-lists?))
     (for/fold ([content content])
               ([clause clause])
       (add-clause content clause))]
    [(void? clause)
     content]
    [(clause? clause)
     (add-to-print (clause-apply clause content))]
    [else
     (add-to-print (content-cons content :clauses clause))]))

(define (add-join content join)
  #;(-> content? any/c content?)
  (content-cons content :joins join))

(define-syntax-parameter attach
  (λ (stx) (raise-syntax-error #f "used out of context" stx)))

(define-syntax (apply-all-no-special-forms stx)
  (syntax-case stx ()
    [(_ id content clause more ...)
     #`(apply-all id
                  (add-clause content clause)
                  more ...)]
    [(_ id content)
     ; This approach runs once per append.
     ; A slight optimization would be to only run `cleanup` once, when we know that
     ; we are at the top level (no more appending). Then we could reverse the clauses
     ; once instead of doing it on demand later.
     #'(cleanup content)]))

; Returns new content by applying statements in order to the given content.
(define-syntax (apply-all stx)
  (syntax-case stx (define attach)
    [(_ id content (attach join-id a b ...) more ...)
     #'(let ([join-id (:join join-id a #:to id b ...)])
         (apply-all id
                    (add-join content join-id)
                    more ...))]
    [(_ id content (define (proc-id arg ... . rest-arg) body ...) more ...)
     #'(let ([proc-id (lambda (arg ... . rest-arg) body ...)])
         (apply-all id content more ...))]
    [(_ id content (define (proc-id arg ...) body ...) more ...)
     #'(let ([proc-id (lambda (arg ...) body ...)])
         (apply-all id content more ...))]
    [(_ id content (define val-id val-expr) more ...)
     #'(let ([val-id val-expr])
         (apply-all id content more ...))]
    [(_ id content (macro stuff ...) more ...)
     (and (identifier? #'macro)
          (syntax-local-value #'macro (lambda () #f)))
     (let* ([expanded (local-expand #'(macro stuff ...)
                                    'module
                                    (list #'define #'attach))]
            [head (car (syntax->list expanded))]
            ; did we local-expand to something we will recognize?
            [recur? (or (free-identifier=? head #'define)
                        (free-identifier=? head #'attach))]
            [reapply (if recur?
                         #'apply-all
                         #'apply-all-no-special-forms)])
       #`(#,reapply id content #,expanded more ...))]
    [(_ stuff ...)
     #'(apply-all-no-special-forms stuff ...)]))

(define (cleanup content)
  #;(-> content? content?)
  (let ([joins (content-ref content :joins)])
    (content-set content :joins
                 (parameterize ([deduplicating-attached-joins? #t])
                   (remove-duplicates (reverse joins))))))

(define (clean-alias id)
  (let* ([id (format "~a" id)]
         [lst (string-split id "/")]
         [item0 (and (string? (car lst))
                     (car lst))])
    (or item0 id)))

; Returns 3 values
; 1) the queryable
; 2) the alias, or #f
; 3) the content-builder procedure (-> tuple? content?)
(define (get-stuff x)
  (cond
    [(base-query? x)
     ; Append
     (values (query-queryable x)
             (query-alias x)
             (base-query-content-builder x))]
    [(queryable? x)
     (get-stuff (unwrap-queryable x))]
    [else
     (values x
             #f
             (λ (tuple) (new-content)))]))

(define-syntax-rule (from tuple-id queryable-expr statement ...)
  (let*-values ([(qval) queryable-expr]
                [(queryable alias append-content-builder)
                 (get-stuff qval)]
                [(alias) (or alias
                             (clean-alias 'tuple-id))]
                [(params) (current-parameterization)]
                [(content-builder)
                 (λ (tuple)
                   (call-with-parameterization
                    params
                    (λ ()
                      ; For safety, use private-tuple-id in case the user
                      ; redefines tuple-id within this scope
                      (let* ([private-tuple-id tuple]
                             [tuple-id private-tuple-id])
                        (apply-all private-tuple-id
                                   (append-content-builder private-tuple-id)
                                   statement ...)))))])
    (new query%
         [content-builder content-builder]
         [alias alias]
         [queryable queryable])))

(define-syntax (:join stx)
  (syntax-case stx ()
    [(_ tuple-id queryable-expr #:to link-expr statement ...)
     (syntax/loc stx
       (let*-values ([(qval) queryable-expr]
                     [(queryable alias append-content-builder)
                      (get-stuff qval)]
                     [(alias) (or alias
                                  (clean-alias 'tuple-id))]
                     ; At this point, we expect `target` to be (or/c join? tuple?) and if it is a tuple,
                     ; we expect that it points to a query and not a join. We will determine the
                     ; "final target" on demand (by passing through simple joins).
                     [(link) link-expr]
                     [(params) (current-parameterization)]
                     [(content-builder)
                      (λ (tuple)
                        (call-with-parameterization
                         params
                         (λ ()
                           ; For safety, use private-tuple-id in case the user
                           ; redefines tuple-id within this scope
                           (let* ([private-tuple-id tuple]
                                  [tuple-id private-tuple-id])
                             (apply-all private-tuple-id
                                        (append-content-builder private-tuple-id)
                                        statement ...)))))])
         (new join%
              [content-builder content-builder]
              [alias alias]
              [queryable queryable]
              [link link])))]
    [(_ a b statement ...)
     (syntax/loc stx
       (:join a b #:to #f statement ...))]))

(module+ test
  (require rackunit)

  (define (print-bq bq)
    ; For some reason, ~v works fine in DrRacket, but via `raco test`
    ; it finds and prints cycles with #0=
    #;(~v bq)
    ; Pretty printing works around this issue.
    ; What is even stranger is that this workaround is not needed
    ; elsewhere, grep for search-key-t4290brfq
    (parameterize ([pretty-print-columns 'infinity])
      (let* ([port (open-output-string)])
        (pretty-print bq port)
        (string-trim (get-output-string port)))))

  ; does check-equal? and checks that the printable representations are also equal
  (define-syntax (check-same stx)
    (syntax-case stx ()
      [(_ AA BB)
       (quasisyntax/loc stx
         (let ([a AA]
               [b BB])
           #,(syntax/loc stx
               (check-equal? a b))
           #,(syntax/loc stx
               (check-equal? (print-bq a) (print-bq b)))))]))

  (check-same
   (from a "A" a a a)
   (from a "A" a a a))
  (check-equal?
   (from a "foo")
   (from b "foo"))
  (check-not-equal?
   (from a "A" (from b "B" a))
   (from a "A" (from b "B" b)))
  (check-same
   (from a "A" (from b "B" a))
   (from a "A" (from b "B" a)))
  (check-same
   (from a (from a "A" 1 a) 2 a)
   (from a "A" 1 a 2 a))
  (check-same
   (from a "A" 1 2 (void) 3 4) ; void is ignored
   (from a "A" 1 2 3 4))

  ; Do something that forces complete evaluation. Printing works.
  (define (force query) (~a query))

  (define-syntax (check-err stx)
    (syntax-case stx ()
      [(_ form)
       #`(let ([query form])
           #,(syntax/loc stx
               (check-exn exn:fail:premature-query-access?
                          (λ () (force query)))))]))

  ; Cannot do equal or hash until after evaluation.
  (check-err (from a "A" (from b "B" (equal? a b))))
  (check-err (from a "A" (equal-hash-code a)))

  ; This is interesting. Apparently our custom `equal?` proc is never called because they are `eq?`.
  ; I'm not sure if this is important, but I'll add a test just to let me know if I
  ; accidentally change this behavior.
  (check-not-exn (λ () (force (from a "A" (equal? a a)))))

  ; the duplicate attachment is ignored
  (check-same
   (from a "A"
         (attach b "B")
         (attach b2 "B")
         a b b2)
   (from a "A"
         (attach b "B")
         a b b))

  ; Test deduplication with appending
  (check-same
   (from a (from a "A"
                 (attach b "B")
                 (list a b))
         (attach b "B")
         (list a))
   (from a "A"
         (attach b "B")
         (list a b)
         (list a)))
  (check-same
   (from a (from a "A"
                 (attach b "B"))
         (attach b "B")
         (list a b)
         (list a))
   (from a "A"
         (attach b "B")
         (list a b)
         (list a)))

  ; test define with a rest arg
  (check-same
   (from a "A"
         (define (foo a b . rest)
           (list a b rest))
         (foo 1 2 3 4 5 a))
   (from a "A"
         (list 1 2 (list 3 4 5 a))))

  (define (str x)
    (if (base-query? x)
        (print-bq x)
        (substring (~v x) 1)))

  (check-equal?
   (str (from a "A"
              (attach b "B")
              (attach b2 "B")
              a b b2
              (list (:join c "C"
                           #:to a
                           a c))))
   (str '(from a/0 "A"
               (attach b/1 "B")
               a/0 b/1 b/1
               (list (join c/2 "C"
                           #:to a/0
                           a/0 c/2)))))

  (check-equal?
   (str (from a "A"
              (attach b "B"
                      (attach c "C"))))
   (str '(from a/0 "A"
               (attach b/1 "B"
                       (attach c/2 "C")))))

  (check-equal?
   (str (from a "A"
              (attach b "B"
                      (attach c "C")
                      (list a b c))))
   (str '(from a/0 "A"
               (attach b/1 "B"
                       (attach c/2 "C")
                       (list a/0 b/1 c/2)))))

  (void "end module"))
