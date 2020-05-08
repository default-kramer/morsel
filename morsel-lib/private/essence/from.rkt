#lang racket

(provide from (rename-out [:join join]) attach)

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

(define base-query%
  (class* object% (base-query<%> equal<%> printable<%> never-quote<%>)
    ; content-builder is a function of (-> tuple? content?)
    ; This makes appending easy - we just call the original content-builder with our
    ; new tuple, then append to it.
    (init-field content-builder alias queryable)
    (super-new)
    (define my-tuple (tuple this alias queryable))

    (define content #f)
    (define content-loop-detector #f)
    (define/public (query-content)
      (or content
          (begin
            (when content-loop-detector
              (raise (exn:fail:premature-query-access msg (current-continuation-marks))))
            (set! content-loop-detector #t)
            (set! content (content-builder my-tuple))
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
    (define/public (equal-to? other recur)
      (recur (equal+hash-content) (send other equal+hash-content)))
    (define/public (equal-hash-code-of hasher)
      (hasher (equal+hash-content)))
    (define/public (equal-secondary-hash-code-of hasher)
      (hasher (equal+hash-content)))

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
    (define/public (custom-print port mode)
      (do-print port mode))
    (define/public (custom-write port)
      (do-print port #t))
    (define/public (custom-display port)
      (do-print port #f))))

(define base-query-content-builder (class-field-accessor base-query% content-builder))

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

(define join%
  (class* base-query% (join<%>)
    (super-new)
    (init-field target)
    (inherit-field queryable)
    (inherit query-content attached-joins)

    (define/override (equal+hash-content)
      (list (query-content)
            queryable
            (join-target this)))

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
    (define/public (join-link) target)))

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
  ; If this tuple refers to an enclosing query or join, then try to use that name.
  ; The enclosing query or join should have registered itself into the namescope earlier.
  (let* ([q (:tuple-query me)]
         [name (and q (write-scope-find q))])
    (write-string (or name
                      (format "#<tuple: ~v>" (tuple-queryable me)))
                  port)))

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
                 (remove-duplicates (reverse joins)))))

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
                      (let ([tuple-id tuple])
                        (apply-all tuple-id (append-content-builder tuple-id)
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
                           (let ([tuple-id tuple])
                             (apply-all tuple-id (append-content-builder tuple-id)
                                        statement ...)))))])
         (new join%
              [content-builder content-builder]
              [alias alias]
              [queryable queryable]
              [target link])))]
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
