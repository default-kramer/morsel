#lang racket

(provide calc-injections ; (-> token procedure?)
         might-need-injection?
         injection? injection-index injection-grouped-join)

(require "model.rkt" "pass.rkt")

; The `calc-injections` pass explores the object and returns a procedure that
; 1) when given a join, returns a list of tokens that should be injected
; 2) when given anything else, returns (or/c #f injection?)
; The SQL renderer uses #1 to add `select` clauses to the join
; The SQL renderer uses #2 to know whether a given token should be replaced by a token
; that refers to an additional `select` clauses from #1.
; We also provide `(might-need-injection? token)` which can quickly say "no, that token
; does not need injection, save yourself the time" which should be faster than the dict
; lookup that would otherwise be needed.
;
; Note that there is a certain "globalness" to this problem - the number of tokens
; that needs to be injected into a join depends on stuff *outside* of that join.
; So it is not possible to make "injections" a property of a join.
; Instead, the injection-registry is a property of any root object.

; An `injection` is
;   index : an integer which says "I am the Nth injection into that grouped join"
(struct injection (index grouped-join) #:transparent)

; An `injection-registry` is
;   join=>tokens : dictionary that maps a grouped join to a unique list of tokens that
;   need to be injected into that grouped join
;   token=>injection : dictionary that maps a token to an injection
; Both dictionaries are mutable; we will build them up as we explore.
(struct injection-registry (join=>tokens token=>injection) #:transparent)

(define (new-injection-registry)
  (injection-registry (make-hash) (make-hash)))

(define current-injection-registry (make-parameter #f))

; Given a join, return the list of tokens to be injected.
; Given anything else, return (or/c #f injection?)
(define (injection-registry-lookup reg x)
  (cond
    [(join? x)
     (reverse (dict-ref (injection-registry-join=>tokens reg) x (list)))]
    [else
     (dict-ref (injection-registry-token=>injection reg) x #f)]))

(define aspect-key:might-need-injection (gensym 'might-need-injection))

(define (might-need-injection? x)
  (and (token? x)
       (get-token-aspect x aspect-key:might-need-injection #f)))

; Mutates the current injection-registry to record the fact that the given
; token should be injected into the given grouped join.
(define (register! token grouped-join)
  (set-token-aspect! token aspect-key:might-need-injection #t)
  (let* ([registry (or (current-injection-registry)
                       (error "morsel bug 9gf42as"))]
         [join=>tokens (injection-registry-join=>tokens registry)]
         [join-tokens (dict-ref! join=>tokens grouped-join (list))])
    (if (member token join-tokens)
        (void) ; already registered
        (let ([index (length join-tokens)]
              [token=>injection (injection-registry-token=>injection registry)])
          (dict-set! join=>tokens grouped-join (cons token join-tokens))
          (dict-set! token=>injection token (injection index grouped-join))))))


; ===
; Aggregate Injection
; ===
; When we see an aggregate, we begin a new agg-tracker
;   targets : mutable list of grouped joins this aggregate is targeting
; When we are done, we hope to see one unique grouped join in the list of targets.
(struct agg-tracker (targets) #:transparent #:mutable)
(define (new-agg-tracker)
  (agg-tracker (list)))

(define agg-tracker-stack (make-parameter '()))

(define-step (step:agg x)
  (cond
    [(aggregate-injectable? x)
     (let ([tracker (new-agg-tracker)])
       (parameterize ([agg-tracker-stack (cons tracker (agg-tracker-stack))])
         (next x))
       ; check if the tracker found exactly 1 target
       (let ([targets (remove-duplicates (agg-tracker-targets tracker))])
         (match targets
           [(list grouped-join)
            (register! x grouped-join)]
           [else (void)])))]
    [(grouped-join? x)
     (let* ([stack (agg-tracker-stack)]
            [tracker (and (pair? stack) (car stack))])
       (if (not tracker)
           (next x)
           (begin
             ; add this grouped join to the current tracker
             (set-agg-tracker-targets! tracker
                                       (cons x (agg-tracker-targets tracker)))
             ; pop the current tracker off the stack
             (parameterize ([agg-tracker-stack (cdr stack)])
               (next x)))))]
    [else (next x)]))


; ===
; Scalar Injection
; ===
; When we encounter a scalar, we might start a new scalar-inject-tracker to keep track
; of whether that scalar can be injected. A scalar-inject-tracker is
;   grouped-join  : the join that we are proposing to inject into
;   has-selfref?  : a boolean that tracks whether we have seen a self reference (from #f to #t)
;   could-inject? : a boolean that tracks if any content prevents injection (from #t to #f)
; We mutate this as we explore the content of the scalar.
(struct scalar-inject-tracker (grouped-join has-selfref? could-inject?) #:transparent #:mutable)

(define (new-si-tracker grouped-join)
  (scalar-inject-tracker grouped-join #f #t))

; stack of scalar-inject-tracker
(define sit-stack (make-parameter (list)))

(define (can-inject? tracker)
  (and (scalar-inject-tracker-has-selfref? tracker)
       (scalar-inject-tracker-could-inject? tracker)))

; Suppress all the proposed injections in the stack
(define (suppress!)
  (for ([sit (sit-stack)])
    (set-scalar-inject-tracker-could-inject?! sit #f)))

; Is it possible that this scalar might need injection?
(define (should-track-scalar-injection? x)
  (and (scalar-injectable? x)
       (in-join-clause?)
       (in-grouped-join?)))

; All tuples within the scalar must be "resolved". A tuple is resolved if it was created
; by 1) the target grouped join, or 2) a simple join within the target. Example:
#;(from a "A"
        ; This (attach b ....) is the target grouped join:
        (attach b "B"
                (group-by (scalar b".Foo"))
                ; this scalar is not eligible because `a` is unresolved:
                (join-on (scalar a".Foo")" = 0")
                ; this scalar is eligible because `b` is resolved:
                (join-on (scalar b".Foo")" = 0")
                ; this scalar is eligible because `b` and `c` are both resolved:
                (join-on (scalar (join c "C" #:to b
                                       (join-on c".CID = "b".CID"))
                                 ".Foo")
                         " = 0")))
(define (resolved? tuple target ctx-stack)
  (if (not (pair? ctx-stack))
      #f
      (let* ([head (car ctx-stack)]
             [head (context-base-query head)])
        (cond
          [(eq? (tuple-query tuple) head)
           ; resolved because we found the join that introduced this tuple
           #t]
          [(eq? target head)
           ; unresolved because we reached the target and should not go higher
           #f]
          [else
           (resolved? tuple target (cdr ctx-stack))]))))

; Mutate the tracker stack if the given content affects the proposed injections
(define (check-tuple! tuple)
  ; The grouped join should be the same throughout the stack, so just grab the first one.
  (define gj (and (pair? (sit-stack))
                  (let ([sit (car (sit-stack))])
                    (scalar-inject-tracker-grouped-join sit))))
  (when gj
    (cond
      [(equal? (tuple-query tuple) gj)
       (for ([si-tracker (sit-stack)])
         (set-scalar-inject-tracker-has-selfref?! si-tracker #t))]
      [(not (resolved? tuple gj (context-stack)))
       (suppress!)]
      [else (void)])))

(define-step (step:scalar x)
  (cond
    ; A scalar containing a simple join could still be injectable...
    [(simple-join? x)
     (next x)]
    ; ... but any other kind of base-query is not allowed
    [(base-query? x)
     (begin
       ; We don't actually *need* to suppress here. It is valid SQL to inject a
       ; self-contained subquery. But that could get very confusing.
       ; So suppress for now, and revisit if any real-world need appears.
       (suppress!)
       ; We need to begin a new sit-stack
       (parameterize ([sit-stack (list)])
         (next x)))]
    [(should-track-scalar-injection? x)
     (let* ([grouped-join (context-base-query (car (context-stack)))]
            [tracker (new-si-tracker grouped-join)])
       (parameterize ([sit-stack (cons tracker (sit-stack))])
         (next x)
         (when (can-inject? tracker)
           (register! x grouped-join)
           ; Because we are injecting x, we want to suppress everything else up the stack
           (suppress!))))]
    [(tuple? x)
     (begin
       (check-tuple! x)
       (next x))]
    [else
     (next x)]))


; ===
; wire up scalar and aggregate injection
; ===
(define aspect-key:injections (gensym 'injections))

(define pass:calc-injections (steps->pass step:scalar step:agg))

(define (calc-injections token)
  (let ([cached (get-token-aspect token aspect-key:injections #f)])
    (or cached
        (let ([reg (new-injection-registry)])
          (parameterize ([current-injection-registry reg])
            (pass:calc-injections token)
            (let ([the-proc (curry injection-registry-lookup reg)])
              (set-token-aspect! token aspect-key:injections the-proc)
              the-proc))))))
