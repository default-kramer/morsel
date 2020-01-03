#lang racket

; Given a base query BQ, returns all the joins found within BQ whose `join-target` is BQ
(provide targeted-joins)

(require "pass.rkt"
         (only-in "model.rkt" base-query? join? join-target))

(define cache-key (gensym 'targeted-joins))

; dict is a mapping from a base query to its list of joins
(struct collector (dict) #:transparent #:mutable)

; There is one collector for the entire pass
(define current-collector (make-parameter #f))

; If the given join J has a target T, add J to T's list of joins
(define (on-found-join! j)
  (let ([target (join-target j)])
    (when target
      (let* ([coll (current-collector)]
             [dict (collector-dict coll)]
             [joins (dict-ref dict target (list))]
             [joins (cons j joins)]
             [dict (dict-set dict target joins)])
        (set-collector-dict! coll dict)))))

(define-pass (pass:targeted-joins x)
  (cond
    [(base-query? x)
     (begin
       (when (join? x)
         (on-found-join! x))
       (next x)
       ; Cache list of joins to query. WARNING! I thought it might work to save this until the
       ; very end. Just iterate over the key-value pairs and do `write-joins!` then.
       ; But this strategy would fail if two queries q1 and q2 are `equal?` but not `eq?`
       ; because only one of them would get written to.
       (do-cache! x))]
    [else (next x)]))

(define (do-cache! token)
  (let* ([coll (current-collector)]
         [dict (collector-dict coll)]
         [joins (dict-ref dict token (list))])
    (cache! token cache-key (remove-duplicates joins))))

(define (targeted-joins x)
  (get/cache! x cache-key #:default (list) #:body
              (parameterize ([current-collector (collector (hash))])
                (pass:targeted-joins x))))
