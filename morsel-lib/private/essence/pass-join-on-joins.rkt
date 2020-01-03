#lang racket

; This pass calculates all the joins found within join-on clauses of a base query.
; It is used by the "infer join type" logic.
(provide join-on-joins)

(require "pass.rkt"
         (only-in "model.rkt" base-query? join?))

(define cache-key (gensym 'join-on-joins))

(define join-collector (make-parameter (list)))

(define-pass (pass:join-on-joins x)
  (cond
    [(base-query? x)
     (begin
       (when (and (join? x)
                  (in-join-clause?))
         (join-collector (cons x (join-collector))))
       (parameterize ([join-collector (list)])
         (next x)
         (cache! x cache-key (remove-duplicates (join-collector)))))]
    [else (next x)]))

(define (join-on-joins x)
  (get/cache! x cache-key #:default (list) #:body (pass:join-on-joins x)))
