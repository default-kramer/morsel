#lang racket

(provide with-write-scope write-scope-find symbol-printer)

(require (only-in "model.rkt" query-alias))

; Maps base queries to unique names for printing
(define namescope (make-parameter (hash)))

(define-syntax-rule (with-write-scope base-queries body ...)
  (let* ([current-dict (namescope)]
         [new-dict (add-unique-names current-dict base-queries)])
    (parameterize ([namescope new-dict])
      body ...)))

; Register each base-query into the dictionary with a unique name
(define (add-unique-names dict base-queries)
  #;(-> dict? (listof base-query?) dict?)
  (match base-queries
    [(list) dict]
    [(list bq rest ...)
     (cond
       [(dict-ref dict bq #f)
        ; already in dict, just continue
        (add-unique-names dict rest)]
       [else
        (let* ([name (query-alias bq)]
               [unique-name (format "~a/~a"
                                    name
                                    (dict-count dict))]
               [new-dict
                (dict-set dict bq unique-name)])
          (add-unique-names new-dict rest))])]))

(define (write-scope-find bq)
  (dict-ref (namescope) bq #f))

(struct symbol-printer (sym) #:transparent
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (let ([me (symbol-printer-sym me)])
       (write-string (~a me) port)))])
