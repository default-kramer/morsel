#lang racket

(provide base-query-printer write-scope-find)

(require "model.rkt"
         racket/struct)

; Maps base queries to unique names for printing
(define namescope (make-parameter (hash)))

(define-syntax-rule (with-write-scope base-queries body ...)
  (let* ([current-dict (namescope)]
         [new-dict (add-unique-names current-dict base-queries)])
    (parameterize ([namescope new-dict])
      body ...)))

; When we do a `with-write-scope` we need to make sure printing completely finishes
; before the parameterization ends. For some reason (maybe pretty-print related?)
; using a separate port accomplishes this.
; TODO it seems like this breaks pretty printing.
; Also, why does `raco test` work different than DrRacket?
(define-syntax-rule (write-now proc x port mode)
  (let* ([alt-port (open-output-string)]
         [_ (proc x alt-port mode)]
         [str (get-output-string alt-port)])
    (write-string str port)))

(struct base-query-printer (query) #:transparent
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (let ([me (base-query-printer-query me)])
       (with-write-scope (cons me (attached-joins me))
         (write-now write-query2 me port mode))))])

(define (query-printer-helper me [attached #f])
  ; build the list backwards
  (let* ([anchor (and (not attached)
                      (join? me)
                      (join-target me))]
         [anchor (if (query? anchor)
                     (query-tuple anchor)
                     anchor)]
         [lst (query-clauses/printable me)]
         [lst (append (map attached-join-print-shim (attached-joins me)) lst)]
         [lst (if anchor
                  (cons (symbol-printer '#:to) (cons anchor lst))
                  lst)]
         [lst (cons (query-queryable me) lst)]
         [lst (cons (query-tuple me) lst)])
    lst))

(define write-query2
  (make-constructor-style-printer
   (λ (me) (if (join? me) 'join 'from))
   (λ (me) (query-printer-helper me))))

(define attached-join-printer
  (make-constructor-style-printer
   (lambda (me) 'attach)
   (lambda (me)
     (let ([join (attached-join-print-shim-join me)])
       (query-printer-helper join #t)))))

(struct attached-join-print-shim (join) #:transparent
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (let* ([join (attached-join-print-shim-join me)])
       (with-write-scope (attached-joins join)
         (write-now attached-join-printer me port mode))))])

(struct symbol-printer (sym) #:transparent
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (let ([me (symbol-printer-sym me)])
       (write-string (~a me) port)))])

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
