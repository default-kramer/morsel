#lang racket

(provide select where group-by having order-by join-on
         scalar aggregate bool subquery sql silence
         limit offset distinct join-type
         :limit :offset :distinct get-join-type)

(module+ TODO-for-plisqin
  (provide clause<%> content-set :limit :offset :distinct :join-type))

(require racket/struct
         "sql-token.rkt"
         "../_essence.rkt")

(define fragment%
  (class* object% (sql-token<%> equal<%> printable<%>)
    (init-field key content)
    (super-new)

    ; sql-token<%>
    (define/public (token-kind) key)
    (define/public (token-content) content)
    (define-token-aspect-stuff)
    (define/public (sql-token-reduce)
      (cons key content))

    ; equal<%>
    (define/public (equal-content)
      (list (token-kind) (token-content)))
    (define/public (equal-to? other recur)
      (recur (equal-content) (send other equal-content)))
    (define/public (equal-hash-code-of hasher)
      (hasher (equal-content)))
    (define/public (equal-secondary-hash-code-of hasher)
      (hasher (equal-content)))

    ; printable<%>
    (define/public (custom-print port mode)
      (fragment-printer this port mode))
    (define/public (custom-write port)
      (fragment-printer this port #t))
    (define/public (custom-display port)
      (fragment-printer this port #f))))

(define (fragment? x)
  (is-a? x fragment%))
(define fragment-key (class-field-accessor fragment% key))
(define fragment-content (class-field-accessor fragment% content))

(define fragment-printer
  (make-constructor-style-printer fragment-key fragment-content))

(define-syntax-rule (def-clauses id ...)
  (begin
    (define (id . args)
      (new fragment% [key 'id] [content args]))
    ...))

(def-clauses select where group-by having order-by join-on)

; These are fragments, not clauses, but there is no difference now
(def-clauses scalar aggregate bool subquery sql silence)




;; ==========
;; A setter is a clause<%> that writes a property value into the content
;; ==========
(define setter%
  ; We don't need to implement equal<%> because anything we do should be captured
  ; when we modify the content.
  (class* object% (clause<%> printable<%>)
    (init-field prop val print-name)
    (super-new)

    (define/public (apply content)
      (content-set content prop val))

    ; printable<%>
    (define/public (custom-print port mode)
      (setter-printer this port mode))
    (define/public (custom-write port)
      (setter-printer this port #t))
    (define/public (custom-display port)
      (setter-printer this port #f))))

(define setter-print-name (class-field-accessor setter% print-name))
(define setter-val (class-field-accessor setter% val))

(define setter-printer
  (make-constructor-style-printer
   setter-print-name
   (λ (me) (list (setter-val me)))))

(define-syntax-rule (define-setters [prop-id id] ...)
  (begin
    (define prop-id (make-property #:name 'id #:default #f))
    ...
    (define (id x)
      (new setter% [prop prop-id] [val x] [print-name 'id]))
    ...))

(define-setters
  [:limit limit]
  [:offset offset]
  [:distinct distinct]
  ; join-type of #f means "infer" which is the default
  [:join-type join-type])

(define (get-join-type x)
  (let ([my-type (property-value x :join-type)])
    (or my-type
        ; infer
        (let* ([joins (join-on-joins x)]
               [types (map get-join-type joins)])
          (if (member 'left types)
              'left
              'inner)))))
