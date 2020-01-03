#lang racket

(provide content?
         new-content   ; (-> content?)
         content-set   ; (-> content? property? any/c content?)
         content-cons  ; (-> content? property? any/c content?)
         content-ref   ; like dict-ref
         property?
         make-property)

(define none (gensym))

; The `hidden?` flag controls whether this property matters for `equal?`
(struct property (name default-value hidden?)
  #:methods gen:custom-write
  [(define (write-proc me port mode)
     (write-string (format "#~a" (property-name me)) port))])

(define (make-property #:name [name (gensym "content:")]
                       #:default [default-value (list)]
                       #:hidden [hidden? #f])
  (property name default-value hidden?))


(define-syntax-rule (content-stuff content)
  (content-hash content))

; We want our content to be super dynamically extensible.
; So content pretty much has to be a dict of some kind.
(struct content (hash hidden-hash) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?)
     (equal? (content-stuff a)
             (content-stuff b)))
   (define (hash-proc a hasher)
     (hasher (content-stuff a)))
   (define (hash2-proc a hasher)
     (hasher (content-stuff a)))])

(define (new-content)
  (content (hasheq) (hasheq)))

(define (choose-hash content prop)
  (if (property-hidden? prop)
      (content-hidden-hash content)
      (content-hash content)))

(define-syntax-rule (update c prop orig-hash new-hash-expr)
  (let* ([hidden? (property-hidden? prop)]
         [orig-hash (choose-hash c prop)]
         [new-hash new-hash-expr])
    (if hidden?
        (struct-copy content c
                     [hidden-hash new-hash])
        (struct-copy content c
                     [hash new-hash]))))

(define (content-ref content prop [failure-result none])
  (let ([hash (choose-hash content prop)])
    (if (eq? none failure-result)
        (hash-ref hash prop (property-default-value prop))
        (hash-ref hash prop failure-result))))

(define (content-set content prop val)
  (let ([default (property-default-value prop)])
    (update content prop orig-hash
            (if (equal? val default)
                (hash-remove orig-hash prop)
                (hash-set orig-hash prop val)))))

(define (content-cons content prop val)
  (update content prop orig-hash
          (let* ([lst (hash-ref orig-hash prop none)]
                 [lst (if (eq? none lst)
                          (property-default-value prop)
                          lst)]
                 [lst (cons val lst)])
            (hash-set orig-hash prop lst))))
