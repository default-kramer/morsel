#lang racket

(require rackunit
         racket/stxparam
         (only-in morsel-lib
                  from join attach))

(define-syntax-rule (def-stxparms id ...)
  (begin
    (define-syntax-parameter id
      (λ (stx) (raise-syntax-error 'id "not parameterized")))
    ...))

(def-stxparms actual expected expected-datum relocate)

;;;;; The `test` macro works like this:
#;(test (from a "A")
        (from b "B")
        stuff ...)
; It will bind
; - actual           to (from a "A")
; - expected         to (from b "B")
; - expected-datum   to '(from b "B")
;
; And it will wire up `relocate` such that we can do
#;(relocate (check-equal? foo bar))
; to copy the source location of the enclosing (test ...) to the check.
; This helps us know which test failed.
; Using `syntax-parameterize` means we have to use macros more than normal, but that's OK.
(define-syntax (test orig-stx)
  (syntax-case orig-stx ()
    [(_ actual-expr expected-expr body ...)
     (quasisyntax/loc orig-stx
       (let ([act actual-expr]
             [exp expected-expr]
             [expd 'expected-expr])
         (syntax-parameterize
             ([relocate (λ (stx)
                          (syntax-case stx ()
                            [(_ a)
                             (datum->syntax stx (syntax-e #'a) #'#,orig-stx)]))]
              [actual (λ (stx) #'act)]
              [expected (λ (stx) #'exp)]
              [expected-datum (λ (stx) #'expd)])
           body ...)))]))

(define (->string x #:pretty [width #f])
  (if width
      ; pretty print
      (parameterize ([pretty-print-columns width])
        (let ([port (open-output-string)])
          (pretty-print x port)
          (get-output-string port)))
      ; regular print
      (let ([port (open-output-string)])
        (print x port)
        (get-output-string port))))

(define (->datum x #:pretty [width #f])
  (let ([str (->string x #:pretty width)])
    (with-handlers ([exn:fail? (λ (ex)
                                 (write-string str)
                                 (error "was unreadable"))])
      (read (open-input-string str)))))

; return #f if test fails
(define-syntax-rule (check-pretty-impl width)
  (let* ([datum (->datum actual #:pretty width)]
         [good? (equal? datum expected-datum)])
    (if good?
        #t
        (let ([msg (format "(in check-pretty-impl, width ~a)" width)])
          (relocate (check-equal? datum expected-datum msg))
          #f))))

; check-pretty with widths from 80 down to 1
(define-syntax-rule (check-datum/pretty)
  (letrec ([checker
            (λ (width)
              (and (check-pretty-impl width)
                   (width . > . 1)
                   (checker (sub1 width))))])
    (checker 80)
    (void)))

(define-syntax-rule (check-datum)
  (let ([d (->datum actual)])
    (relocate (check-equal? d expected-datum "check-datum"))))

(define-syntax-rule (check-same)
  (begin
    (relocate (check-equal? actual expected
                            "check-same, direct equality"))
    ; I don't know why ~v works here but needs a workaround
    ; elsewhere, grep for search-key-t4290brfq
    (relocate (check-equal? (~v actual) (~v expected)
                            "check-same, ~v equality"))))

(define-syntax-rule (check-all)
  (begin
    (check-same)
    (check-datum)
    (check-datum/pretty)))

(test (from a "A"
            (list a 1 a 2 a 3)
            (list a 'hi))
      (from a/0 "A"
            (list a/0 1 a/0 2 a/0 3)
            (list a/0 'hi))
      (check-all))

(test (from a "A"
            (attach b "B"
                    (attach c "C")
                    (list a b c)))
      (from a/0 "A"
            (attach b/1 "B"
                    (attach c/2 "C")
                    (list a/0 b/1 c/2)))
      (check-all))
