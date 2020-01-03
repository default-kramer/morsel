#lang racket

(provide render-root query? make-query join? make-join)

(require "dialect.rkt")

(struct query (table alias
                     selects joins wheres group-bys havings order-bys
                     distinct? limit offset
                     ) #:transparent)

(struct join (type query join-ons) #:transparent)

(define (make-query #:table [table "table"]
                    #:alias [alias "alias"]
                    #:selects [selects (list)]
                    #:joins [joins (list)]
                    #:wheres [wheres (list)]
                    #:group-bys [group-bys (list)]
                    #:havings [havings (list)]
                    #:order-bys [order-bys (list)]
                    #:distinct? [distinct? #f]
                    #:limit [limit #f]
                    #:offset [offset #f])
  (query table alias
         selects joins wheres group-bys havings order-bys
         distinct? limit offset))

(define (make-join query
                   #:type [type 'inner]
                   #:join-ons [join-ons (list)])
  (join type query join-ons))

; A join is simple if its query has no clauses
(define (simple? x)
  (cond
    [(join? x) (simple? (join-query x))]
    [(query? x) (equal? x (make-query #:table (query-table x)
                                      #:alias (query-alias x)))]
    [else #f]))

(define the-port (make-parameter #f))

(define indentation (make-parameter ""))

(define (get-spaces x)
  (cond
    [(integer? x) (make-string x #\space)]
    [(string? x) (get-spaces (string-length x))]
    [else (error "morsel bug")]))

(define-syntax (indent stx)
  (syntax-case stx ()
    [(_ #:size size body ...)
     (syntax/loc stx
       (parameterize ([indentation (string-append (get-spaces size) (indentation))])
         body ...))]
    [(_ body ...)
     (syntax/loc stx
       (indent #:size 2 body ...))]))

(define (-write str)
  (when newline?
    (set! newline? #f)
    (-write "\n")
    (-write (indentation)))
  (write-string str (the-port)))

(define newline? #f)
(define (newline)
  (set! newline? #t))

(define-syntax (write stx)
  (syntax-case stx ()
    [(_ str)
     (syntax/loc stx
       (-write str))]
    [(_ fmt arg ...)
     (syntax/loc stx
       (write (format fmt arg ...)))]))

(define-syntax (writeln stx)
  (syntax-case stx ()
    [(_ str)
     (syntax/loc stx
       (begin
         (write str)
         (newline)))]
    [(_ fmt arg ...)
     (syntax/loc stx
       (writeln (format fmt arg ...)))]))

(define (render-root x [port #f])
  (set! newline? #f)
  (if port
      (parameterize ([the-port port])
        (if (query? x)
            ; Skip indentation if the root is a query
            (render-query x)
            (render x))
        (void))
      (let ([port (open-output-string)])
        (render-root x port)
        (get-output-string port))))

(define (render x)
  (cond
    [(query? x)
     (indent
      (newline)
      (render-query x))]
    [(join? x)
     (render-join x)]
    [(string? x)
     (write x)]
    [(list? x)
     (map render x)]
    [else
     (error "cannot render" x)])
  (void))

(define (render-query q)
  (let* ([selects (query-selects q)]
         [limit (query-limit q)]
         [offset (query-offset q)]
         [dialect (current-dialect)]
         [ms? (mssql? dialect)]
         ; for SQL Server, if we have a limit but no offset, we can render the limit
         ; as "top" instead for better compatibility
         [top (and ms? (not offset) limit)]
         [select-intro ; will be "select [distinct] [top N]"
          (string-append "select"
                         (if (query-distinct? q)
                             " distinct"
                             "")
                         (if top
                             (format " top ~a" top)
                             ""))])
    ; dialect validation
    (when (not dialect)
      (when limit
        (error "cannot render limit without current-dialect"))
      (when offset
        (error "cannot render offset without current-dialect")))
    ; select
    (if (empty? selects)
        (writeln "~a ~a.*" select-intro (query-alias q))
        (begin
          (write "~a " select-intro)
          (render-clause (car selects))
          (newline)
          (indent
           (for/list ([select (cdr selects)])
             (write ", ")
             (render-clause select)
             (newline)))))
    ; from
    (write "from ")
    (render (query-table q))
    (writeln " ~a" (query-alias q))
    ; join
    (map render-join (query-joins q))
    ; where
    (multi-line-style "where "
                      "  and "
                      (query-wheres q))
    ; group by
    (single-line-style "group by" (query-group-bys q))
    (newline)
    ; having
    (multi-line-style "having "
                      "   and "
                      (query-havings q))
    ; order by
    (single-line-style "order by" (query-order-bys q))
    (newline)
    ; non-MS uses "limit" and "offset"
    (when (not ms?)
      (when limit
        (writeln "limit ~a" limit))
      (when offset
        (writeln "offset ~a" offset)))
    ; MS uses "offset" and "fetch next"
    (when ms?
      (when offset
        (writeln "offset ~a rows" offset))
      (when (and limit (not top))
        (writeln "fetch next ~a rows only" limit)))))

(define (single-line-style intro clauses)
  (when (not (empty? clauses))
    (write intro)
    (write " ")
    (render-clause (car clauses))
    (for/list ([clause (cdr clauses)])
      (write ", ")
      (render-clause clause))))

(define (multi-line-style intro separator clauses)
  (when (not (empty? clauses))
    (write intro)
    (render-clause (car clauses))
    (newline)
    (for/list ([clause (cdr clauses)])
      (write separator)
      (render-clause clause)
      (newline))))

(define (render-join j)
  (let* ([q (join-query j)]
         [type (join-type j)]
         [table (query-table q)]
         [alias (query-alias q)])
    (if (simple? q)
        (writeln "~a join ~a ~a" type table alias)
        (begin
          (write "~a join (" type)
          (render q)
          (writeln ") ~a" alias)))
    (multi-line-style "   on "
                      "  and "
                      (join-join-ons j))))

(define (render-clause c)
  (render c))

(module+ test
  (require rackunit)

  (define j1 (make-join (make-query #:table "Bar" #:alias "b")
                        #:join-ons (list "b.Id = f.Id" "b.Blah is not null")))
  (define j2 (make-join (make-query #:table "Baz" #:alias "baz" #:selects (list "inner"))
                        #:join-ons (list "baz.Id = b.Id" "baz.baz2 = 99")))
  (define q (make-query #:table "Foo"
                        #:alias "f"
                        #:joins (list j1 j2)
                        #:selects '("s1" "s2" "s3")
                        #:wheres '("1=1" "2=2" "3=3")
                        #:group-bys '("gb1" "gb2" "gb3")
                        #:havings '("have1" "have2" "have3")
                        #:order-bys '("sort1" "sort2" "sort3")))
  (check-equal? (render-root q) #<<HEREDOC
select s1
  , s2
  , s3
from Foo f
inner join Bar b
   on b.Id = f.Id
  and b.Blah is not null
inner join (
  select inner
  from Baz baz
) baz
   on baz.Id = b.Id
  and baz.baz2 = 99
where 1=1
  and 2=2
  and 3=3
group by gb1, gb2, gb3
having have1
   and have2
   and have3
order by sort1, sort2, sort3
HEREDOC
                )

  (void "end module"))
