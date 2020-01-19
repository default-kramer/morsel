#lang racket

(require rackunit
         morsel-lib
         morsel-lib/sql)

(struct my-queryable (item) #:transparent
  #:methods gen:queryable
  [(define (unwrap-queryable me)
     (my-queryable-item me))])

; Do not fully unwrap. Need to append here:
(check-equal? (from x (my-queryable (from x "X" 1 2 3)))
              (from x "X" 1 2 3))



(define frag%
  (class* object% (sql-token<%>)
    (init-field kind
                content
                reduction)
    (super-new)

    ; token<%>
    (define/public (token-kind) kind)
    (define/public (token-content) content)
    (define-token-aspect-stuff)
    (define/public (sql-token-reduce) reduction)))

(let ([q
       (from x "X"
             (attach y "Y"
                     (group-by (scalar y".XID"))
                     (join-on (scalar y".XID")" = "(scalar x".XID")))
             (select (new frag%
                          [kind 'aggregate]
                          [content (scalar y".Blah")]
                          ; regression test: the reduction was not being honored when
                          ; this fragment was being injected (it was using content instead)
                          [reduction (list "sum(" (scalar y".Blah") ")")])))])

  (check-equal? (to-sql q) #<<HEREDOC
select y.__INJECT1
from X x
inner join (
  select y.XID as __INJECT0
    , sum(y.Blah) as __INJECT1
  from Y y
  group by y.XID
) y
   on y.__INJECT0 = x.XID
HEREDOC
                ))
