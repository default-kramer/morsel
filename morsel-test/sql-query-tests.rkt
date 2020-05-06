#lang racket

(module+ test
  (require rackunit)

  ; To maximize the reusability of these tests, try to minimize what we require
  (require (only-in morsel-lib
                    from join attach)
           (only-in morsel-lib/sql
                    to-sql
                    join-on select group-by where
                    scalar aggregate subquery
                    distinct join-type))

  (define (avg . tokens)
    (aggregate "avg(" tokens ")"))

  (define (exists . tokens)
    ; it would be more correct to use bool instead of scalar but it doesn't matter here
    (scalar "exists " (subquery tokens)))

  (define-syntax (check-sql stx)
    (syntax-case stx ()
      [(_ q sql)
       (syntax/loc stx
         (check-equal? (to-sql q) sql))]))

  (define q #f)
  (define expected #f)

  (set! q
        (from p "Product"
              (attach c "Category"
                      (join-on c".CategoryId = "p".ProductId")
                      (join-on c".Foo = 'Bar'"))
              (select p".ProductName")
              (select c".CategoryName")))
  (check-sql q #<<HEREDOC
select p.ProductName
  , c.CategoryName
from Product p
inner join Category c
   on c.CategoryId = p.ProductId
  and c.Foo = 'Bar'
HEREDOC
             )

  (set! q
        (from p "Product"
              (define (Category p)
                (join c "Category"
                      #:to p
                      (join-on c".CategoryId = "p".ProductId")))
              (select p".ProductName")
              (select (Category p)".CategoryName")
              (select (Category p)".CategoryId")))
  (check-sql q #<<HEREDOC
select p.ProductName
  , c.CategoryName
  , c.CategoryId
from Product p
inner join Category c
   on c.CategoryId = p.ProductId
HEREDOC
             )

  (define (X a)
    (join x "X"
          #:to a
          (join-on x".XID = "a".XID")))
  (define (Y a)
    (join y "Y"
          #:to a
          (join-on y".YID = "a".YID")))
  (set! q
        (from a "A"
              (select (Y (X a))".bar")
              (select (X a)".foo")))
  (check-sql q #<<HEREDOC
select y.bar
  , x.foo
from A a
inner join X x
   on x.XID = a.XID
inner join Y y
   on y.YID = x.YID
HEREDOC
             )

  ; Nested attach
  (set! q (from x "X"
                (attach z "Z"
                        (attach y "Y"
                                (join-on y".YID = "x".YID"))
                        (join-on z".ZID = "y".ZID"))
                (select x".*")))
  (check-sql q #<<HEREDOC
select x.*
from X x
inner join Y y
   on y.YID = x.YID
inner join Z z
   on z.ZID = y.ZID
HEREDOC
             )

  (set! q
        (from a "A"
              (select (Y (X a))".bar")
              (select (X a)".foo")
              (attach y (Y (X a)))
              (select y".baz")))
  (check-sql q #<<HEREDOC
select y.bar
  , x.foo
  , y.baz
from A a
inner join X x
   on x.XID = a.XID
inner join Y y
   on y.YID = x.YID
HEREDOC
             )

  ; No scalar injection should happen because the scalars do not refer to `b`
  (set! q
        (from a "A"
              (attach b "B"
                      (group-by (scalar "Foo"))
                      (join-on (scalar "Foo")" = "(scalar a".Bar")))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select b.*
  from B b
  group by Foo
) b
   on Foo = a.Bar
HEREDOC
             )

  ; Basic scalar injection test
  (set! q
        (from a "A"
              (attach b "B"
                      (group-by (scalar b".Foo"))
                      (join-on (scalar b".Foo")" = "(scalar a".Bar")))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select b.Foo as __INJECT0
  from B b
  group by b.Foo
) b
   on b.__INJECT0 = a.Bar
HEREDOC
             )

  ; Basic scalar injection: the scalar cannot contain `a`
  (set! q (from a "A"
                (attach b "B"
                        (group-by (scalar b".Foo"))
                        ; b.Foo can be injected because it doesn't contain `a`
                        (join-on (scalar (scalar a".Bar")" + "(scalar b".Foo"))" = 0"))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select b.Foo as __INJECT0
  from B b
  group by b.Foo
) b
   on a.Bar + b.__INJECT0 = 0
HEREDOC
             )

  ; Basic scalar injection: the scalar cannot contain `a`
  (set! q (from a "A"
                (attach b "B"
                        (group-by (scalar b".Foo"))
                        ; this scalar cannot be injected because it contains `a`
                        (join-on (scalar a".Bar + "b".Foo")" = 0"))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select b.*
  from B b
  group by b.Foo
) b
   on a.Bar + b.Foo = 0
HEREDOC
             )

  ; Basic scalar injection: the scalar cannot contain `a`
  (set! q (from a "A"
                (attach b "B"
                        (group-by (scalar b".Foo"))
                        ; this scalar cannot be injected because it contains `a`
                        (join-on (scalar (scalar a".Bar")" + "b".Foo")" = 0"))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select b.*
  from B b
  group by b.Foo
) b
   on a.Bar + b.Foo = 0
HEREDOC
             )

  ; Basic scalar injection: the scalar cannot contain `a`
  (set! q (from a "A"
                (attach b "B"
                        (group-by (scalar b".Foo"))
                        ; this scalar can be injected because it does not contain `a`
                        (join-on (scalar (scalar "42")" + "b".Foo")" = "(scalar a".Bar")))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select 42 + b.Foo as __INJECT0
  from B b
  group by b.Foo
) b
   on b.__INJECT0 = a.Bar
HEREDOC
             )

  ; Combined scalar injection test. This might be easier to understand than each individual test
  (set! q (from a "A"
                ; This (attach b ....) is the target grouped join:
                (attach b "B"
                        (group-by (scalar b".GroupKey"))
                        ; this scalar is not eligible because `a` is unresolved:
                        (join-on (scalar a".One")" = 1")
                        ; this scalar is eligible because `b` is resolved:
                        (join-on (scalar b".Two")" = 2")
                        ; this scalar is eligible because `b` and `c` are both resolved:
                        (join-on (scalar (join c "C" #:to b
                                               (join-on c".CID = "b".CID"))
                                         ".Three")
                                 " = 3")
                        ; repeat the previous pattern using `attach`
                        (attach d "D"
                                (join-on d".DID = "b".DID"))
                        (join-on (scalar d".Four")" = 4")
                        ; this scalar is not eligible because it does not contain
                        ; a self-reference (it lacks the tuple `b`):
                        (join-on (scalar "Five")" = 5"))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select b.Two as __INJECT0
    , c.Three as __INJECT1
    , d.Four as __INJECT2
  from B b
  inner join D d
     on d.DID = b.DID
  inner join C c
     on c.CID = b.CID
  group by b.GroupKey
) b
   on a.One = 1
  and b.__INJECT0 = 2
  and b.__INJECT1 = 3
  and b.__INJECT2 = 4
  and Five = 5
HEREDOC
             )

  ; Regression: this failed because b.Foo needs to be injected into the join-on
  ; clause but not in the group-by clause.
  (set! q
        (from a "A"
              (attach b "B"
                      (define b.Foo (scalar b".Foo"))
                      (group-by b.Foo)
                      (join-on b.Foo" = "(scalar a".Bar")))))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join (
  select b.Foo as __INJECT0
  from B b
  group by b.Foo
) b
   on b.__INJECT0 = a.Bar
HEREDOC
             )

  ; Scalar injection when the scalar contains an attached join
  (set! expected #<<HEREDOC
select cat.*
from Category cat
inner join (
  select subcat.CategoryId as __INJECT0
  from Product p
  inner join Subcategory subcat
     on subcat.SubcategoryId = p.SubcategoryId
  group by subcat.CategoryId
) p
   on p.__INJECT0 = cat.CategoryId
HEREDOC
        )
  (set! q
        (from cat "Category"
              ; A Category has a group of Products (via Product.Subcategory.CategoryId)
              (attach p "Product"
                      (attach subcat "Subcategory"
                              (join-on (scalar subcat".SubcategoryId")
                                       " = "(scalar p".SubcategoryId")))
                      (group-by (scalar subcat".CategoryId"))
                      (join-on (scalar subcat".CategoryId")
                               " = "(scalar cat".CategoryId")))))
  (check-sql q expected)
  ; This is equivalent to the previous test except that we inline
  ; and duplicate the Product.Subcategory join
  (set! q
        (from cat "Category"
              (define (Subcategory p)
                (join subcat "Subcategory" #:to p
                      (join-on (scalar subcat".SubcategoryId")
                               " = "(scalar p".SubcategoryId"))))
              ; A Category has a group of Products (via Product.Subcategory.CategoryId)
              (attach p "Product"
                      (group-by (scalar (Subcategory p)".CategoryId"))
                      (join-on (scalar (Subcategory p)".CategoryId")
                               " = "(scalar cat".CategoryId")))))
  (check-sql q expected)

  ; This is invalid SQL! We could throw an error or just forge ahead and
  ; generate some invalid SQL. I think either would be fine. This test is
  ; just to let me know if I accidentally change this behavior.
  (set! q
        (from cat "Category"
              ; A Category has a group of Products (via Product.Subcategory.CategoryId)
              (attach p "Product"
                      (attach subcat "Subcategory"
                              (join-on (scalar subcat".SubcategoryId")
                                       " = "(scalar p".SubcategoryId"))
                              ; Mentioning `cat` here prevents injection from happening
                              (join-on (scalar cat".Foo")" = "(scalar cat".Foo")))
                      (group-by (scalar subcat".CategoryId"))
                      (join-on (scalar subcat".CategoryId")
                               " = "(scalar cat".CategoryId")))))
  (check-sql q #<<HEREDOC
select cat.*
from Category cat
inner join (
  select p.*
  from Product p
  inner join Subcategory subcat
     on subcat.SubcategoryId = p.SubcategoryId
    and cat.Foo = cat.Foo
  group by subcat.CategoryId
) p
   on subcat.CategoryId = cat.CategoryId
HEREDOC
             )

  ; Basic aggregate injection
  (set! expected #<<HEREDOC
select b.__INJECT0
from A a
inner join (
  select sum(b.Bar + b.Baz) as __INJECT0
  from B b
  group by b.Foo
) b
   on 1=1
HEREDOC
        )
  (set! q (from a "A"
                (attach b "B"
                        (group-by b".Foo")
                        (join-on "1=1"))
                (select (aggregate "sum("b".Bar + "b".Baz)"))))
  (check-sql q expected)
  ; Same as previous test, but the grouped joins are not `eq?`
  (set! q (from a "A"
                (define (B)
                  (join b "B" #:to a
                        (group-by b".Foo")
                        (join-on "1=1")))
                (select (aggregate "sum("(B)".Bar + "(B)".Baz)"))))
  (check-sql q expected)

  ; Combine scalar and aggregate injection
  (set! expected #<<HEREDOC
select p.__INJECT1
  , p.__INJECT2
from Category cat
inner join (
  select subcat.CategoryId as __INJECT0
    , count(* /* p */) as __INJECT1
    , avg(p.ListPrice) as __INJECT2
  from Product p
  inner join Subcategory subcat
     on subcat.SubcategoryId = p.SubcategoryId
  group by subcat.CategoryId
) p
   on p.__INJECT0 = cat.CategoryId
HEREDOC
        )
  (set! q
        (from cat "Category"
              ; A Category has a group of Products (via Product.Subcategory.CategoryId)
              (attach p "Product"
                      (attach subcat "Subcategory"
                              (join-on (scalar subcat".SubcategoryId")
                                       " = "(scalar p".SubcategoryId")))
                      (group-by (scalar subcat".CategoryId"))
                      (join-on (scalar subcat".CategoryId")
                               " = "(scalar cat".CategoryId")))
              (select (aggregate "count(* /* "p" */)"))
              (select (aggregate "avg("p".ListPrice)"))))
  (check-sql q expected)
  ; This is equivalent to the previous test except that we inline
  ; and duplicate the Product.Subcategory join
  (set! q
        (from cat "Category"
              (define (Subcategory p)
                (join subcat "Subcategory" #:to p
                      (join-on (scalar subcat".SubcategoryId")
                               " = "(scalar p".SubcategoryId"))))
              ; A Category has a group of Products (via Product.Subcategory.CategoryId)
              (attach p "Product"
                      (group-by (scalar (Subcategory p)".CategoryId"))
                      (join-on (scalar (Subcategory p)".CategoryId")
                               " = "(scalar cat".CategoryId")))
              (select (aggregate "count(* /* "p" */)"))
              (select (aggregate "avg("p".ListPrice)"))))
  (check-sql q expected)

  ; aggregate can also be used without grouped joins
  (set! q (from x "X"
                (select (aggregate "count(* /* "x" */)"))))
  (check-sql q #<<HEREDOC
select count(* /* x */)
from X x
HEREDOC
             )

  ; Nested aggregates!
  (set! q (from item "Item"
                (define (Copies/g item)
                  (join copy "Copy" #:to item
                        (group-by (scalar copy".ItemId"))
                        (join-on (scalar copy".ItemId")" = "(scalar item".ItemId"))))
                (define (Rentals/g copy)
                  (join rental "Rental" #:to copy
                        (group-by (scalar rental".CopyId"))
                        (join-on (scalar rental".CopyId")" = "(scalar copy".CopyId"))))
                (select (aggregate "sum("
                                   (aggregate "count(* /* "
                                              (Rentals/g (Copies/g item))
                                              " */)")
                                   ")"))))
  (check-sql q #<<HEREDOC
select copy.__INJECT1
from Item item
inner join (
  select copy.ItemId as __INJECT0
    , sum(rental.__INJECT1) as __INJECT1
  from Copy copy
  inner join (
    select rental.CopyId as __INJECT0
      , count(* /* rental */) as __INJECT1
    from Rental rental
    group by rental.CopyId
  ) rental
     on rental.__INJECT0 = copy.CopyId
  group by copy.ItemId
) copy
   on copy.__INJECT0 = item.ItemId
HEREDOC
             )

  ; Deduplicate injections
  (set! q (from t "Title"
                (attach ratings "Rating"
                        (group-by ratings".TitleID")
                        (join-on (scalar ratings".TitleID")" = "t".TitleID"))
                (select (avg ratings".Score")" as AvgScore")
                (select (avg ratings".Score")" as AvgScore2")))
  (check-sql q #<<HEREDOC
select ratings.__INJECT1 as AvgScore
  , ratings.__INJECT1 as AvgScore2
from Title t
inner join (
  select ratings.TitleID as __INJECT0
    , avg(ratings.Score) as __INJECT1
  from Rating ratings
  group by ratings.TitleID
) ratings
   on ratings.__INJECT0 = t.TitleID
HEREDOC
             )

  ; Aliases are made to be unique if needed.
  ; Both of these want the alias "x" but the subquery gets renamed to "x1":
  (set! q (from x "Parent"
                (define (my-subquery parent)
                  (from x "Sub"
                        (where x".Something = "parent".Something")))
                (where "not "(exists (my-subquery x)))))
  (check-sql q #<<HEREDOC
select x.*
from Parent x
where not exists (
  select x1.*
  from Sub x1
  where x1.Something = x.Something
)
HEREDOC
             )

  ; Another test of unique aliases.
  ; Also a regression test - there was a bug when you rebind the tuple id to something else.
  (set! q (from x "Foo"
                (define orig-x x)
                (attach x "Bar")
                (where orig-x".id = "x".id")))
  (check-sql q #<<HEREDOC
select x.*
from Foo x
inner join Bar x1
   on 1=1
where x.id = x1.id
HEREDOC
             )

  ; Regression - make sure `join` also tolerates rebinding the tuple id
  (set! q (from x "X"
                (attach y "Y"
                        (define y "hello")
                        (join-on x".foo = "y))))
  (check-sql q #<<HEREDOC
select x.*
from X x
inner join Y y
   on x.foo = hello
HEREDOC
             )

  ; `attach` includes the join even if it is never used anywhere else
  (set! q (from x "X"
                (attach y "Y"
                        (join-on "1=1"))))
  (check-sql q #<<HEREDOC
select x.*
from X x
inner join Y y
   on 1=1
HEREDOC
             )

  ; distinct
  (set! q (from x "X"
                (distinct #t)))
  (check-sql q #<<HEREDOC
select distinct x.*
from X x
HEREDOC
             )
  (set! q (from x "X"
                (select "foo")
                (distinct #f)
                (distinct #t)
                (select "bar")))
  (check-sql q #<<HEREDOC
select distinct foo
  , bar
from X x
HEREDOC
             )

  ; simple subquery
  (set! q (from x (subquery "select 1 as ONE")
                (select x".ONE")))
  (check-sql q #<<HEREDOC
select x.ONE
from (select 1 as ONE) x
HEREDOC
             )

  ; subquery prevents appending
  (set! q (from x (subquery (from y "Y"
                                  (select y".ONE")))
                (select x".ONE")))
  (check-sql q #<<HEREDOC
select x.ONE
from (
  select y.ONE
  from Y y
) x
HEREDOC
             )

  ; infer join type
  (define (make-xyz jtype)
    (from x "X"
          (define y
            (join y "Y" #:to x
                  (join-type jtype)
                  (join-on y".foo = "x".foo")))
          (define z
            ; because z is joined to y, will inherit its join type
            (join z "Z" #:to y
                  (join-on z".bar = "y".bar")))
          (select x".*, "y".*, "z".*")))

  (define (expect-xyz jtype)
    (define template #<<HEREDOC
select x.*, y.*, z.*
from X x
JTYPE join Y y
   on y.foo = x.foo
JTYPE join Z z
   on z.bar = y.bar
HEREDOC
      )
    (string-replace template "JTYPE" jtype))

  (check-sql (make-xyz #f)
             (expect-xyz "inner"))
  (check-sql (make-xyz 'inner)
             (expect-xyz "inner"))
  (check-sql (make-xyz 'left)
             (expect-xyz "left"))

  ; Another infer join type test
  (define-syntax-rule (make-test join/where)
    (from p "Product"
          (define s
            (join s "Subcategory" #:to p
                  (join-type 'left)
                  (join-on s".SubcategoryID = "p".SubcategoryID")))
          (attach c "Category"
                  ; If this is a join-on clause, then c needs to infer the
                  ; left join from s. Otherwise, it should remain an inner join.
                  ; This kind of feels like I haven't thought it totally through...
                  ; We could allow `(join-type s ...)` which would mean
                  ; "infer your join type from the given joins/tuples."
                  (join/where c".CategoryID = "s".CategoryID"))
          (select p".*, "s".*, "c".*")))

  (set! q (make-test join-on))
  (check-sql q #<<HEREDOC
select p.*, s.*, c.*
from Product p
left join Subcategory s
   on s.SubcategoryID = p.SubcategoryID
left join Category c
   on c.CategoryID = s.CategoryID
HEREDOC
             )

  (set! q (make-test where))
  (check-sql q #<<HEREDOC
select p.*, s.*, c.*
from Product p
left join Subcategory s
   on s.SubcategoryID = p.SubcategoryID
inner join (
  select c.*
  from Category c
  where c.CategoryID = s.CategoryID
) c
   on 1=1
HEREDOC
             )

  ; If there are no join-on clauses we assume a cross product.
  ; We need "1=1" to make valid SQL
  (set! q (from a "A"
                (attach b "B")))
  (check-sql q #<<HEREDOC
select a.*
from A a
inner join B b
   on 1=1
HEREDOC
             )

  ; to-sql accepts strings
  (check-equal? (to-sql "hello world")
                "hello world")

  (void "end module"))
