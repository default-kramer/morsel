#lang scribble/manual
@(require scribble/eval
          (for-syntax racket/base)
          (for-label morsel-lib
                     morsel-lib/sql
                     morsel-lib/sql/dialect
                     "racket.rkt"))

@title{Morsel}
@author{Ryan Kramer}
@defmodule[morsel-lib]

@(define my-eval #f)
@(define (reset-eval!)
   (set! my-eval (make-base-eval))
   (my-eval '(require morsel-lib
                      morsel-lib/sql
                      morsel-lib/sql/dialect))
   (my-eval '(require "racket.rkt")))
@(reset-eval!)

@(define-syntax-rule (repl x ...)
   (interaction #:eval my-eval x ...))

@(begin
   ; shortcuts to avoid repeating our tech #:keys all over
   (define-syntax (def@tech stx)
     (syntax-case stx ()
       [(_ [id key] ...)
        (with-syntax ([ooo (quote-syntax ...)])
          #'(begin
              (define-syntax (id stx)
                (syntax-case stx ()
                  [(_) #'(tech #:key key key)]
                  [(_ stuff ooo) #'(tech #:key key stuff ooo)]
                  [id #'(id)]))
              ...))]))
   (def@tech
     [:query "query"]
     [:join "join"]
     [:tuple "tuple"]
     [:clause "clause"]
     [:append "append"]))

Morsel implements the core logic of
@link["https://docs.racket-lang.org/plisqin/index.html"]{Plisqin}.
If you are looking for a ready-to-use alternative to SQL, look at Plisqin first.
If Plisqin isn't quite right for you, maybe Morsel could be used to create a similar
library (a few things that are currently private would have to be made public).

Morsel is very generic and makes as few assumptions as possible.
Arguments are rarely validated and almost nothing is considered an error.

@section{Definitions}
A @deftech{query} is the value returned by @(racket (from ....)).
It is recognized by the @(racket query?) predicate.

A @deftech{join} is the value returned by @(racket (join ....)).
It is recognized by the @(racket join?) predicate.

A @deftech{base query} is a @tech{query} or a @tech{join}.
It is recognized by the @(racket base-query?) predicate.

A @deftech{queryable} is a value that represents the data source (such as a table or
a view in an SQL database) that is being queried by @(racket from) or @(racket join).
A queryable is allowed to be @(racket any/c).
In the following example, the queryable is @(racket "Album").

A @deftech{tuple} is a value that is instantiated by @(racket from) and @(racket join).
Nothing else can instantiate a tuple. It is an abstract concept and mostly opaque,
but it is meant to refer to the @tech{queryable}.
In the following example, the tuple is @(racket the-album).

A @(italic "clause") can be imprecisely defined as an expression inside
@(racket from) or @(racket join) that contributes to its content.
(A more precise definition @:clause{will come later}.)
The following example has three clauses.

@(racketblock
  (from the-album "Album"
        (code:comment "These three expressions are clauses:")
        (where the-album".ReleaseYear = 1973")
        (select the-album".ArtistName")
        (select the-album".AlbumName")))

@section{Queries}
@defform[#:literals (define attach)
         (from tuple-id queryable-expr body ...)
         #:grammar [(body (define (proc-id proc-stuff ...) proc-body ...)
                          (define val-id val-expr)
                          (attach join-id join-queryable join-body ...)
                          clause-expr)]]{
 Creates a @tech{query} and binds @(racket tuple-id) as a @tech{tuple} within @(racket body).

 @subsubsub*section{Resolving the Queryable}
 If @(racket queryable-expr) is a @(racket query?), then the new query inherits the
 @tech{queryable} and the @:clause{clauses} of the existing query.
 This is called @deftech[#:key "append"]{appending} to a query:
 @(repl (define 2clause
          (from x "X"
                "one"
                "two"))
        (define 3clause
          (code:comment "3clause appends to 2clause")
          (from x 2clause
                "three"))
        3clause)

 Else if @(racket queryable-expr) is an instance of @(racket gen:queryable), the resolution
 process restarts using the value returned by @(racket unwrap-queryable) in place of
 @(racket queryable-expr).

 Else the value of @(racket queryable-expr) becomes this query's @tech{queryable}.

 @subsubsub*section{Special Forms}
 Using @(racket define) within @(racket body) binds @(racket proc-id) or @(racket val-id)
 in the remainder of @(racket body). It defines a procedure or a value much like Racket's
 built-in @(racket define):
 @(repl
   (from x "X"
         (define (foo a)
           (list "hello" a))
         (define bar 42)
         (foo bar)
         (foo "world")))

 Using @(racket attach) creates an attached join.
 See documentation on @(racket attach) for more information.

 @subsubsub*section{Clauses}
 If a @(racket body) does not match one of the special forms, it is a @deftech{clause}
 and its value is handled according to the following rules.

 If the clause is @(racket void?) it is discarded, as the following example demonstrates:
 @(repl
   (equal? (from x "X"
                 (when #f "nope")
                 (when #t "yep"))
           (from x "X"
                 "yep")))

 If the clause is a @(racket clause<%>), then its @(racket clause-apply) method is invoked.
 This allows a custom clause to make any arbitrary modification to the query's content.
 (For now, these are private and undocumented but could be made public if needed:
 @(racket clause<%> clause? property? make-property content? content-set content-cons content-ref).)

 Otherwise the clause is simply added to the query's list of clauses.
}

@section{Joins}
@defform[#:literals (define attach)
         (join tuple-id queryable-expr maybe-to body ...)
         #:grammar [(maybe-to (code:line)
                              (code:line #:to link-expr))
                    (body (define (proc-id proc-stuff ...) proc-body ...)
                          (define val-id val-expr)
                          (attach join-id join-queryable join-body ...)
                          clause-expr)]]{
 Like @(racket from) except that
 @(itemlist
   @item{It creates a @tech{join} instead of a @tech{query}.}
   @item{@:append{Appending} occurs when @(racket queryable-expr) is a
  @(racket join?) instead of a @(racket query?).})

 If @(racket link-expr) is given, its value should satisfy @(racket (or/c tuple? join?)).
 This enables the SQL renderer to find the "join target", which is the @tech{base query}
 in whose scope this join belongs.
 For now, @(racket join-target) is private and undocumented, but could be made public.
 It follows the links through tuples and "simple joins" (also undocumented)
 to reach the last link in the chain, which should be a query or non-simple join.
}


@defform[(attach tuple-id queryable-expr body ...)]{
 Creates a join, links it @(racket #:to) the enclosing @tech{base query}, and binds
 @(racket tuple-id) to that join in the body of the enclosing @tech{base query}.
 @(racketblock
   (from x "X"
         (attach y "Y"
                 (join-on y".Foo = "x".Foo"))
         (select y".Bar"))
   (code:comment "is *almost* equal to")
   (from x "X"
         (define y
           (join y "Y"
                 #:to x
                 (join-on y".Foo = "x".Foo")))
         (select y".Bar")))

 Note that by default, any use of @(racket attach) is an error.
 It is the @(racket from) and @(racket join) macros that have special handling for @(racket attach).

 In the previous example, I said that @(racket (attach y ....)) is *almost* equal to
 @(racket (define y (join y ....))).
 The difference is that @(racket attach) immediately adds the join to the enclosing query,
 whereas @(racket define) does not.
 The following example shows the difference:
 @(repl
   (define (example-1)
     (from x "X"
           (attach y "Y"
                   (join-on y".Foo = "x".Foo"))))
   (define (example-2)
     (from x "X"
           (code:comment "y is defined but never used")
           (define y
             (join y "Y"
                   #:to x
                   (join-on y".Foo = "x".Foo")))))
   (example-1)
   (example-2))
}

@section{Reference}
@defproc[(query? [v any/c]) boolean?]{
 A predicate that recognizes @:query{queries}.
 @(repl (query? (from x "X"))
        (query? (join x "X")))
}
@defproc[(join? [v any/c]) boolean?]{
 A predicate that recognizes @:join{joins}.
 @(repl (join? (join x "X"))
        (join? (from x "X")))
}
@defproc[(base-query? [v any/c]) boolean?]{
 A predicate equivalent to @(racket (or/c query? join?)).
 @(repl (base-query? (from x "X"))
        (base-query? (join x "X")))
}
@defproc[(tuple? [v any/c]) boolean?]{
 A predicate that recognizes @:tuple{tuples}.
 @(repl (from x "X" (tuple? x)))
}
@defproc[(token? [v any/c]) boolean?]{
 A predicate equivalent to @(racket (is-a?/c token<%>)).
}

@definterface[token<%> ()]{
 @defmethod[(token-kind) symbol?]{
  The SQL renderer uses this to decide what kind of clause each is:
  @(repl
    (send (select) token-kind)
    (send (group-by) token-kind))
  Certain token kinds are also used by some core logic, but for now this logic
  is private and only consumed by the SQL renderer.
 }
 @defmethod[(token-content) any/c]{
  Allows Morsel to explore deeper into an object graph.
  When Morsel encounters a @(racket token<%>) it calls this method and continues
  exploring whatever value is returned.
  Morsel knows how to explore @:query{queries}, @:join{joins}, pairs, and tokens.
 }
 @defmethod[(set-token-aspect! [key any/c] [value any/c]) any/c]{
  You can use @(racket define-token-aspect-stuff) to implement this.

  Used to cache the results of potentially expensive computations.
 }
 @defmethod[(get-token-aspect [key any/c] [not-found-value any/c]) any/c]{
  Use @(racket define-token-aspect-stuff) to implement this.
 }
}
@defform[(define-token-aspect-stuff)]{
 Should be used inside a @(racket class*) definition that implements @(racket token<%>).
 Creates definitions for the @(racketplainfont "get-token-aspect") and
 @(racketplainfont "set-token-aspect!") methods.
}

@defproc[(queryable? [x any/c]) any/c]{
 A predicate that recognizes instances of @(racket gen:queryable).
}

@defthing[gen:queryable any/c]{
 A generic interface that defines an @(racket unwrap-queryable) method.
 @(repl
   (struct my-queryable (item) #:transparent
     #:methods gen:queryable
     [(define (unwrap-queryable me)
        (displayln (format "unwrapping ~v" me))
        (my-queryable-item me))])
   (get-queryable (my-queryable (my-queryable "hello")))
   (from x (my-queryable 'world)))
}

@defproc[(get-queryable [x any/c]) any/c]{
 Fully unwraps a queryable.
 @(itemlist
   @item{If @(racket x) is an instance of @(racket gen:queryable), then
  @(racket (get-queryable (unwrap-queryable x))) is returned.}
   @item{Else if @(racket (or (base-query? x) (tuple? x))), then its @tech{queryable} is returned.}
   @item{Else @(racket x) is returned.})
 @(repl
   (get-queryable (from a (from b "Here is the queryable")))
   (get-queryable 'nothing-special)
   (from a "ABC" 1 2 3 (get-queryable a)))
}

@section{Caveats}
@subsection{Laziness}
The body of @(racket from) and @(racket join) is evaluated lazily, as the following
example demonstrates:
@(repl
  (define counter 0)
  (define the-query (from x "X"
                          (set! counter (add1 counter))
                          "hello"))
  (code:comment "counter is still zero")
  counter
  (code:comment "printing the-query forces evaluation")
  the-query
  (code:comment "now the counter has been incremented")
  counter)

However, this laziness is not guaranteed. Morsel is currently only well-defined
for @link["https://en.wikipedia.org/wiki/Pure_function"]{"purely functional"} code.

@subsection{Premature Tuple Access}
Every @tech{tuple} contains a reference back to the @tech{base query} that created it.
This is an implementation detail.
However, this creates a cycle in the data structure.
Prematurely attempting to access the base query via the tuple could cause an
infinite loop. If this situation is detected, an exception is raised instead:
@(repl
  (from x "X"
        (attach y "Y"
                (equal? x y))))

@section{morsel-lib/sql}
@(defmodule morsel-lib/sql)

@defproc[(to-sql [x any/c]) string?]{
 Generates SQL
 @(repl
   (define (example)
     (from p "Product"
           (attach c "Category"
                   (join-on c".CategoryId = "p".ProductId"))
           (select p".ProductName")
           (select c".CategoryName")))
   (displayln (to-sql (example))))
}

@defproc[(sql-token? [x any/c]) any/c]{
 A predicate that recognizes instances of @(racket sql-token<%>).
}

@definterface[sql-token<%> (token<%>)]{
 @defmethod[(sql-token-reduce) any/c]{
  When @(racket to-sql) encounters an object that implements @(racket sql-token<%>),
  it calls this method and continues to generate SQL using the return value in place
  of the object.
}}

@(define-syntax-rule (def-frags ooo [proc-id ...] content ...)
   (deftogether
     [(defproc (proc-id [x any/c] ooo) token?)
      ... ]
     content ...))
@def-frags[... (select where join-on group-by having order-by)]{
 To be used as clauses.
}
@def-frags[... (scalar aggregate bool subquery sql silence)]{
 To be used as fragments.
}
@defproc[(limit [v exact-positive-integer?]) any/c]{
 To be used as a @(:clause). Limits the result set to at most @(racket v) rows.
 @(repl
   (define q (from x "X"
                   (limit 42)))
   (parameterize ([current-dialect (sqlite)])
     (displayln (to-sql q))))
}
@defproc[(offset [v exact-positive-integer?]) any/c]{
 To be used as a @(:clause). Skips past the first @(racket v) rows of the result set.
 @(repl
   (define q (from x "X"
                   (offset 100)))
   (parameterize ([current-dialect (sqlite)])
     (displayln (to-sql q))))
}
@defproc[(distinct [v any/c]) any/c]{
 To be used as a @(:clause). Holds a boolean flag which controls whether the select
 list is rendered with "distinct" (default is #f).
 @(repl
   (displayln (to-sql (from x "X"
                            (distinct #t))))
   (displayln (to-sql (from x "X"
                            (distinct #f)))))
}
@defproc[(join-type [v (or/c #f 'inner 'left)]) any/c]{
 To be used as a @(:clause) within @(racket join) or @(racket attach).
 @(repl
   (define q
     (from x "X"
           (attach y "Y"
                   (join-type 'left)
                   (join-on y".YID = "x".YID"))))
   (displayln (to-sql q)))

 The default join type is @(racket #f).
 This means that the join will be an @(racket 'inner) join unless its @(racket join-on)
 clauses contain a @(racket 'left) join, in which case it becomes a @(racket 'left) join.
 @(repl
   (define (q jtype)
     (from x "X"
           (attach y "Y"
                   (join-type jtype)
                   (join-on y".YID = "x".YID"))
           (attach z "Z"
                   (code:comment "y appears in z's join-on clauses.")
                   (code:comment "Because z's join-type is #f, it will become")
                   (code:comment "a left join if y is a left join.")
                   (join-on z".ZID = "y".ZID"))))
   (displayln (to-sql (q 'inner)))
   (displayln (to-sql (q 'left))))
}

@section{morsel-lib/sql/dialect}
@(defmodule morsel-lib/sql/dialect)
@defparam[current-dialect dialect dialect? #:value #f]{
 A parameter representing the SQL dialect that @(racket to-sql) should generate.
 @(repl
   (define q
     (from x "X"
           (limit 10)))
   (parameterize ([current-dialect (postgres)])
     (displayln (to-sql q)))
   (parameterize ([current-dialect (mssql)])
     (displayln (to-sql q))))
}

@defproc[(dialect? [x any/c]) any/c]{
 A predicate that recognizes dialects.
}

@deftogether[[
 @defproc[(mssql) dialect?]
 @defproc[(postgres) dialect?]
 @defproc[(sqlite) dialect?]]]{
 Creates a dialect representing Microsoft SQL Server, PostgreSQL, or SQLite.
}

@deftogether[[
 @defproc[(mssql? [x any/c]) any/c]
 @defproc[(postgres? [x any/c]) any/c]
 @defproc[(sqlite? [x any/c]) any/c]]]{
 Tests whether @(racket x) is a @(racket dialect?) of a certain kind.
 @(repl
   (mssql? (mssql))
   (mssql? (postgres))
   (postgres? (postgres)))
}
