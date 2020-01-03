#lang racket
(module+ test
  (require doc-coverage
           morsel-lib
           morsel-lib/sql
           morsel-lib/sql/dialect)
  (check-all-documented 'morsel-lib)
  (check-all-documented 'morsel-lib/sql)
  (check-all-documented 'morsel-lib/sql/dialect))
