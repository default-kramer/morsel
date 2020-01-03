#lang racket
(module+ test
  (require doc-coverage
           morsel-lib
           morsel-lib/sql)
  (check-all-documented 'morsel-lib)
  (check-all-documented 'morsel-lib/sql)

  ; TODO why does this crash?
  ;(check-all-documented 'morsel-lib/sql/dialect)
  ; The root cause is that this crashes:
  ;(module->exports 'morsel-lib/sql/dialect)
  ; Leave a failing test here to remind me
  (check-documented 'morsel-lib/sql/dialect 'TODO)
  )