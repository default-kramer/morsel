#lang racket/base

; This file implements (require morsel-lib/sql/dialect)

(require "../private/sql/dialect.rkt")

(provide current-dialect dialect?
         postgres postgres? mssql mssql? sqlite sqlite?)
