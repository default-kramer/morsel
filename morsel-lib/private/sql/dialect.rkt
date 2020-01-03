#lang racket

(provide current-dialect dialect?
         postgres postgres? mssql mssql? sqlite sqlite?)

(struct dialect (name) #:transparent)

(define (dialect-guard x)
  (if (or (not x)
          (dialect? x))
      x
      (raise-argument-error 'current-dialect "(or/c #f dialect?)" x)))

(define current-dialect (make-parameter #f dialect-guard))

(define-syntax-rule (def-dialect id id?)
  (begin
    (define (id) (dialect 'id))
    (define (id? x)
      (and (dialect? x)
           (equal? 'id (dialect-name x))))))

(def-dialect postgres postgres?)
(def-dialect mssql mssql?)
(def-dialect sqlite sqlite?)
