#lang info

(define collection "morsel-doc")
(define scribblings '(("morsel.scrbl")))

(define deps '("base"))
(define build-deps '("morsel-lib" "scribble-lib" "racket-doc" "doc-coverage"))

(define pkg-desc "Documentation for morsel")
