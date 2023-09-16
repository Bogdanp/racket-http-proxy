#lang info

(define license 'BSD-3-Clause)
(define collection "net")
(define deps '("base"
               "http-proxy-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define implies '("http-proxy-lib"))
(define scribblings '(("http-proxy-manual.scrbl")))
