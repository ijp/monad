#!r6rs
;;; continuation.sls --- Continuation Monad

;; Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

(library (monad continuation)
(export >>=
        return
        callcc
        run-cont
        cont
        map-cont
        with-cont
        )

(import (rnrs))

(define (return x)
  (lambda (k) (k x)))

(define (>>= m f)
  (lambda (k)
    (m (lambda (v)
         ((f v) k)))))

(define (callcc f)
  (lambda (k)
    ((f (lambda (v)
          (lambda (k2)
            (k v))))
     k)))

(define (cont f)
  (lambda (k)
    (f k)))


(define (run-cont m k)
  (m k))

(define (map-cont f m)
  (>>= m (lambda (x) (return (f x)))))

(define (with-cont f m)
  (lambda (k)
    (run-cont m (f k))))

)
