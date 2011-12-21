#!r6rs
(library (monad continuation)
(export >>=
        return
        callcc
        run-cont
        cont
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
)
