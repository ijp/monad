#!r6rs
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
