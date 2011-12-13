#!r6rs
;; Interestingly, return and >>= are basically K and S combinators
(library (monad reader)
(export >>=
        return
        ask
        run-reader
        local
        asks
        map-reader
        with-reader
        )
(import (rnrs))

;; see http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-Reader-Class.html
;; and http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-Reader.html

(define (return x)
  (lambda (env)
    x))

(define (>>= m f)
  (lambda (env)
    ((f (m env)) env)))

(define ask
  (lambda (env)
    env))

(define (run-reader reader env)
  (reader env))

(define (local f m)
  (lambda (env)
    (m (f env))))

(define (asks f)
  (lambda (env)
    (f env)))

(define (map-reader f m)
  (lambda (env)
    (f (m env))))

(define (with-reader f m)
  (lambda (env)
    (m (f env))))

)
;; scheme@(guile−user)> (run-reader (return 33) 10)
;; $3 = 33
;; scheme@(guile−user)> (run-reader ask 10)
;; $4 = 10
;; scheme@(guile−user)> (run-reader (>>= ask (lambda (x) (return (cons x x)))) 10)
;; $5 = (10 . 10)
;; scheme@(guile−user)> (run-reader (>>= ask (lambda (x) (>>= (return 20) (lambda (y) (>>= ask (lambda (z) (return (list x y z)))))))) 10)
;; $6 = (10 20 10)
;; scheme@(guile−user)> (run-reader (>>= (local (lambda (x) (* 2 x)) ask)
;;                                       (lambda (x)
;;                                         (>>= ask
;;                                              (lambda (y)
;;                                                (return (cons x y))))))
;;                                  10)
;; $7 = (20 . 10)
;; scheme@(guile−user)> (run-reader (>>= (asks (lambda (x) (* 2 x)))
;;                                       (lambda (x)
;;                                         (>>= ask
;;                                              (lambda (y)
;;                                                (return (cons x y))))))
;;                                  10)
;; $8 = (20 . 10)
;; scheme@(guile−user)> 
;; scheme@(guile−user)> (run-reader (map-reader number->string (return 3))
;;                                  '(1 2 3))
;; $10 = "3"
;; scheme@(guile−user)> (run-reader (map-reader number->string ask) 3)
;; $11 = "3"
;; scheme@(guile−user)> (run-reader (with-reader number->string (return "foo")) 3)
;; ;;; <stdin>:327:12: warning: possibly unbound variable `with−reader'
;; <unnamed port>:327:12: In procedure #<procedure 9f4a8c0 at <current input>:327:0 ()>:
;; <unnamed port>:327:12: In procedure module−lookup: Unbound variable: with−reader

;; Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.
;; scheme@(guile−user) [1]> ,q
;; scheme@(guile−user)> (run-reader (with-reader number->string (return "foo")) 3)
;; $12 = "foo"
;; scheme@(guile−user)> (run-reader (with-reader number->string ask) 3)
;; $13 = "3"
;; scheme@(guile−user)>
