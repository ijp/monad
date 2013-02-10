#!r6rs
;;; maybe.sls --- Maybe Monad

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

(library (monad maybe)
(export return
        >>=
        fail
        maybe

        just
        nothing
        ;; for type-case
        Maybe

        from-just
        from-maybe
        just? ; called isJust in haskell
        nothing? ; isNothing
        (rename (Maybe? maybe?)) ; for good measure

        fmap
        map-maybe ; a filter

        ;; haskell also has listToMaybe, maybeToList and catMaybes
        list->maybe
        maybe->list
        cat-maybes
        )
(import (rnrs)
        (ijputils datatypes))

(define-type Maybe
  (just (value (lambda (_) #t)))
  (nothing))

(define from-just just-value)

(define (from-maybe a maybea)
  (type-case Maybe maybea
    (just (x) x)
    (nothing () a)))

(define (return a)
  (just a))

(define (>>= x f)
  (type-case Maybe x
    (just (a) (f a))
    (nothing () x)))

(define (fail)
  (nothing))

;; maybe :: b -> (a -> b) -> Maybe a -> b
(define (maybe b f)
  ;; seems more useful curried, but may change it back
  (lambda (m)
    (type-case Maybe m
      (just (a) (f a))
      (nothing () b))))


(define (fmap f m)
  (type-case Maybe m
    (just (a) (just (f a)))
    (nothing () m)))


;; map-maybe :: (a -> Maybe b) -> [a] -> [b]
(define (map-maybe f l)
  (fold-right (lambda (elem l)
                (type-case Maybe (f elem)
                  [just (x) (cons x l)]
                  [nothing () l]))
              '()
              l))

(define (cat-maybes l)
  (map-maybe (lambda (x) x) l))

(define (list->maybe l)
  (if (null? l)
      (fail)
      (return (car l))))

(define (maybe->list m)
  (type-case Maybe m
    [just (a) (list a)]
    [nothing () (list)]))

)
