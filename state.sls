(library (monad state)
(export >>=
        return
        get
        put
        modify
        gets
        run-state
        )
(import (rnrs))
;; see
;; http://cvs.haskell.org/Hugs/pages/libraries/mtl/Control-Monad-State.html

;; State Monad m s = s -> (a . s)

(define (return x)
  (lambda (s)
    (cons x s)))

(define (>>= x f)
  (lambda (s)
    (let* ((result (x s))
           (val (car result))
           (s* (cdr result)))
      ((f val) s*))))

(define get
  (lambda (s)
    (cons s s)))

(define (put new)
  (lambda (s) ; not sure whether I should return old state or #f
    (cons s new)))

(define (modify f)
  (lambda (s) ; not sure whether I should return old state or #f
    (cons s (f s))))

(define (gets f)
  (lambda (s)
    (cons (f s) s)))

(define (run-state m state)
  (m state))

)

;; (run-state get 10) ; (10 . 10)
;; (run-state (>>= (gets values) return) 10) ; (10 . 10)
;; (run-state (>>= (gets (lambda (x) (* 2 x))) return) 10) ;(20 . 10)
;; (run-state (>>= (modify (lambda (x) (* 2 x))) return) 10) ; (10 . 20)
;; (run-state (>>= (put 88) return) 10) ;(10 . 88)
