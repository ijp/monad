(library (monad writer)
(export >>=
        return
        tell
        listen
        pass
        run-writer
        map-writer
        listens
        censor
        (rename (pair->writer writer)
                (writer-output exec-writer))
        make-writer ; seems more useful than writer to me
        )
(import (rnrs))
;; see http://hackage.haskell.org/packages/archive/mtl/2.0.1.0/doc/html/Control-Monad-Writer-Lazy.html

;; Currently limited to strings, maybe generalize to monoids
(define mempty "")
(define mappend string-append)

(define-record-type writer
  (fields result output))

(define (run-writer writer)
  (cons (writer-result writer)
        (writer-output writer)))

(define (pair->writer pair)
  (make-writer (car pair) (cdr pair)))

(define (return x)
  (make-writer x mempty))

(define (>>= m f)
  (let ((result (f (writer-result m))))
    (make-writer (writer-result result)
                 (mappend (writer-output m)
                          (writer-output result)))))

(define (tell w)
  (make-writer '() w))

(define (listen ma)
  (make-writer (run-writer ma)
               (writer-output ma)))

(define (pass maww)
  (let ([result (writer-result maww)]
        [output (writer-output maww)])
    (let ([return-val (car result)]
          [modifier (cdr result)])
      (make-writer return-val (modifier output)))))

(define (map-writer f w)
  (pair->writer (f (run-writer w))))

(define (listens f ma)
  (let ((result (run-writer ma)))
    (make-writer (cons (car result)
                       (f (cdr result)))
                 (writer-output ma))))

(define (censor f ma)
  (let ([result (writer-result ma)]
        [output (writer-output ma)])
    (make-writer result (f output))))

)
