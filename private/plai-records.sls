#!r6rs
;; 1.1 Defining Types: define-type
;;
;; (define-type type-id variant ...)
;;
;; variant	 	=	 	(variant-id (field-id contract-expr) ...)
;; Defines the datatype type-id. A constructor variant-id is defined
;; for each variant. Each constructor takes an argument for each field
;; of its variant.
;;
;; The value of each field is checked by its associated
;; contract-expr. A contract-expr may be an arbitrary predicate or a
;; contract.
;;
;; In addition to the contructors, a define-type expression also defines:
;;
;;     a predicate type-id? that returns true for instances of the
;;     datatype, and false for any other value,
;;
;;     for each variant, a predicate variant-id? that returns true
;;     when applied to a value of the same variant and false for any
;;     other value,
;;
;;     for each field of each variant, an accessor variant-id-field-id
;;     that returns the value of the field, and
;;
;;     for each field of each variant, a mutator
;;     set-variant-id-field-id! that set the value of the field.

;; 1.2 Deconstructing Data Structures: type-case
;;
;; (type-case datatype-id expr
;;    branch ...)
 ;;
;; branch	 	=	 	(variant-id (field-id ...) result-expr ...)
;;  	 	|	 	(else result-expr ...)
;; Branches on the datatype instance produced by expr, which must be
;; an instance of datatype-id (previously defined with define-type)
;; Each branch extracts the values of the fields, and binds them to
;; field-id ....
;;
;; If a branch is not specified for each variant, you may use an else
;; branch to create a catch-all branch. An else branch must be the
;; last branch in the sequence of branches. type-case signals a
;; compile-time error if all variants are not covered and the else
;; branch is missing. Similarly, type-case signals a compile-time
;; error if an else branch is unreachable because a branch exists for
;; all variants.
(library (monad private plai-records)
(export define-type
        type-case
        )
(import (rnrs)
        ;(srfi :9 records)
        (for (familiars syntax) expand))

(define-syntax define-type
  (lambda (stx)
    (define (make-predicate variant)
      (make-id variant variant '?))
    (define (make-constructor variant)
      variant)
    (define (make-accessors variant-id variant-slots) ;variant 'listof (field-id contract-expr)...
      (map (lambda (field)
             (make-id variant-id variant-id '- (car field)))
           variant-slots))
    (define (make-mutators variant-id variant-slots)
      (map (lambda (field temp)
             (let ((id (make-id variant-id 'set- variant-id '- (car field) '!)))
               (list temp
                     #`(define (#,id obj val)
                         (assert (#,(cadr field) val))
                         (#,temp obj val)))))
           variant-slots
           (generate-temporaries variant-slots)))
    (define (make-variant-name variant)
      (make-id variant '< variant '>))
    (syntax-case stx ()
      [(define-type type-id (variant-id (field-id contract-expr) ...) ...)
       (let ((variants (syntax->list #'(variant-id ...)))
             (slots (map (lambda (variant-slots)
                           (map (lambda (slot)
                                  (syntax->list slot))
                                (syntax->list variant-slots)))
                         (syntax->list #'(((field-id contract-expr) ...) ...)))))
         (with-syntax (((predicate ...)
                        (map make-predicate variants))
                       ((constructor ...)
                        (map make-constructor variants))
                       (((accessor ...) ...)
                        (map make-accessors variants slots))
                       ((((mutator real-mutator) ...) ...)
                        (map make-mutators variants slots))
                       ((variant-name ...)
                        (map make-variant-name variants))
                       ;;  (make-id #'define-type #'type-id '?)
                       (type-id? (make-predicate #'type-id)))
           #`(begin
               (define (type-id? obj)
                 (or (predicate obj) ...))
               (define-record-type (variant-name constructor predicate)
                 (protocol
                  (lambda (new)
                    (lambda (field-id ...)
                      (assert (and (contract-expr field-id) ...))
                      (new field-id ...))))
                 (fields
                  (mutable field-id accessor mutator) ...))
               ...
               #,@(apply append (map syntax->list (syntax->list #'((real-mutator ...) ...))))
               
               (define-syntax type-id
                 (lambda (stx)
                   (define (expand-type-case expr clauses variant-env)
                     #`(let ((value #,expr))
                         ;; while raising an assertion-violation is
                         ;; correct, this could use a more specific message
                         (assert (type-id? value))
                         (cond #,@(map (lambda (clause)
                                         (expand-clause clause #'value variant-env))
                                       clauses))))
                   (define (expand-clause clause value bindings)
                     (syntax-case clause (else)
                       [(else body (... ...))
                        #'(else body (... ...))]
                       [(variant (field (... ...)) body (... ...))
                        (let ((variant-procs (assp (lambda (x)
                                                     (free-identifier=? x #'variant))
                                                   bindings)))
                          (with-syntax ((predicate? (cadr variant-procs))
                                        ((accessor* (... ...)) (cddr variant-procs)))
                            #`((predicate? #,value)
                               (let ((field (accessor* #,value)) (... ...))
                                 body (... ...)))))]))
                   (define (assert-valid-clauses clauses variants)
                     #t)
                   (syntax-case stx (type-case)
                     ((type-id type-case expr clauses (... ...))
                      (let ((variants (map syntax->list
                                           (syntax->list #'((constructor predicate accessor ...) ...))))
                            (clauses #'(clauses (... ...))))
                        (assert-valid-clauses clauses variants)
                        (expand-type-case #'expr clauses variants)))))))))])))

(define-syntax type-case
  (syntax-rules ()
    ((type-case type-id expr clauses ...)
     (type-id type-case expr clauses ...))))

)
