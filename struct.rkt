#lang racket/base
(module primitive racket/base
  (provide (all-defined-out))
  (require ffi/unsafe/vm syntax/parse/define
           (for-syntax racket/base racket/syntax
                       racket/sequence racket/list))

  (begin-for-syntax
    (struct hack-struct-info (size)
      #:property prop:procedure
      (λ (self stx)
        (syntax-case stx ()
          [(_ rec args ...)
           #'(make-record rec args ...)]))))

  (define closure-ref (vm-eval '($primitive $closure-ref)))
  (define closure-length (vm-eval '($primitive $closure-length)))

  (define hash-table? (vm-primitive 'hash-table?))
  (define hashtable-ephemeron? (vm-primitive 'hashtable-ephemeron?))
  (define hashtable-ref (vm-primitive 'hashtable-ref))

  (define property-list (vm-primitive 'property-list))
  (define record-type-uid (vm-primitive 'record-type-uid))

  (define (get-properties rec)
    (property-list (record-type-uid (record-type-descriptor rec))))

  (define hash-table-map (vm-primitive 'hash-table-map))

  (define make-record
    (let ([$record (vm-eval '($primitive $record))]
          [apply (vm-eval '($primitive apply))])
      (λ (template . args)
        (apply $record (record-type-descriptor template) args))))
  
  (define record-ref (vm-eval '($primitive $record-ref)))

  (define record-type-name (vm-primitive 'record-type-name))
  (define record? (vm-primitive 'record?))

  (define record-type-descriptor
    (vm-eval '($primitive $record-type-descriptor)))

  (define record-type-parent
    (vm-primitive 'record-type-parent))

  (define (check-record? rtd name)
    (let loop ([rtd rtd])
      (cond
        [(not rtd) #f]
        [(eq? (record-type-name rtd) name) #t]
        [else
         (loop (record-type-parent rtd))])))

  (define-syntax-parser hack-struct
    [(_ name:id (~optional parent:id #:defaults ([parent #'#f]))
        (field:id ...))
   
     #:with name? (format-id #'name "~a?" #'name #:subs? #t)
   
     #:with (name-field ...)
     (for/list ([field (in-syntax #'(field ...))])
       (format-id #'name "~a-~a" #'name field #:subs? #t))
     #:do
     [(define start (if (syntax-e #'parent)
                        (hack-struct-info-size
                         (syntax-local-value #'parent))
                        0))
      (define len (length (syntax->list #'(field ...))))]
   
     #:with size (datum->syntax #'k (datum->syntax #'k (+ start len)))
     #:with (pos ...)
     (datum->syntax #'k (map (λ (o) (+ start o)) (range len)))
   
     #'(begin
         (define-values (name? name-field ...)
           (let ()
             (define (name? rec)
               (and (record? rec)
                    (check-record? (record-type-descriptor rec) 'name)))
             (define (name-field rec)
               (record-ref rec pos))
             ...
             (values name? name-field ...)))
         (define-syntax name (hack-struct-info size)))])
  
  (hack-struct struct-type-prop (name guard supers))
  
  (define (find-property rec name)
    (for/first ([prop (in-list (get-properties rec))]
                #:when (struct-type-prop? prop)
                #:when (eq? (struct-type-prop-name prop) name))
      prop))

  (define find-property-pred-acc
    (let ([tables #f])
      (λ (prop)
        (unless tables
          (set! tables
                (for*/list ([i (in-range (closure-length make-struct-type-property))]
                            [t (in-value (closure-ref make-struct-type-property i))]
                            #:when (and (hash-table? t) (hashtable-ephemeron? t)))
                  t)))
        (define pred
          (let/ec cc
            (for ([t (in-list tables)])
              (define ls (hash-table-map t cons))
              (for ([p (in-list ls)]
                    #:break (pair? (cdr p))
                    #:when (eq? (cdr p) prop))
                (cc (car p))))))
        (define acc
          (let/ec cc
            (for ([t (in-list tables)])
              (define ls (hash-table-map t cons))
              (for ([p (in-list ls)]
                    #:break (not (pair? (cdr p)))
                    #:when (eq? (cadr p) pred))
                (cc (car p))))))
        (values pred acc))))
  )
  

(require 'primitive)
(provide (all-defined-out))

(hack-struct -syntax (content*
                      scopes
                      shifted-multi-scopes
                      mpi-shifts
                      srcloc
                      props
                      inspector))

(hack-struct scope (id
                    kind
                    binding-table))

(hack-struct interned-scope scope (key))

(hack-struct representative-scope scope (owner
                                         phase))

(hack-struct multi-scope (id
                          name
                          scopes
                          shifted
                          label-shifted))

(hack-struct shifted-multi-scope (phase
                                  multi-scope))

(hack-struct shifted-to-label-phase (from))

(hack-struct fallback (search-list))

(hack-struct bulk-binding-at (scopes
                              bulk))


(hack-struct table-with-bulk-bindings (syms
                                       syms/serialize
                                       bulk-bindings))


(hack-struct bulk-binding-class (get-symbols
                                 create))

(define bulk-binding-ref
  (let ([ref #f])
    (λ (b)
      (unless ref
        (set! ref (call-with-values
                   (λ () (find-property-pred-acc (find-property b 'bulk-binding)))
                   (λ (a b) b))))
      (ref b))))

(module+ test
  (require rackunit)
  (test-case "scope"
    (define intro (make-interned-syntax-introducer 'a))
    (define x (car (hash-keys
                    (-syntax-scopes (intro (datum->syntax #f 'x))))))
    (check-true (interned-scope? x))
    (check-eq? (interned-scope-key x) 'a)
    (check-true
     (interned-scope? (interned-scope x 0 'macro #hasheq() 'a))))

  (test-case "prop"
    (define-values (prop pred ref) (make-struct-type-property 'myprop))
    (struct x () #:property prop 'foo)
    (define p (find-property (x) 'myprop))
    (define-values (p? r) (find-property-pred-acc p))
    (check-equal? (list prop pred ref)
                  (list p p? r))))