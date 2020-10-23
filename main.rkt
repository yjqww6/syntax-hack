#lang racket/base
(require "struct.rkt")
(provide syntax-scope-set syntax-mapped-names)

(define (syntax-scope-set s phase)
  (scope-set-at-fallback s (fallback-first (-syntax-shifted-multi-scopes s)) phase))

(define (fallback-first smss)
  (if (fallback? smss)
      (car (fallback-search-list smss))
      smss))

(define (label-phase? a)
  (not a))

(define (phase- a b)
  (and a b (- a b)))

(define (deserialized-scope-id? scope-id)
  (negative? scope-id))

(define (multi-scope-to-scope-at-phase ms phase)
  (let ([scopes (unbox (multi-scope-scopes ms))])
    (or (hash-ref scopes phase #f)
        (error 'multi-scope-to-scope-at-phase "didn't materialize"))))

(define (scope-set-at-fallback s smss phase)
  (for*/fold ([scopes (-syntax-scopes s)]) ([sms (in-hash-keys smss)]
                                            #:when (or (label-phase? phase)
                                                       (not (shifted-to-label-phase? (shifted-multi-scope-phase sms)))))
    (hash-set scopes
              (multi-scope-to-scope-at-phase (shifted-multi-scope-multi-scope sms)
                                             (let ([ph (shifted-multi-scope-phase sms)])
                                               (if (shifted-to-label-phase? ph)
                                                   (shifted-to-label-phase-from ph)
                                                   (phase- ph phase))))
              #t)))


(define (hash-union s1 s2)
  (if ((hash-count s1) . < . (hash-count s2))
      (hash-union s2 s1)
      (for/fold ([s1 s1]) ([k (in-hash-keys s2)])
        (hash-set s1 k #t))))

(define (bulk-binding-symbols b s extra-shifts)
  ;; Providing the identifier `s` supports its shifts
  ((bulk-binding-class-get-symbols (bulk-binding-ref b))
   b 
   (append extra-shifts (if s (-syntax-mpi-shifts s) null))))

(define (binding-table-symbols table scs s extra-shifts subset?)
  (define-values (ht bulk-bindings)
    (if (hash? table)
        (values table null)
        (values (table-with-bulk-bindings-syms table)
                (table-with-bulk-bindings-bulk-bindings table))))
  (hash-union
   (for/hasheq ([(sym at-sym) (in-hash ht)]
                #:when (or (not subset?)
                           (for/or ([an-scs (in-hash-keys at-sym)])
                             (hash-keys-subset? an-scs scs))))
     (values sym #t))
   
   (for*/hasheq ([bba (in-list bulk-bindings)]
                 #:when (or (not subset?)
                            (hash-keys-subset? (bulk-binding-at-scopes bba) scs))
                 [sym (in-hash-keys
                       (bulk-binding-symbols (bulk-binding-at-bulk bba) s extra-shifts))])
     (values sym #t))))


(define (syntax-mapped-names s [phase 0]
                             #:multi? [multi? #t]
                             #:subset? [subset? #t])
  (define s-scs (if multi?
                    (syntax-scope-set s phase)
                    (-syntax-scopes s)))
  (for/fold ([syms (hasheq)])
            ([sc (in-hash-keys s-scs)])
    (hash-union syms
                (binding-table-symbols (scope-binding-table sc)
                                       s-scs s null subset?))))


(module+ test
  (require rackunit)
  (test-case
   "quote-syntax local"
   (let ([x 1] [y 1])
     (define syms
       (syntax-mapped-names (quote-syntax a #:local)))
     (check-not-false (hash-has-key? syms 'x))
     (check-not-false (hash-has-key? syms 'memq))
     (check-not-false (hash-has-key? syms 'check-not-false))
     (check-not-false (hash-has-key? syms 'syms))))
  (test-case
   "quote-syntax"
   (let ([x 1] [y 1])
     (define syms
       (syntax-mapped-names (quote-syntax a)))
     (check-false (hash-has-key? syms 'x))
     (check-not-false (hash-has-key? syms 'memq))
     (check-not-false (hash-has-key? syms 'check-not-false))
     (check-false (hash-has-key? syms 'syms))))
  
  (define id
    (let ([x 1] [y 1])
      (quote-syntax a #:local)))
  
  (test-case
   "local"
   (check-equal?
    (syntax-mapped-names id
                         #:multi? #f #:subset? #f)
    #hasheq((x . #t) (y . #t)))))