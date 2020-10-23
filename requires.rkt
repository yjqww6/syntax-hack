#lang racket
(require "main.rkt"
         syntax/kerncase racket/set)

(define (walk stx [phase (namespace-base-phase)])
  (define h (mutable-seteq))
  (define (add-ids s phase)
    (for ([id (in-hash-keys
               (syntax-mapped-names s phase))])
      (set-add! h id)))
  (let loop ([stx stx] [phase phase])
    
    (define (loop* body phase)
      (for ([s (in-list (syntax->list body))])
        (loop s phase)))
    
    (kernel-syntax-case/phase
     stx phase
     [(module _ lang (_ body ...))
      (begin
        (add-ids #'lang 0)
        (loop #'(body ...) 0))]
     [(module* _ lang (_ body ...))
      (not (syntax-e #'lang))
      (begin
        (add-ids #'lang phase)
        (loop #'(body ...) phase))]
     [(module* _ lang (_ body ...))
      (begin
        (add-ids #'lang 0)
        (loop #'(body ...) 0))]
     [(begin-for-syntax expr ...)
      (loop #'(expr ...) (add1 phase))]
     [(begin expr ...)
      (loop #'(expr ...) phase)]
     [_ (void)]))
  h)

(module+ test
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (define s
      (walk
       (expand '(module a racket/base
                  (module b racket/base
                    (define x 1)
                    (provide x))
                  (require (prefix-in g: 'b))))))
    (set-member? s 'g:x)))