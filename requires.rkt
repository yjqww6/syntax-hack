#lang racket
(require "main.rkt"
         syntax/kerncase racket/set)
(provide requires)

(define (requires stx [phase (namespace-base-phase)])
  (define h (mutable-seteq))
  (define (add-ids s)
    (for ([phase (in-list (syntax-multi-scope-available-phases s))])
      (identifier-binding (datum->syntax s 'x) phase)
      (define names (syntax-mapped-names s phase))
      (for ([id (in-hash-keys names)])
        (set-add! h id))))
  (let loop ([stx stx] [phase phase])
    
    (define (loop* body phase)
      (for ([s (in-list (syntax->list body))])
        (loop s phase)))
    
    (kernel-syntax-case/phase
     stx phase
     [(module _ lang (_ body ...))
      (begin
        (add-ids #'lang)
        (loop* #'(body ...) 0))]
     [(module* _ lang (_ body ...))
      (not (syntax-e #'lang))
      (begin
        (add-ids #'lang)
        (loop* #'(body ...) phase))]
     [(module* _ lang (_ body ...))
      (begin
        (add-ids #'lang)
        (loop #'(body ...) 0))]
     [(begin-for-syntax expr ...)
      (loop* #'(expr ...) (add1 phase))]
     [(begin expr ...)
      (loop* #'(expr ...) phase)]
     [_ (void)]))
  h)

(module+ test
  (require rackunit)
  (test-case
   "inter require")
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (define s
      (requires
       (expand '(module a racket/base
                  (module b racket/base
                    (define x 1)
                    (provide x))
                  (module c racket/gui
                    (require (prefix-in k: (submod ".." b))))
                  (require (for-syntax (prefix-in g: 'b)))))))
    (check-true (set-member? s 'g:x))
    (check-true (set-member? s 'frame%))
    (check-true (set-member? s 'k:x))
    (check-true (set-member? s 'x))))