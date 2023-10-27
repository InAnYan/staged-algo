#lang racket

(define current-stages empty)

(define-syntax-rule (define-stages name FNS ...)
  (define (name arg)
    (let ([goal ((apply compose
                        (reverse
                         (list (lambda (x)
                                 (let ([res (FNS x)])
                                   (push-stage-info 'FNS res)
                                   res))
                               ...))) arg)])
      (wrap-stages 'name (length (list FNS ...)))
      goal)))

(define (push-stage-info fn-name res)
  (if (and (not (empty? current-stages))
           (list? (first current-stages))
           (second (first current-stages))
           (eq? fn-name (first (first current-stages))))
      current-stages
      (push-info fn-name res)))

(define (push-info fn-name res)
  (set! current-stages
        (cons (list fn-name #f res)
              current-stages))
  current-stages)

(define (wrap-stages fn-name n)
  (set! current-stages
        (cons (list fn-name #t (take current-stages n))
              (drop current-stages n)))
  current-stages)

(define (clear-current-stages)
  (set! current-stages empty))

(provide define-stages
         current-stages
         clear-current-stages)
