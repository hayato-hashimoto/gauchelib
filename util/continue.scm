(define-module util.continue
  (export continue genloop guard))
(select-module util.continue)
(use gauche.partcont)

(define-macro (continue args fn)
  `((with-module gauche.partcont reset)
     (let1 break (lambda args ((with-module gauche.partcont shift) k (apply values args)))
       ((genloop ,fn) ,@args))))

(define (genloop fn)
  (define (loop . args)
    (call-with-values
      (lambda () (apply fn args)) loop))
  loop)

(define-macro (guard args test . body)
  `(lambda ,args (cond (((lambda ,args ,test) ,@args) ,@body)
                       (else (break ,@args)))))

(provide "util/continue")
