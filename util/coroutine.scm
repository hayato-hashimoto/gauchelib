(define-module util.coroutine
  (export define-coroutine make-coroutine yield end-of-coroutine?))

(select-module util.coroutine)

(use srfi-11)
(use gauche.partcont)
(use gauche.parameter)

(define-class <yield-values> ()
  ((continuation :init-keyword :cont)
   (retval :init-keyword :ret)))
(define-class <end-of-coroutine> () ())
(define-method write-object ((obj <end-of-coroutine>) port)
  (display "#<end-of-coroutine>" port))
(define (end-of-coroutine? obj)
  (is-a? obj <end-of-coroutine>))
(define-syntax define-coroutine
  (syntax-rules ()
    ((_ (routine . args) body ...)
      (define routine (make-coroutine (lambda args body ...))))))

(define (make-coroutine entry)
  (define k (make-parameter entry))
  (lambda args
    (let-values ((val (reset (apply (k) args))))
      (cond 
        ((and (pair? val) (is-a? (car val) <yield-values>))
          (k (ref (car val) 'continuation))
          (apply values (ref (car val) 'retval)))
        (else 
          (k (lambda _ (make <end-of-coroutine>)))
          (apply values val))))))

(define (yield . retval)
  (shift k (make <yield-values> :cont k :ret retval)))


;(use srfi-19)
;(use ansi-color)
;(use util.continue)

;  (define-coroutine (co-read status)
;    (continue (0 status)
;      (lambda (lc status)
;        (display (format "~a (~a) ~a> "
;          ((ansi-color 'red) "read")
;          lc status))
;        (flush)
;        (values (+ lc 1) (yield (read))))))

;  (define-coroutine (co-print retval)
;    (continue (0 retval)
;      (lambda (lc retval)
;        (print (format "~a (~a): ~a\n" ((ansi-color 'green) "print")lc retval))
;        (values (+ lc 1) (yield #t)))))

;  (continue ("ready")
;    (lambda (status) (co-print (eval (co-read status) (interaction-environment)))))
(provide "util/coroutine")
