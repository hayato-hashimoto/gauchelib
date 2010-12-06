(define-module util.monad
  (use gauche.mop.singleton)
  (export Just unJust Nothing nothing? unitM bindM return >>= seqM mapM foldM))

(select-module util.monad)

(define-class <Monad> ()
  ((unitM :allocation :each-subclass :getter unit-function)))

(define-method unitM ((m <Monad>) a)
  ((unit-function m) a))
(define-generic bindM)

(define return unitM)
(define (multiple-bindM m a . b)
  (if (null? b)
    (bindM m a)
    (apply multiple-bindM `(,(bindM m a) ,@b))))
(define >>= multiple-bindM)

(define (Just . a) (make <Just> :just (lambda () (apply values a))))
(define (Nothing)  (make <Nothing>))
(define-class <Maybe> (<Monad>)
  ((unitM :init-value Just)))
(define-class <Just> (<Maybe>)
  ((cont :init-keyword :just :getter unJust-cps)))
(define-class <Nothing> (<Maybe>) () :metaclass <singleton-meta>)

(define-method unJust ((m <Just>))
  ((unJust-cps m)))

(define-method bindM ((m <Just>) f)
  (call-with-values (unJust-cps m) f))
(define-method bindM ((m <Nothing>) f)
  (Nothing))

(define-method nothing? ((maybe <Nothing>)) #t)
(define-method nothing? ((maybe <Just>)) #f)

(define-method write-object ((a <Just>) port)
  (display 
    (format "#<<Just> ~a>" 
      (call-with-values 
        (unJust-cps a)
        (lambda x (cond 
          ((null? x) "#values ()")
          ((null? (cdr x)) (car x))
          (else (format "#values ~a" x))))))
    port))
(define-method write-object ((a <Nothing>) port)
  (display (format "#<Nothing>") port))

(define (seqM ms)
  (define (mcons p q)
    (bindM p (lambda (x) (bindM q (lambda (y) (unitM (car ms) (cons x y)))))))
  (fold-right mcons (unitM (car ms) '()) ms))

(define (mapM proc args)
  (seqM (map proc args)))

(define (foldM proc seed lis)
  (if (null? lis)
    '()
    (let1 m (proc (car lis) seed)
      (bindM m (lambda (s) (foldM-internal m proc s (cdr lis)))))))

(define (foldM-internal m proc seed lis)
  (if (null? lis)
    (unitM m seed)
    (let1 m (proc (car lis) seed)
      (bindM m (lambda (s) (foldM-internal m proc s (cdr lis)))))))

(provide "util/monad")
