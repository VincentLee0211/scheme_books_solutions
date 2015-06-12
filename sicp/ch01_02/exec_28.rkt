#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

(#%require (only racket/base random))

;; 费马检查不受欺骗的变形

(define expmod
  (lambda (base exp m)
    (define non-trival-sqrt?
      (lambda (t)
        (cond
          ((or (= t 1) (= t (- m 1))) #f)
          (else (= 1 (remainder (expt t 2) m))))))
    (cond
      ((= exp 0) 1)
      ((even? exp)
       ;; (remainder (expt (expmod base (/ exp 2)) m) m))
       (let ((t (expmod base (/ exp 2) m)))
         (if (non-trival-sqrt? t)
             0
             (remainder (expt t 2) m))))
      (else
       (remainder (* base (expmod base (- exp 1) m)) m)))))

(define fast-prime?
  (lambda (n times)
    (cond
      ((= times 0) #t)
      ((fermat-test n) (fast-prime? n (- times 1)))
      (else #f))))

(define fermat-test
  (lambda (n)
    (define try
      (lambda (t)
        (= (expmod t (- n 1) n) 1)))
    (try (+ 1 (random (- n 1))))))

(fast-prime? 1105 100) ; #f

(fast-prime? 1009 100) ; #t

(fast-prime? 561 100)  ; #f

(fast-prime? 1729 100) ; #f