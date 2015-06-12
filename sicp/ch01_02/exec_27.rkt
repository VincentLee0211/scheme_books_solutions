#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

(define carmichael
  (lambda (n)
    (define iter
      (lambda (test)
        (cond
          ((= test n) #t)
          ((= (expmod test n n) test) (iter (+ test 1)))
          (else #f))))
    (iter 1)))

(define expmod
  (lambda (base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp)
       (remainder (expt (expmod base (/ exp 2) m) 2) m))
      (else
       (remainder (* base (expmod base (- exp 1) m)) m)))))

(carmichael 561)

(carmichael 1105)

(carmichael 1729)

(carmichael 2465)

(carmichael 2821)

(carmichael 6601)