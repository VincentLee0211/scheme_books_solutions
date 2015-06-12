#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 有奇葩过程
(define A
  (lambda (x y)
    (cond
      ((= y 0) 0)
      ((= x 0) (* 2 y))
      ((= y 1) 2)
      (else (A (- x 1)
               (A x (- y 1)))))))

;; (A 0 n) = 2 * n
;; (A 1 n) = (expt 2 n)
;; (A 2 n) = (expt 2 (expt 2 (expt 2 ... (expt 2 2))))  n-1个(expt 2 2)
;; (A 3 n) = (A 2 (A 2 ... (A 3 1))) = (A 2 (A 2 ... 2)) n-1个(A 2 2)

;; (expt 2 10) = 1024
(A 1 10)

;; (expt 2 (expt 2 (expt 2 2))) = 65536
(A 2 4)

;; (A 2 (A 2 2)) = 65536
(A 3 3)

;; f(n) = 2 * n
(define f (lambda (n) (A 0 n)))

;; g(n) = 2 ^ n
(define g (lambda (n) (A 1 n)))

;; h(n) = 2 ^ 2 ... ^ 2
(define h (lambda (n) (A 2 n)))

;; k(n) = 5 * n * n
(define k (lambda (n) (* 5 n n)))