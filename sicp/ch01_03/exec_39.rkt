#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 使用连分式求tan

(define cont-frac
  (lambda (n d k)
    (define recv
      (lambda (count)
        (if (= count k)
            (/ (n count) (d count))
            (/ (n count) (+ (d count) (recv (+ count 1)))))))
    (recv 1)))

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           1000)

(define tan_
  (lambda (x k)
    (cont-frac (lambda (n)
                 (if (= n 1)
                     x
                     (- (expt x 2))))
               (lambda (n)
                 (- (* 2 n) 1))
               k)))

(tan_ 3.14 1000)
(tan_ (/ 3.14 4) 1000)