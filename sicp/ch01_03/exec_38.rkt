#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 求自然对数

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

(define e
  (lambda (n)
    (cont-frac (lambda (x) 1.0)
               (lambda (x)
                 (if (= (remainder x 3) 2)
                     (* 2 (+ 1 (quotient x 3)))
                     1))
               n)))

(+ 2.0 (e 1000))