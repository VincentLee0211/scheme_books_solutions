#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 使用(smallest-divisor ...)找出199, 1999, 19999的最小因子

(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))

(define find-divisor
  (lambda (n test)
    (cond
      ((> (expt test 2) n) n)
      ((divides? n test) test)
      (else (find-divisor n (next test))))))

(define divides?
  (lambda (n test)
    (= (remainder n test) 0)))

(define next
  (lambda (test)
    (if (= test 2)
        (+ test 1)
        (+ test 2))))
       
(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999)