#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 定义一个过程, 以三个数为参数, 返回其中较大的两个数之和

;; 可以查找三个数中最小的数, 返回另外两个数的和

(define max-two-sum
  (lambda (x y z)
    (cond
      ((< x y z) ;; x 最小
       (+ y z))
      ((< y z)   ;; y 最小
       (+ x z))
      (else      ;; z 最小
       (+ x y)))))