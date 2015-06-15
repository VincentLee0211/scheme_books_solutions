#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 辅助函数
(define square (lambda (x) (expt x 2)))
(define add1 (lambda (x) (+ x 1)))

;; 返回执行两次的过程

(define double 
  (lambda (f)
    (lambda (x)
      (f (f x)))))

;; 判断下面的过程
(((double (double double)) add1) 5)

#|
(lambda (x)
  ((double double) ((double double) x)))

((double double) ((double double) add1))

;; (double double) 化简
(lambda (x)
  (double (double x)))

;; ((double double) add1) 化简
((lambda (x)
   (double (double) x))
 add1)

(double (double add1))
|#

((double (double (double (double add1)))) 5)

;; (double (double (double (double add1))))

;; 没多一层(double ...) 函数f被执行2^n次

;; 则此处被add1 被执行2^4=16次
;; 16 + 5 = 21

#|
(double (double f))

(lambda (x)
  ((double f) ((double f) x)))

(lambda (x)
  ((lambda (y)
     (f (f y)))
   ((lambda (z)
      (f (f z)))
    x)))

(lambda (x)
  ((lambda (y)
     (f (f y)))
   (f (f x))))

(lambda (x)
  (f (f (f (f x)))))
|#
          




