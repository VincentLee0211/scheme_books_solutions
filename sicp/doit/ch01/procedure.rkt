#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 过程作为参数

;; 过程: 计算从a到b的各整数和
(define sum-integers
  (lambda (a b)
    (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b)))))

;; 过程: 计算给定范围的整数立方之和
(define sum-cubes
  (lambda (a b)
    (if (> a b)
        0
        (+ (expt a 3) (sum-cubes (+ a 1) b)))))

;; 过程: 计算序列和
(define pi-sum
  (lambda (a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b)))))

;; 定义抽象模式
(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)))))

;; 辅助函数
(define identity (lambda (x) x))

(define add1 (lambda (x) (+ x 1)))

(define cube (lambda (x) (expt x 3)))

(define pi-term (lambda (x) (/ 1.0 (* x (+ x 2)))))

(define pi-next (lambda (x) (+ x 4)))

;; 使用抽象过程表述上述求和
(define sum-integers_
  (lambda (a b)
    (sum identity a add1 b)))

(define sum-cubes_
  (lambda (a b)
    (sum cube a add1 b)))

(define pi-sum_
  (lambda (a b)
    (sum pi-term a pi-next b)))

(sum-integers 1 100)
(sum-integers_ 1 100)

(sum-cubes 1 100)
(sum-cubes_ 1 100)

(* 8 (pi-sum 1 1000))
(* 8 (pi-sum_ 1 1000))

;; 求函数f在区间a, b之间的定积分
(define integral
  (lambda (f a b dx)
    (define init (+ a (/ dx 2)))
    (define next (lambda (x) (+ x dx)))
    (* (sum f init next b)
       dx)))

(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)