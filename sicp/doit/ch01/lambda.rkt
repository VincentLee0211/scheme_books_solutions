#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 使用lambda重写过程

;; 求和概念
(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)))))

;; 求区间[a,b]的和
(define sum-integers
  (lambda (a b)
    (sum (lambda (x) x)
         a
         (lambda (x) (+ x 1))
         b)))

(sum-integers 1 100)

;; 求区间[a,b]的立方和
(define sum-cubes
  (lambda (a b)
    (sum (lambda (x) (expt x 3))
         a
         (lambda (x) (+ x 1))
         b)))

(sum-cubes 1 100)

;; 求pi近似值
(define pi-sum
  (lambda (a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
         a
         (lambda (x) (+ x 4))
         b)))

(* 8.0 (pi-sum 1 1000))

;; 区间[a,b]定积分
(define integral
  (lambda (f a b dx)
    (* (sum f
            (+ a (/ dx 2))
            (lambda (x) (+ x dx))
            b)
       dx)))

(integral (lambda (x) (expt x 3)) 0 1 0.0001)

;; --------------------------------------------

;; 使用let定义局部变量

;; 求方程f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
(define f1
  (lambda (x y)
    (define f-helper
      (lambda (a b)
        (+ (* x (expt a 2))
           (* y b)
           (* a b))))
    (f-helper (+ 1 (* x y))
              (- 1 y))))

(define f2
  (lambda (x y)
    ((lambda (a b)
       (+ (* x (expt a 2))
          (* y b)
          (* a b)))
     (+ 1 (* x y))
     (- 1 y))))

(define f3
  (lambda (x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
      (+ (* x (expt a 2))
         (* y b)
         (* a b)))))
             