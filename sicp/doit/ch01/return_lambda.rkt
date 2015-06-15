#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 过程作为返回值

;; 辅助函数
(define average (lambda (x y) (/ (+ x y) 2)))

;; 平均阻尼
(define average-damp
  (lambda (f)
    (lambda (x)
      (average x (f x)))))

;; 函数不动点
(define fixed-point
  (lambda (f guess)
    (define try
      (lambda (guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next)))))
    (define close-enough?
      (lambda (x y)
        (< (abs (- x y)) 0.0001)))
    (try guess)))

;; 平方根
(define sqrt_
  (lambda (x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)))

(sqrt_ 2)

;; 立方根
(define cube-root
  (lambda (x)
    (fixed-point (average-damp (lambda (y) (/ x (expt y 2)))) 1.0)))

(cube-root 2)

;; 牛顿法
;; 若函数x->g(x)是一个可微函数, 那么方程g(x)=0的一个解就是函数x->f(x)的不动点
;; 其中 f(x) = x - g(x) / Dg(x)
;; Dg(x) = (g(x+dx) - g(x)) / dx

(define dx 0.00001)

(define deriv
  (lambda (g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx))))

;; 牛顿转换, 就是就一个可微函数的微分
(define newton-transform
  (lambda (g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x))))))

(define newtons-method
  (lambda (g guess)
    (fixed-point (newton-transform g) guess)))

;; 使用牛顿法求平方根
(define sqrt_vx
  (lambda (x)
    (newtons-method (lambda (y) (- (expt y 2) x)) 1.0)))

(sqrt_vx 2)

;; 抽象不动点搜寻和牛顿法
(define fixed-point-of-transform
  (lambda (g transform guess)
    (fixed-point (transform g) guess)))

(define sqrt_v1 
  (lambda (x)
    (fixed-point-of-transform
     (lambda (y) (/ x y))
     average-damp
     1.0)))

(define sqrt_v2
  (lambda (x)
    (fixed-point-of-transform
     (lambda (y) (- (expt y 2) x))
     newton-transform
     1.0)))

(sqrt_v1 2)
(sqrt_v2 2)