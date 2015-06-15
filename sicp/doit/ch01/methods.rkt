#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 辅助函数
(define square (lambda (x) (expt x 2)))
(define cube (lambda (x) (expt x 3)))

(define close-enough? (lambda (x y) (< (abs (- x y)) 0.0001)))

(define average (lambda (x y) (/ (+ x y) 2)))

;; 通过区间折半寻找方程的根
(define search
  (lambda (f neg-point pos-point)
    (let ((mid-point (average neg-point pos-point)))
      (if (close-enough? neg-point pos-point)
          mid-point
          (let ((tv (f mid-point)))
            (cond
              ((negative? tv)
               (search f mid-point pos-point))
              ((positive? tv)
               (search f neg-point mid-point))
              (else mid-point)))))))

;; 前置函数, 合法调用search函数
(define half-interval-method
  (lambda (f a b)
    (let ((av (f a))
          (bv (f b)))
      (cond
        ((and (negative? av) (positive? bv))
         (search f a b))
        ((and (negative? bv) (positive? av))
         (search f b a))
        (else
         (error "Values are not of opposite sign" a b))))))

;; test
(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (expt x 3) (* 2 x) 3))
                      1.0
                      2.0)

;; 求函数不动点
(define fixed-point
  (lambda (f guess)
    ;; 本来需要再次添加(close-enough? ...)函数, 但是上面已经存在, 该处不添加
    (define try
      (lambda (guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next)))))
    (try guess)))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

;; 求sqrt
#|
;; 无法停机, 震荡函数, 无法收敛
(define sqrt_
  (lambda (x)
    (fixed-point (lambda (y) (/ x y)) 1.0)))

(sqrt_ 2)
|#

(define sqrt_
  (lambda (x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0)))

(sqrt_ 2)