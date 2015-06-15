#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 函数的平滑函数
(define smooth
  (lambda (f)
    (lambda (x)
      (define dx 0.00001)
      (/ (+ (f (+ x dx))
            (f x)
            (f (- x dx)))
         3.0))))


;; 重复执行函数
(define repeated
  (lambda (f times)
    (lambda (x)
      (if (= times 1)
          (f x)
          (f ((repeated f (- times 1)) x))))))

;; 重复平滑n次
(define smooth-n
  (lambda (f n)
    (lambda (x)
      ((repeated (smooth f) n) x))))

(define square (lambda (x) (expt x 2)))

((repeated square 2) 5)

((smooth square) 2)

((smooth-n square 4) 2)