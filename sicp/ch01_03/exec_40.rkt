#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 定义过程cubic

(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (expt x 3)
         (* a (expt x 2))
         (* b x)
         c))))

;; 牛顿转换
(define newton-transform
  (lambda (g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x))))))

(define deriv
  (lambda (g)
    (define dx 0.00001)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx))))

(define fixed-point-of-transform
  (lambda (g transform guess)
    (fixed-point (transform g) guess)))

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

;; test
(fixed-point-of-transform (cubic 1 2 3) newton-transform 1.0)