#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 修改fixed-point函数, 使其可以打印计算中产生的近似序列
(define fixed-point
  (lambda (f guess)
    (define try
      (lambda (guess)
        (print-line guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next)))))
    (define close-enough?
      (lambda (guess next)
        (< (abs (- guess next)) 0.0001)))
    (try guess)))

(define print-line
  (lambda (x)
    (newline)
    (display x)))

(define v
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))