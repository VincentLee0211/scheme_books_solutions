#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 求一个数的立方根

(define cube-root
  (lambda (x)
    (define iter
      (lambda (guess)
        (let ((next (improve guess)))
          (if (good-enough? guess next)
              next
              (iter next)))))
    (define improve
      (lambda (guess)
        (/ (+ (/ x (expt guess 2))
              (* 2 guess))
           3)))
    (define good-enough? 
      (lambda (guess next)
        (< (abs (- guess next))
           (abs (* 0.001 next)))))
    (iter 1.0)))