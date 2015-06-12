#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 书中示例
(define sqrt_v1
  (lambda (x)
    (define iter
      (lambda (guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess)))))
    (define good-enough?
      (lambda (guess)
        (< (abs (- guess (/ x guess))) 0.001)))
    (define improve
      (lambda (guess)
        (average guess (/ x guess))))
    (iter 1.0)))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

#|
;; 测试
(sqrt_v1 1000000000000000000000000000000)
(expt 1000000000000000000000000000000 1/2)

(sqrt_v1 10000000000000000000000000000000)
(expt 10000000000000000000000000000000 1/2)

;; 对于很小的数, 可能比精度还要小
(sqrt_v1 0.0000000000000001)
(expt 0.0000000000000001 1/2)
|#

(define sqrt_v2
  (lambda (x)
    (define iter
      (lambda (guess)
        (let ((next (improve guess)))
          (if (good-enough? guess next)
              next
              (iter next)))))
    (define improve
      (lambda (guess)
        (average guess (/ x guess))))
    (define good-enough?
      (lambda (guess next)
        (< (abs (- guess next)) (* 0.001 next))))
    (iter 1.0)))