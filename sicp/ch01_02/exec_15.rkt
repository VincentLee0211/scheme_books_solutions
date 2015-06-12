#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 有如下过程

(define cube (lambda (x) (* x x x)))

(define p (lambda (x) (- (* 3 x) (* 4 (cube x)))))

(define sine
  (lambda (angle)
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0))))))

;; a
;; 12.15
;; 4.05
;; 1.35
;; 0.45
;; 0.15
;; 0.03
;; 总共5次
(sine 12.15)

;; b
;; x的值每次减小1/3
;; 时间复杂度 lg n
;; 空间复杂度 lg n
