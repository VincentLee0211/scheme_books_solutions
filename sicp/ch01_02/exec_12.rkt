#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 模拟帕斯卡三角形

(define pascal
  (lambda (row col)
    (cond
      ((or (<= row 0) (<= col 0) (< row col)) 0)
      ((or (= row 1) (= row col)) 1)
      (else (+ (pascal (- row 1) (- col 1))
               (pascal (- row 1) col))))))