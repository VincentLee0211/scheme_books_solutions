#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 验证解释器的求值规则, 正则序 or 应用序

(define p
  (lambda () 
    (p)))

(define test
  (lambda (x y)
    (if (= x 0)
        0
        (y))))

(test 0 (p))

;; 应用序
;; 求值(test 0 (p)), 需要求值test, 0, (p)
;; 在求值(p)时无法停机

;; 正则序
;; 求值(test 0 (p)), 此时展开过程(test)
;; 求值(if)表达式
;; 返回结果0