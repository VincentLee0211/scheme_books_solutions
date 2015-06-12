#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 定义if的新版本
(define new-if
  (lambda (predicate then-clause else-clause)
    (cond
      (predicate then-clause)
      (else else-clause))))
       
(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

;; new-if 是一个通用过程, 在调用(new-if )时, 需要参数全部求值(应用序)

;; if 是一个特殊过程, 在调用(if )时, 部分求值

;; 所以如果用new-if代替if在sqrt过程中求值, 会无法停机