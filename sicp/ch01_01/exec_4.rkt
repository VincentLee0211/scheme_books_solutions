#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 描述下面过程的行为

(define a-plus-abs-b
  (lambda (a b)
    ((if (< b 0) - +) a b)))

;; a + | b |