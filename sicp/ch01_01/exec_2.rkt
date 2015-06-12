#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 将表达式转换为前缀表达式

(/ (+ 5
      4
      (- 2
         (- 3 
            (+ 6
               4/5))))
   (* 3
      (- 6 2)
      (- 2 7)))