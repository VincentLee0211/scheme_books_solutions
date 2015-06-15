#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/15 2015
|#

;; Cons the Magnificent

;; 辅助函数
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

;; 移除元素第一次出现的位置
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(rember 'mint
        '(lamb chops and mint jelly))

(rember 'mint
        '(lamb chops and mint flavored mint jelly))

(rember 'toast
        '(bacon lettuce and tomato))

(rember 'cup
        '(coffee cup tea cup and hick cup))

;; 获取列表组合的第一个元素
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))

(firsts '((a b) (c d) (e f)))

(firsts '())

(firsts '((five plums)
          (four)
          (eleven green oranges)))

(firsts '(((five plums) four)
          (eleven gree orangs)
          ((no) more)))

;; 将元素A插到列表中第一次出现的元素B的右边
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'topping
         'fudge
         '(ice cream with fudge for dessert))

(insertR 'jalapeno
         'and
         '(tacos tamales and salsa))

(insertR 'e 'd '(a b c d f g d h))

;; 将元素A插到列表中第一次出现的元素B的左边
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new lat))
      (else
       (cons (car lat)
             (insertL new old (cdr lat)))))))

;; 将元素A替换列表中第一次出现的元素B
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else
       (cons (car lat)
             (subst new old (cdr lat)))))))

(subst 'topping
       'fudge
       '(ice cream with fudge for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1)
           (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else
       (cons (car lat)
             (subst2 new o1 o2 (cdr lat)))))))

(subst2 'vanilla
        'chocolate
        'banana
        '(banana ice cream with chocolate topping))

;; 删除列表中所有元素A
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))

(multirember 'cup
             '(coffee cup tea cup and hick cup))

;; 插入元素A到列表中所有元素B的右边
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old 
             (cons new 
                   (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat) 
             (multiinsertR new old (cdr lat)))))))

(multiinsertR 'fried
              'fish
              '(chips and fish or fish and fried))

;; 插入元素A到列表不要所有元素B的左边
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new 
             (cons (car lat) 
                   (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat) 
             (multiinsertL new old (cdr lat)))))))

(multiinsertL 'fried
              'fish
              '(chips and fish or fish and fried))

;; 替换列表中所有元素A到元素B
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new 
             (subst new old (cdr lat))))
      (else
       (cons (car lat) 
             (subst new old (cdr lat)))))))