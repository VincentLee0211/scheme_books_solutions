#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/16 2015
|#

;; Friends and Relations

;; 辅助函数
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

;; 符号A是否存在于列表中
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

;; 从列表中移除所有符号A
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) 
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

;; 取列表的列表的第一个符号
(define firsts
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (cons (car (car ls))
                  (firsts (cdr ls)))))))

;; 取列表的列表的第二个符号
(define seconds
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (cons (car (cdr (car ls)))
                  (firsts (cdr ls)))))))

;; 判断列表是否微集合
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '(apple 3 pear 4 9 apple 3 4))

;; 将列表转为集合
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

;; 将列表转为集合
(define makeset_
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset_ (multirember (car lat) (cdr lat))))))))
       
(makeset_ '(apple peach pear peach plum apple lemon peach))

;; 集合A是集合B的子集
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

(subset? '(5 chicken wings)
         '(5 hamburgers 2 pieces fried chicken and light duckling wings))

(subset? '(4 pounds of horseradish)
         '(four pounds chicken and 5 ounces horseradish))

;; 集合A与集合B相等
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '(6 large chickens with wings)
        '(6 chickens with large wings))

;; 集合A与集合B是否存在交集
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))

;; 集合A与集合B的交集
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni casserole)
           '(macaroni and cheese))

;; 集合A与集合B的并集
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

;; 集合A减集合B
(define setdiff
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (setdiff (cdr set1) set2))
      (else (cons (car set1)
                  (setdiff (cdr set1) set2))))))

(setdiff '(stewed tomatoes and macaroni casserole)
         '(macaroni and cheese))

;; 集合列表的交集
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

(intersectall '((a b c) (c a d e) (e f g h a b)))

(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))

;; 什么是序对
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(a-pair? '(pear pear))

(a-pair? '(3 7))

(a-pair? '((2) (pair)))

(a-pair? '(full (house)))

;; 构造序对
(define first 
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
       
;; rel, 序对的集合
(define rel?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (a-pair? (car l))
                 (not (member? (car l) (cdr l)))
                 (rel? (cdr l)))))))
              
(rel? '(apples peaches pumpkin pie))

(rel? '((apples peaches)
        (pumpkin pie)
        (apples peaches)))
       
(rel? '((apples peaches)
        (pumpkin pie)))

(rel? '((4 3) (4 2) (7 6) (6 2) (3 4)))

;; fun?
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

;; 反转pair
(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))

;; 反转rel
#|
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel))
                         (first (car rel)))
                  (revrel (cdr rel)))))))
|#
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))

(fullfun? '((grape raisin) 
            (plum prune)
            (stewed prune)))

(fullfun? '((grape raisin)
            (plum prune)
            (stewed grape)))