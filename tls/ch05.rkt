#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/16 2015
|#

;; Oh My Gawd: It's Full of Stars

;; 辅助函数
(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1 
  (lambda (n)
    (- n 1)))

;; 移除列表中所有的元素A
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else
          (cons (car l)
                (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l))
             (rember* a (cdr l)))))))

(rember* 'cup
         '((coffee) cup ((tea) cup) (and (hick)) cup))

(rember* 'sauce
         '(((tomato sauce))
           ((bean) sauce)
           (and ((flying)) sauce)))

;; 纯列表
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (atom? (car l))
                 (lat? (cdr l)))))))

(lat? '(((tomato sauce))
        ((bean) sauce)
        (and ((flying)) sauce)))

(car '(((tomato sauce))
       ((bean) sauce)
       (and ((flying) sauce))))

;; 插入元素A到列表B的右边
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old 
                (cons new
                      (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(insertR* 'roast
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

;; 列表中, 统计符号A的数量
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

(occur* 'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))

;; 替换符号A为符号B
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

(subst* 'orange
        'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))
      
;; 将符号A插入到符号B的左边
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (cons old
                      (insertL* new old (cdr l)))))
         (else (cons (car l)
                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

;; 符号A是否存在
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       ;; (cond
         ;; ((eq? (car l) a) #t)
         ;; (else (member* a (cdr l)))))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(member* 'chips
         '((potato) (chips ((with) fish) (chips))))

;; 左侧原子
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish) (chips))))
      
(leftmost '(((hot) (tuna (and))) cheese))

;; (leftmost '(((() four)) 17 (seventeen)))

(and (atom? (car '(mozzarella pizza)))
     (eq? (car '(mozzarella pizza)) 'pizza))

;; 符号和数字判等
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) 
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

;; 相等
(define equal_?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else (eqlist? s1 s2)))))

;; 列表相等
#|
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2)))
       #f)
      (else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))
|#

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal_? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(strawberry ice cream)
         '(strawberry ice cream))

(eqlist? '(strawberry ice cream)
         '(strawberry cream ice))

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))

;; 重写rember
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) 
       (rember s (cdr l)))
      (else
       (cons (car l) (rember s (cdr l)))))))