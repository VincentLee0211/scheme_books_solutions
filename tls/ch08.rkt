#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/16 2015
|#

;; Lambda the Ultimate

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

;; 指定移除函数的, 删除操作
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a)
       (rember-f test? a (cdr l)))
      (else (cons (car l)
                  (rember-f test? a (cdr l)))))))

(rember-f = 5 '(6 2 5 3))

(rember-f eq? 'jelly '(jelly beans are good))

(rember-f equal? 
          '(pop corn) 
          '(lemonade (pop corn) and (cake)))

;; Curry-ing
(define eq-c?
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad (eq-c? 'salad))

(eq?-salad 'salad)

(eq?-salad 'tuna)

;; 使用Curry-ing重写rember-f
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a)
         ((rember-f test?) a (cdr l)))
        (else (cons (car l) 
                    ((rember-f test?) a (cdr l))))))))

((rember-f eq?) 'tuna '(tuna salad is good))

((rember-f eq?) 'tuna '(shrimp salad and tuna salad))

((rember-f eq?) 'tuna '(equal? eq? eqan? eqlist? eqpair?))

;; 使用Curry-ing重写insertL insertR
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old (cdr l))))))))

;; 抽象出共同模式
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

(define insertR
  (insert-g (lambda (new old l)
              (cons old (cons new l)))))

(define insertL
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

(define subst
  (insert-g (lambda (new old l)
              (cons new l))))

(define rember
  (lambda (a l)
    ((insert-g (lambda (new old l)
                l))
     #f a l)))

(rember 'a '(a b c a))
  
;; 使用抽象化, 化简value函数(第七节)
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '^+)
       (^+ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '^*)
       (^* (value (1st-sub-exp nexp))
           (vlaue (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '^^)
       (^^ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '^+) ^+)
      ((eq? x '^*) ^*)
      ((eq? x '^^) ^^))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-funtion (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))))

;; 使用Curry-ing重写multirember
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'tuna
                     '(shrimp salad tuna salad and tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

(multiremberT (lambda (x)
                (eq? 'tuna x))
              '(shrimp sald tuna salad and tuna))
             
;; comtinuaton, 变现的类似于栈, 符号都是先进后出的
;; 将与a相同的存入seen
;; 将与a不同的存入newlat
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a 
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna
                '(strawberries tuna and swordfish)
                a-friend)

(multirember&co 'tuna
                '()
                a-friend)
         
(multirember&co 'tuna
                '(tuna)
                a-friend)

;; 多插入
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new 
             (cons old 
                   (multiinsertL new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old 
             (cons new
                   (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new 
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else  (cons (car lat)
                   (multiinsertLR new oldL oldR (cdr lat)))))))
       
;; 使用continuation完成multiinsertLR操作的多返回
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col 0 0 '()))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new 
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (L R newlat)
                           (col (add1 L)
                                R
                                (cons new
                                      (cons (car lat) newlat))))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (L R newlat)
                           (col L
                                (add1 R)
                                (cons (car lat)
                                      (cons new newlat))))))
      (else (multiinsertLR&co new
                              oldL
                              oldR
                              (cdr lat)
                              (lambda (L R newlat)
                                (col L
                                     R
                                     (cons (car lat) newlat))))))))

(multiinsertLR&co 'salty
                  'fish
                  'chips
                  '(chips and fish or fish and chips)
                  (lambda (L R newlat)
                    (cons L (cons R newlat))))

;; 剔除奇数
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

;; 使用continuation计算, 列表中的奇数和, 偶数积, 返回只有偶数的列表
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col 0 1 '()))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                         (lambda (s p newlat)
                           (col s
                                (* (car l) p)
                                (cons (car l) newlat)))))
         (else (evens-only*&co (cdr l)
                              (lambda (s p newlat)
                                (col (+ (car l) s)
                                     p
                                     newlat))))))
      (else (evens-only*&co (car l)
                           (lambda (s1 p1 newlat1)
                             (evens-only*&co (cdr l)
                                            (lambda (s2 p2 newlat2)
                                              (col (+ s1 s2)
                                                   (* p1 p2)
                                                   (cons newlat1 newlat2))))))))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                (lambda (s p newlat)
                  (cons s (cons p newlat))))
              