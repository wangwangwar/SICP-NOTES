;;; 3.3.1 变动的表结构
;;;
;;; cons 通过创建新序对的方式构造新的表,可以用两个改变函数和一个过程
;;; get-new-pair 实现。
;(define (cons x y)
;  (let ((new (get-new-pair)))
;   (set-car! new x)
;   (set-cdr! new y)
;   new))

;;; 过程 append!,与 append 类似,但它是一个改变函数而不是一个构造函数。
;;; 它将表拼接起来的方式是将两个表粘起来,修改 x 的最后一个序对,使它的
;;; cdr 现在变成 y （对空的 x 调用 append! 将是一个错误）。 (ex3.12)
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

;;; 过程 make-cycle,构造循环（链表）。 (ex3.13)
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;; 过程 mystery,(ex3.14)
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

;;; 共享和相等
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
(set-to-wow! z1)
(set-to-wow! z2)

;;; 有问题的 count-pairs (ex3.16)
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))
(define cons1
  (cons 1 1))
(define cons2
  (cons 2 2))
(define cons3
  (cons 3 3))
(set-car! cons2 cons3)
(set-cdr! cons2 cons3)
(set-car! cons1 cons2)
(set-cdr! cons1 cons2)
(define pair1
  (cons (cons 'x 'x) (cons 'x 'x)))
(define pair2
  (cons cons2 1))
(define pair3 cons1)
(define pair4
  (cons 1 (cons 2 (cons 3 '()))))
(set-cdr! (last-pair pair4) pair4)

;;; count-pairs 正确版本 (ex3.17)
;;; 注意遍历的写法
(define (get-travelled-pairs pairs memo-list)
  (if (or (not (pair? pairs)) (memq pairs memo-list))
    memo-list 
    (get-travelled-pairs (car pairs)
                         (get-travelled-pairs (cdr pairs) (cons pairs memo-list)))))
(define (new-count-pairs x)
  (length (get-travelled-pairs x '())))

;;; 写一个过程检查一个表,确定其中是否包含环,即,如果某个程序打算通过不断做
;;; cdr 去找到这个表的结尾,是否会陷入无穷循环。 (ex3.18)
(define (check-list-ring pairs)
  (define (check-list-ring-iter pairs memo-list)
    (if (not (pair? pairs))
      #f
      (if (memq pairs memo-list)
        #t
        (check-list-ring-iter (cdr pairs) (cons pairs memo-list)))))
  (check-list-ring-iter pairs '()))

;;; 重写上面的算法,只需要常量空间 (ex3.19)
(define (check-list-ring-smart pairs)
  (define (step-walk pairs step)
    (if (or (< step 1) (eq? pairs '()))
      pairs
      (step-walk (cdr pairs) (- step 1))))
  (define (check-list-ring-smart-iter pairs1 pairs2)
    (if (or (eq? pairs1 '()) (eq? pairs2 '()))
      #f
      (if (eq? pairs1 pairs2)
        #t
        (check-list-ring-smart-iter (step-walk pairs1 1) (step-walk pairs2 2)))))
  (check-list-ring-smart-iter (step-walk pairs 1) (step-walk pairs 2)))

;;; 改变也是赋值
;;; 自定义 cons 系列操作
; (define (cons x y)
;   (define (set-x! v) (set! x v))
;   (define (set-y! v) (set! y v))
;   (define (dispatch m)
;   (cond ((eq? m 'car) x)
;         ((eq? m 'cdr) y)
;         ((eq? m 'set-car!) set-x!)
;         ((eq? m 'set-cdr!) set-y!)
;         (else (error "Undefined operation -- CONS" m))))
;   dispatch)
; (define (car z) (z 'car))
; (define (cdr z) (z 'cdr))
; (define (set-car! z new-value)
;   ((z 'set-car!) new-value)
;   z)
; (define (set-cdr! z new-value)
;   ((z 'set-cdr!) new-value)
;   z)
