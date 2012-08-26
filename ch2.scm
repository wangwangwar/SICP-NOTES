; vi: tabstop=2
(load "basic.scm")
(load "ch1.scm")
;;;;;;;;;;;;;;;;;;;
;;; 2.1 数据抽象导引
;;;;;;;;;;;;;;;;;;;


;;; 2.1.1 实例：有理数的算术运算
;;;
;;; 有理数的表示 (在构造时化简)
(define (make-rat n d)
  (if (< d 0)
	(let ((n (- n))
		  (d (- d)))
	  (let ((g (gcd n d)))
		(cons (/ n g) (/ d g))))
	(let ((g (gcd n d)))
	  (cons (/ n g) (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;;; 有理数的表示 (不在构造时而是在访问时化简)
(define (make-rat2 n d)
  (cons n d))

(define (numer2 x)
  (let ((g (gcd (car x) (cdr x))))
	(/ (car x) g)))

(define (denom2 x)
  (let ((g (gcd (car x) (cdr x))))
	(/ (cdr x) g)))

;;; 有理数的运算
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (denom x) (numer y)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (denom x) (numer y)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (denom x) (numer y))))


;;; 2.1.2 抽象屏障
;;;
;;; 点，线段 (ex2.2)
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
					(x-point (end-segment segment)))
				 2.0)
			  (/ (+ (y-point (start-segment segment))
					(y-point (end-segment segment)))
				 2.0)))

(define (length-segment segment)
  (sqrt (+ (expt (- (x-point (start-segment segment))
					(x-point (end-segment segment)))
				 2)
		   (expt (- (y-point (start-segment segment))
					(y-point (end-segment segment)))
				 2))))

;;; 平面矩形的表示 (ex2.3)
(define (make-square segment1 segment2)
  (cons segment1 segment2))

(define (x-square square)
  (car square))

(define (y-square square)
  (cdr square))

(define (circum square)
  (* 2.0 (+ (length-segment (x-square square))
			(length-segment (y-square square)))))

(define (area square)
  (* (length-segment (x-square square))
	 (length-segment (y-square square))))


;;; 2.1.3 数据意味着什么
;;;
;;; 定义 cons 过程，实现序对
(define (cons2 x y)
  (define (dispatch m)
	(cond ((= m 0) x)
		  ((= m 1) y)
		  (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car2 z) (z 0))

(define (cdr2 z) (z 1))

;;; 序对的另一种过程性表示 (ex2.4)
(define (cons3 x y)
  (lambda (m) (m x y)))

(define (car3 z)
  (z (lambda (p q) p)))

(define (cdr3 z)
  (z (lambda (p q) q)))

;;; 如果将 a 和 b 的序对表示为 2^a * 3^b 对应的整数，
;;; 我们就可以只用非负整数和算术运算表示序对。
;;; 写出过程 cons, car, cdr 的定义。(ex2.5)
(define (cons4 x y)
  (* (expt 2 x) (expt 3 y)))

;;; car 的递归实现，有栈满限制
(define (car4-r z)
  (if (even? z)
	(+ 1 (car4-r (/ z 2)))
	0))

;;; 迭代，无须担心栈满限制
(define (car4 z)
  (define (car4-iter z count)
	(if (even? z)
	  (car4-iter (/ z 2) (+ count 1))
	  count))
  (car4-iter z 0))

(define (cdr4 z)
  (define (cdr4-iter z count)
	(if (= (remainder z 3) 0)
	  (cdr4-iter (/ z 3) (+ count 1))
	  count))
  (cdr4-iter z 0))

;;; 我们可以完全没有数（至少在只考虑非负整数的情况下）,
;;; 可以将 0 和加一实现为： (ex2.6)
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f (n f) x))))


;;; 2.1.4 扩展练习：区间算术
;;;
;;; 区间抽象的实现 (ex2.7)
(define (make-interval a b) 
  (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

;;; 区间的加法
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

;;; 区间的乘法
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

;;; 区间的除法
(define (div-interval x y)
  (mul-interval x
				(make-interval (/ 1.0 (upper-bound y))
							   (/ 1.0 (lower-bound y)))))

;;; 区间的减法 (ex2.8)
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
				 (- (upper-bound x) (lower-bound y))))

;;; 除以一个横跨过 0 的区间意义不清楚。检查这种情况并在出现时报错。 (ex2.10)
(define (div-interval2 x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
	(error "区间 y 横跨了 0 -- INTERVAL")
  	(mul-interval x
				  (make-interval (/ 1.0 (upper-bound y))
								 (/ 1.0 (lower-bound y))))))

;;; 将 mul-interval 分为 9 种情况，重写 (ex2.11)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2 层次性数据和闭包性质
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 2.2.1 序列的表示
;;; 
;;; 取出表中第 n 个元素，(从 0 开始计数)
(define (list-ref items n)
  (if (= n 0)
	(car items)
	(list-ref (cdr items) (- n 1))))

;;; 求表长度，递归
(define (length-r items)
  (if (null? items)
	0
	(+ 1 (length-r (cdr items)))))

;;; 求表长度，迭代
(define (length-i items)
  (define (length-iter items count)
	(if (null? items)
	  count
	  (length-iter (cdr items) (+ count 1))))
  (length-iter items 0))

;;; 两个表的连接，递归
(define (append-r list1 list2)
  (if (null? list1)
	list2
	(cons (car list1) (append-r (cdr list1) list2))))
;;; 两个表的连接，迭代

;;; last-pair,返回只包含给定（非空）表里最后一个元素的表 (ex2.17)
(define (last-pair items)
  (list (list-ref items (- (length items) 1))))

;;; reverse,迭代 (ex2.18)
(define (reverse-i items)
  (define (reverse-iter items reversed-items)
	(if (null? items)
	  reversed-items
	  (reverse-iter (cdr items) (cons (car items) reversed-items))))
  (reverse-iter items '()))
;;; reverse,递归,有问题

;;; 重写兑换零钱方式计数程序，可以改变兑换币种 (ex2.19)
(define (cc amount coin-values)
  (define (no-more? items)
	(null? items))
  (define (except-first-denomination items)
	(cdr items))
  (define (first-denomination items)
	(car items))
  (cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else 
		  (+ (cc amount
				 (except-first-denomination coin-values))
			 (cc (- amount
					(first-denomination coin-values))
				 coin-values)))))

;;; 带点尾部记法形式的define，
;;; 写个 same-parity过程，它以一个或多个整数为参数，
;;; 返回与其第一个参数有着相同奇偶性的参数形成的表 (ex2.20)
(define (same-parity first . items)
  (define (same-parity-iter first items same-parity-items)
	(if (null? items)
	  same-parity-items
	  (if (= (remainder first 2) (remainder (car items) 2))
		(same-parity-iter first (cdr items) (cons (car items) same-parity-items))
		(same-parity-iter first (cdr items) same-parity-items))))
  (reverse (same-parity-iter first items (list))))
  
;;; 表的平方，递归 (ex2.21)
(define (square-list-r items)
  (if (null? items)
	'()
	(cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-m items)
  (map (lambda (x) (* x x)) items))

;;; 表的平方，迭代 (ex2.22)
(define (square-list-i items)
  (define (iter things answer)
	(if (null? things)
	  answer
	  (iter (cdr things)
			(cons (square (car things))
				  answer))))
  (iter items '()))

;;; for-each，以一个过程和一个元素表为参数，
;;; 将这个过程从左到右应用于各个元素，
;;; 将过程应用于元素得到的值都丢掉不用。
;;; for-each 通常用于那些执行了某些动作的过程，如打印。 (ex2.23)
(define (for-each proc items)
  (if (null? items)
	#t
	(and (proc (car items)) (for-each proc (cdr items)))))


;;; 2.2.2 层次性结构
;;;
;;; 计算一棵树中树叶的数目 
(define (count-leaves tree)
  (cond ((null? tree) 0)
		((not (pair? tree)) 1)
		(else (+ (count-leaves (car tree))
				 (count-leaves (cdr tree))))))

;;; 给出能够从下面各表中取出 7 的 car 和 cdr 组合。 (ex2.25)
(define x (list 1 3 (list 5 7) 9))
(define y (list (list 7)))
(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(display x)
(newline)
(display (car (cdr (car (cdr (cdr x))))))
(newline)
(display (car (cdaddr x)))
(newline)
(display y)
(newline)
(display (car (car y)))
(newline)
(display (caar y))
(newline)
(display z)
(newline)
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))))))
(newline)
(display (cadadr (cadadr (cadadr z))))
(newline)

;;; (ex2.26)
(define x (list 1 2 3))
(define y (list 4 5 6))
(display (append x y))
(newline)
(display (cons x y))
(newline)
(display (list x y))
(newline)

;;; deep-reverse 过程，将一棵树翻转过来，包括子树 (ex2.27) 网上答案
(define (deep-reverse lst)
  (define (iter lst-o lst-d)
	(cond ((null? lst-o)
		   lst-d)
		  ((not (pair? (car lst-o)))
		   (iter (cdr lst-o)
				 (cons (car lst-o) lst-d)))
		  (else
			(iter (cdr lst-o)
				  (cons (deep-reverse (car lst-o))
						lst-d)))))
  (iter lst '()))

;;; fringe，以一个树（表示为表）为参数，返回一个表，
;;; 表中元素是这棵树的所有树叶，按从左到右的顺序 (ex2.28)
;;; 迭代
(define (fringe-i x)
  (define (iter tree lst)
	(cond ((null? tree)
		   lst)
		  ((not (pair? tree))
		   (cons tree lst))
		  (else
			(iter (car tree) (iter (cdr tree) lst))))) ; 对pair的两部分都要进行iter。
  (iter x '()))

;;; 递归
(define (fringe-r tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (list tree))
		(else (append (fringe-r (car tree))
					  (fringe-r (cdr tree))))))
	
;;; 二叉活动体， (ex2.29)
;;; 表示一个二叉活动体
(define (make-mobile left right)
  (list left right))
;;; 表示一个分支
(define (make-branch length structure)
  (list length structure))
;;; 返回活动体的两个分支
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
;;; 返回一个分支上的成分
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
;;; 返回一个活动体的总重量
(define (total-weight mobile)
  (define (weight branch)
	(let ((s (branch-structure branch)))
	  (if (not (pair? s))
		s
		(+ (weight (left-branch s)) (weight (right-branch s))))))
  (+ (weight (left-branch mobile)) (weight (right-branch mobile))))
;;; 检查一个活动体是否平衡
(define (check-balance mobile)
  (define (weight branch)
	(let ((s (branch-structure branch)))
	  (if (not (pair? s))
		s
		(+ (weight (left-branch s)) (weight (right-branch s))))))
  (if (not (pair? mobile))
	#t
	(and (check-balance (branch-structure (left-branch mobile)))
		 (check-balance (branch-structure (right-branch mobile)))
		 (= (* (branch-length (left-branch mobile))
			   (weight (left-branch mobile)))
			(* (branch-length (right-branch mobile))
			   (weight (right-branch mobile)))))))

;;; square-tree ( ex2.30 )
;;; 普通定义方式
(define (square-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (* tree tree))
		(else (cons (square-tree (car tree))
					(square-tree (cdr tree))))))
;;; map方式
(define (square-tree-m tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		   (square-tree-m sub-tree)
		   (* sub-tree sub-tree)))
	   tree))

;;; square-tree, 再抽象一层 (ex2.31)
(define (tree-map square tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		   (tree-map square sub-tree)
		   (square sub-tree)))
	   tree))
(define (square-tree-h tree)
  (tree-map square tree))

;;; 生成一个集合的所有子集 (ex2.32)
(define (subsets s)
  (if (null? s)
	(list '())
	(let ((rest (subsets (cdr s))))
	  (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;; 2.2.3 序列作为一种约定的界面
;;;
;;; 过滤器
(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

;;; 累积器
(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

;;; 枚举器
;;; 枚举一个给定区间里的整数序列
(define (enumerate-interval low high)
  (if (> low high)
	'()
	(cons low (enumerate-interval (+ low 1) high))))

;;; 枚举一棵树的所有树叶（即fringe）
(define (enumerate-tree tree)
  (fringe-r tree))

;;; sum-odd-squares, 枚举一棵树的树叶序列，过滤它，
;;; 只留下序列中的奇数，求每个元素的平方，然后加起来得到结果
(define (sum-odd-squares tree)
  (accumulate +
			  0
			  (map square
				   (filter odd?
						   (enumerate-tree tree)))))

;;; even-fibs, 枚举从 0 到 n 的所有整数，
;;; 对某个整数生成相应的菲波那契数，通过过滤只剩下其中的偶数，
;;; 并将结果积累在一个表中
(define (even-fibs n)
  (accumulate cons
			  '()
			  (filter even?
					  (map fib
						   (enumerate-interval 0 n)))))

;;; 提取前面的模块，构造一个程序，
;;; 这个程序构造前 n＋1 个斐波那契数的平方的列表
(define (list-fib-squares n)
  (accumulate cons
			  '()
			  (map square
				   (map fib
						(enumerate-interval 0 n)))))

;;; 将一些基本的表操作看作累积 ( ex2.33 )
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;; 用 Horner 规则求多项式的值 ( ex2.34 )
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
			  0
			  coefficient-sequence))

;;; 将 count-leaves 重新定义为一个累积 ( ex2.35 )
(define (count-leaves-beta t)
  (accumulate +
			  0 
			  (map (lambda (tree)
					(cond ((null? tree) 0)
					  	  ((not (pair? tree)) 1)
					  	  (else (+ (count-leaves (car tree))
							 	   (count-leaves (cdr tree))))))
				   t)))

;;; 过程 accumulate-n 与 accumulate 类似，
;;; 除了他的第三个参数是一个序列的序列，
;;; 假定其中每个序列的元素个数相同。
;;; 如果 s 是包含 4 个序列的序列 ((1 2 3) (4 5 6) (7 8 9) (10 11 12))，
;;; 那么 (accumulate-n + 0 s) 的值就是序列 (22 26 30) (ex2.36)
;(define (accumulate-n op init seqs)
;  (if (null? (car seqs))
;	'()
;	(cons (accumulate op init (accumulate cons '() 
;		  (accumulate-n op init (cdar seqs)))))



;;;;;;;;;;;;;;;;;;;
;;; 2.3 符号数据
;;;;;;;;;;;;;;;;;;;


;;; 2.3.1 引号
;;;
;;; memq, 检查这个符号是否在表中
(define (memq item x)
  (cond ((null? x) #f)
		((eq? item (car x)) x)
		(else (memq item (cdr x)))))

;;; 打印什么 (ex2.53)
(print "---------- TEST memq -------------------------")
(print (list 'a 'b 'c))
(print (list (list 'george)))
(print (cdr '((x1 x2) (y1 y2))))
(print (cadr '((x1 x2) (y1 y2))))
(print (pair? (car '(a short list))))
(print (memq 'red '((red shoes) (blue socks))))
(print (memq 'red '(red shoes blue socks)))
(print "---------- END  memq -------------------------")

;;; 定义过程 equal? (ex2.54)
(define (equal? item-1 item-2)
  (cond ((and (symbol? item-1) (symbol? item-2))
         (eq? item-1 item-2))
        ((and (number? item-1) (number? item-2))
         (eq? item-1 item-2))
        ((and (null? item-1) (null? item-2))
         #t)
        ((and (pair? item-1) (pair? item-2))
         (and (equal? (car item-1) (car item-2))
              (equal? (cdr item-1) (cdr item-2))))
        (else
          #f)))
  
;;; 2.3.2 实例：符号求导
;;;
;;; 代数表达式的实现
;;; 变量？
(define (variable? x) (symbol? x))
;;; 相同的变量？
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;;; 这个表达式等于这个数？
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;;; 和式
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list '+ a1 a2))))
;;; 积式
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))))
;;; 和式？
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
;;; 被加数
(define (addend s) (cadr s))
;;; 加数
(define (augend s) (caddr s))
;;; 积式？
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
;;; 被乘数
(define (multiplier p) (cadr p))
;;; 乘数
(define (multiplicand p) (caddr p))

;;; 求导
(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv (addend exp) var)
				   (deriv (augend exp) var)))
		((product? exp)
		 (make-sum 
		   (make-product (multiplier exp)
						 (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var)
						 (multiplicand exp))))
		(else
		  (error "unknown expression type -- DERIV" exp))))

;;; 给 deriv 程序添加一个乘幂(**)符号，定义 exponentiation?, base, exponent,
;;; 和 make-exponentiation, 实现幂求导。
;;; 并加入这两个规则：任何东西的 0 次幂都是 1, 而它们的 1 次幂都是其自身。(ex2.56)
;;; 幂式
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
		((=number? exponent 1) base)
		(else (list '** base exponent))))
;;; 幂式？
(define (exponentiation? exponentiation)
  (and (pair? exponentiation) (eq? (car exponentiation) '**)))
;;; 取幂式的底数
(define (base exponentiation) (cadr exponentiation))
;;; 取幂式的指数
(define (exponent exponentiation) (caddr exponentiation))
;;; 求导--扩展幂式
(define (deriv-exp exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv-exp (addend exp) var)
				   (deriv-exp (augend exp) var)))
		((product? exp)
		 (make-sum 
		   (make-product (multiplier exp)
						 (deriv-exp (multiplicand exp) var))
		   (make-product (deriv-exp (multiplier exp) var)
						 (multiplicand exp))))
		((exponentiation? exp)
		 (make-product (exponent exp)
					   (make-product (make-exponentiation (base exp)
														  (make-sum (exponent exp) -1))
									 (deriv-exp (base exp) var))))
		(else
		  (error "unknown expression type -- DERIV" exp))))

;;; 扩展求导程序，使之能够处理任意多项的和与积。设法通过只修改和与积的表示，
;;; 而完全不修改过程 deriv 的方式完成这一扩充。 (ex2.57)
;;; 被加数
(define (addend-more s) (cadr s))
;;; 加数
(define (augend-more s)
  (let ((a2 (cddr s)))
	(if (eq? (cdr a2) '())
	  (car a2)
	  (append (list '+) a2))))
;;; 被乘数
(define (multiplier-more s) (cadr s))
;;; 乘数
(define (multiplicand-more s)
  (let ((a2 (cddr s)))
	(if (eq? (cdr a2) '())
	  (car a2)
	  (append (list '*) a2))))
;;; 求导--能处理任意多项
(define (deriv-more exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv-more (addend-more exp) var)
				   (deriv-more (augend-more exp) var)))
		((product? exp)
		 (make-sum 
		   (make-product (multiplier-more exp)
						 (deriv-more (multiplicand-more exp) var))
		   (make-product (deriv-more (multiplier-more exp) var)
						 (multiplicand-more exp))))
		(else
		  (error "unknown expression type -- DERIV" exp))))

;;; 将之前的前缀表达式改写成中缀表达式，只需要换一套新的谓词、选择函数和构造函数
;;; (ex2.58)
;;; a) 假定 + 和 * 总是取两个参数，而且表达式中已经加上了所有的括号。
;;; 和式
(define (make-sum-infix a1 a2)
  (cond ((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list a1 '+ a2))))
;;; 积式
(define (make-product-infix m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list m1 '* m2))))
;;; 和式？
(define (sum-infix? x)
  (and (pair? x) (eq? (cadr x) '+)))
;;; 被加数
(define (addend-infix s) (car s))
;;; 加数
(define (augend-infix s) (caddr s))
;;; 积式？
(define (product-infix? x)
  (and (pair? x) (eq? (cadr x) '*)))
;;; 被乘数
(define (multiplier-infix p) (car p))
;;; 乘数
(define (multiplicand-infix p) (caddr p))

;;; 求导
(define (deriv-infix exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum-infix? exp)
		 (make-sum-infix (deriv-infix (addend-infix exp) var)
				   (deriv-infix (augend-infix exp) var)))
		((product-infix? exp)
		 (make-sum-infix 
		   (make-product-infix (multiplier-infix exp)
						 (deriv-infix (multiplicand-infix exp) var))
		   (make-product-infix (deriv-infix (multiplier-infix exp) var)
						 (multiplicand-infix exp))))
		(else
		  (error "unknown expression type -- DERIV" exp))))

;;; 2.3.3 实例：集合的表示
;;;
;;; 集合作为未排序的表
;;; 判断元素 x 是否在集合 set 中
(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))
;;; 将元素 x 加入集合 set 
(define (adjoin-set x set)
  (if (element-of-set? x set)
	set
	(cons x set)))
;;; 求两个集合的交集
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
		 '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) set2)))
		(else
		  (intersection-set (cdr set1) set2))))
;;; 求两个集合的并集 (ex2.59)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2)
		 (union-set (cdr set1) set2))
		(else 
		  (cons (car set1)
				(union-set (cdr set1) set2)))))

;;; 集合作为排序的表
;;; 判断元素 x 是否在集合 set 中
(define (element-of-ordered-set? x set)
  (cond ((null? set) #f)
		((= (car set) x) #t)
		((> (car set) x) #f)
		(else (element-of-ordered-set? x (cdr set)))))
;;; 将元素 x 加入集合 set，过程类似 element-of-ordered-set? (ex 2.61)
(define (adjoin-ordered-set x set)
  (cond ((null? set) (list x))
		((= x (car set)) set)
		((< x (car set)) (cons x set))
		(else (cons (car set) (adjoin-ordered-set x (cdr set))))))
;;; 求两个集合的交集
(define (intersection-ordered-set set1 set2)
  (cond ((or (null? set1) (null? set2))
		 '())
		((= (car set1) (car set2))
		 (cons (car set1)
			   (intersection-ordered-set (cdr set1) (cdr set2))))
		((< (car set1) (car set2))
		 (intersection-ordered-set (cdr set1) set2))
		(else (intersection-ordered-set set1 (cdr set2)))))
;;; 求两个集合的并集 (ex2.62)
(define (union-ordered-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((= (car set1) (car set2))
		 (cons (car set1) (union-ordered-set (cdr set1) (cdr set2))))
		((< (car set1) (car set2))
		 (cons (car set1) (union-ordered-set (cdr set1) set2)))
		(else
		  (cons (car set2) (union-ordered-set set1 (cdr set2))))))

;;; 集合作为二叉树
;;; 本节点数据项
(define (entry tree) (car tree))
;;; 左子树
(define (left-child tree) (cadr tree))
;;; 右子树
(define (right-child tree) (caddr tree))
;;; 创建一棵树
(define (make-tree entry left right)
  (list entry left right))
;;; 判断元素 x 是否在集合 set 中
(define (element-of-tree-set? x set)
  (cond ((null? set) #f)
		((= x (entry set)) #t)
		((> x (entry set)) (element-of-tree-set? x (right-child set)))
		(else (element-of-tree-set? x (left-child set)))))
;;; 将元素 x 加入集合 set
(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
		 (make-tree (entry set)
			  (adjoin-tree-set x (left-child set))
			  (right-child set)))
		(else
		  (make-tree (entry set)
			   (left-child set)
			   (adjoin-tree-set x (right-child set))))))
;;; 将树变换为表 (ex2.63)
(define (tree->list-1 tree)
  (if (null? tree)
	'()
	(append (tree->list-1 (left-child tree))
			(cons (entry tree)
				  (tree->list-1 (right-child tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
	(if (null? tree)
	  result-list
	  (copy-to-list (left-child tree)
					(cons (entry tree)
						  (copy-to-list (right-child tree)
										result-list)))))
  (copy-to-list tree '()))
;;; list->tree 将一个有序表变换为一棵平衡二叉树。其中的辅助函数 partial-tree
;;; 以整数 n 和一个至少包含 n 个元素的表为参数，构造出一棵包含这个表的前 n
;;; 个元素的平衡树。由 partial-tree 返回的结果是一个序对（由 cons 构造），其
;;; car 是构造出的树，其 cdr 是没有包含在树中那些元素的表。 (ex2.64)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
	  (let ((left-result (partial-tree elts left-size)))
		(let ((left-tree (car left-result))
			  (non-left-elts (cdr left-result))
			  (right-size (- n (+ left-size 1))))
		  (let ((this-entry (car non-left-elts))
				(right-result (partial-tree (cdr non-left-elts)
											right-size)))
			(let ((right-tree (car right-result))
				  (remaining-elts (cdr right-result)))
			  (cons (make-tree this-entry left-tree right-tree)
					remaining-elts))))))))
;;; 利用 ex2.63 和 ex2.64 的结果，给出对采用（平衡）二叉树方式实现的集合的
;;; union-tree-set 和 intersection-tree-set 操作的 O(n) 实现。 (ex2.65)
(define (union-tree-set tree-set1 tree-set2)
  (list->tree (union-ordered-set (tree->list-2 tree-set1)
								 (tree->list-2 tree-set2))))
(define (intersection-tree-set tree-set1 tree-set2)
  (list->tree (intersection-ordered-set (tree->list-2 tree-set1)
										(tree->list-2 tree-set2))))

;;; 2.3.4 实例：Huffman 编码树
;;;
;;; Huffman 树的表示
;;; 叶结点！将一棵树的树叶表示为包含符号 leaf、叶中
;;; 符号和权重的表
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
;;; 内部结点（树）！归并两个结点做出一棵树时，树
;;; 的权重也就是这两个结点的权重之和，其符号集就是
;;; 两个结点的符号集的并集。
(define (make-code-tree left right)
  (list left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
	(list (symbol-leaf tree))
	(caddr tree)))
(define (weight tree)
  (if (leaf? tree)
	(weight-leaf tree)
	(cadddr tree)))
;;; 解码过程，以一个 0/1 表和一棵 Huffman 树为参考
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
	(if (null? bits)
	  '()
	  (let ((next-branch
			  (choose-branch (car bits) current-branch)))
		(if (leaf? next-branch)
		  (cons (symbol-leaf next-branch)
				(decode-1 (cdr bits) tree))
		  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
;;; 带权重元素的集合，将树叶和树的集合表示为一批元素的表，按照权重的
;;; 上升顺序排列表中的元素。
(define (adjoin-weight-set x set)
  (cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set)
					(adjoin-weight-set x (cdr set))))))
(define (get-min-elements set)
  (car set))
;;; 此过程以一个符号-权重对偶的表为参数，如 ((A 4) (B 2) (C 1) (D
;;; 1))，它构造出的树叶的初始排序集合，以便 Huffman 算法能够去做归并
(define (make-leaf-set pairs)
  (if (null? pairs)
	'()
	(let ((pair (car pairs)))
	  (adjoin-weight-set (make-leaf (car pair)
									(cadr pair))
						 (make-leaf-set (cdr pairs))))))
;;; 定义一棵编码树和一个样例bits串，然后用过程 decode 完成该bits串的解码 (ex2.67)
;;;     {A,B,C,D} 8
;;;     /        \
;;;	 0 {A} 4	1 {B,C,D} 4		
;;;			   /	\
;;;		 10{B} 2   11 {C,D} 2
;;;				   /	   \
;;;           110 {D} 1  111 {C} 1
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
				  (make-code-tree (make-leaf 'B 2)
								  (make-code-tree (make-leaf 'D 1)
												  (make-leaf 'C 1)))))
(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(print "----------------- TEST decode --------------------")
(define decoded-message (decode sample-bits sample-tree))
(print decoded-message)
(print "----------------- END  decode --------------------")
;;; 过程 encode 以一个消息和一棵树为参数，产生出被编码消息所对应的二进制位的表。
;;; encode-symbol 在遇到未出现在树中的符号时应报告错误。 (ex2.68)
(define (encode message tree)
  (if (null? message)
	'()
	(append (encode-symbol (car message) tree)
			(encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (if (element-of-set? symbol (symbols tree))
	(if (leaf? tree)
	  (if (eq? symbol (symbol-leaf tree))		; 若是叶结点，检查 symbol 是否与叶结点 symbol 相同，若相同，则找到了，返回 '()
		'()
		(error "symbol not in the leaf!" symbol))	; 若不相同，则报告 error
	  (if (element-of-set? symbol (symbols (left-branch tree)))			; 若是内部结点，先检查是否在此子树内，有则检查左子树和右子树，并用 0（左） 或 1（右）标明 bits，否则报告 error
		(cons 0 (encode-symbol symbol (left-branch tree)))
		(cons 1 (encode-symbol symbol (right-branch tree)))))
	(error "symbol not in the branch!" symbol)))

;;; 下面过程以一个符号-频度（权重）对偶表为参数（其中没有任何符号出现在多于一个对偶中），并根据
;;; Huffman 算法生成出 Huffman 编码树。其中 make-leaf-set
;;; 是前面给出的过程，它将对偶表变换为叶的有序集， successive-merge
;;; 是需要你写的过程，它使用 make-code-tree
;;; 反复归并集合中具有最小权重的元素直至集合里只剩下一个元素为止。这个元素就是我们所需要的
;;; Huffman 树。 (ex2.69)
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
	(get-min-elements leaf-set)					; 若 leaf-set 中只剩一个元素，这个元素就是我们所需要的 Huffman 树。
	(successive-merge (adjoin-weight-set (make-code-tree (get-min-elements leaf-set)	; 否则取出最小的两个元素合并，然后放回 leaf-set 中，再递归进行 successive-merge，由于每次 leaf-set 的势减一，所以终会结束。
														 (get-min-elements (cdr leaf-set)))
					   (cddr leaf-set)))))
;;; 下面带有相对频度的 8 个符号的字母表，是为了有效编码 20 世纪 50 年代的摇滚歌曲中的词语而设计的。
;;; A		2
;;; NA		16
;;; BOOM	1
;;; SHA		3
;;; GET		2
;;; YIP		9
;;; JOB		2
;;; WAH		1
;;; 用 generate-huffman-tree 过程生成对应的 Huffman 树，用 encode
;;; 过程编码下面的消息：
;;; get a job
;;; sha na na na na na na na na
;;; get a job
;;; sha na na na na na na na na
;;; wah yip yip yip yip yip yip yip yip
;;; sha boom
;;; 这一编码需要多少个二进制位？如果对这 8
;;; 个符号的字母表采用定长编码，完成这个歌曲的编码最少需要多少个二进制位？
(define my-pairs 
  '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))
(define my-huffman-tree
  (generate-huffman-tree my-pairs))
(print my-huffman-tree)
(define m1
  '(get a job))
(define m2
  '(sha na na na na na na na na))
(define m3
  '(get a job))
(define m4
  '(sha na na na na na na na na))
(define m5
  '(wah yip yip yip yip yip yip yip yip)) 
(define m6
  '(sha boom))
(print (length (encode m1 my-huffman-tree)))
(print (length (encode m2 my-huffman-tree)))
(print (length (encode m3 my-huffman-tree)))
(print (length (encode m4 my-huffman-tree)))
(print (length (encode m5 my-huffman-tree)))
(print (length (encode m6 my-huffman-tree)))


;;;;;;;;;;;;;;;;;;;
;;; 2.4 抽象数据的多重表示
;;;;;;;;;;;;;;;;;;;


;;; 2.4.1 复数的表示
;;;
;;; 复数的表示可以用直角坐标形式（实部和虚部）以及极坐标（模和幅角）分别表示。
;;; 实部（z1 + z2） = 实部（z1） + 实部（z2）
;;; 虚部（z1 + z2） = 虚部（z1） + 虚部（z2）
;;; 
;;; 模(z1*z2） = 模（z1） * 模（z2）
;;; 幅角（z1*z2） = 幅角（z1） + 幅角（z2）
;;;
;;; 复数的加减乘除
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
					   (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
					   (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
					 (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					 (- (angle z1) (angle z2))))

;;; 标签相关
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
	(car datum)
	(error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
	(cdr datum)
	(error "Bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;;; 选择函数和构造函数，直角坐标表示
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z)) 
		   (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) 
		(real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y) 
  (attach-tag 'rectangular
			  (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
			  (cons (* r (cos a)) (* r (sin a)))))
;;; 选择函数和构造函数，极坐标表示
(define (real-part-polar z)
  (* (magnitude-polar z)
	 (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z)
	 (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
			  (cons (sqrt (+ (square x)
							 (sqaure y)))
					(atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
			  (cons r a)))
;;; 选择函数和构造函数，首先检查参数的标志，然后调用处理该类数据的适当过程。
(define (real-part z)
  (cond ((rectangular? z)
		 (real-part-rectangular (contents z)))
		((polar? z)
		 (real-part-polar (contents z)))
		(else
		  (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
		 (imag-part-rectangular (contents z)))
		((polar? z)
		 (imag-part-polar (contents z)))
		(else
		  (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
		 (magnitude-rectangular (contents z)))
		((polar? z)
		 (magnitude-polar (contents z)))
		(else
		  (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
		 (angle-rectangular (contents z)))
		((polar? z)
		 (angle-polar (contents z)))
		(else
		  (error "Unknown type -- ANGLE" z))))
;;; 构造复数，有实部和虚部则采用直角坐标表示，有模和幅角则采用极坐标表示。
(define (make-from-real-imag x y)
  (make-from-mag-ang-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;; 数据导向的程序设计和可加性
;;;
