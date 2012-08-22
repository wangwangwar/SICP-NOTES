(load "basic.scm")
(load "ch2.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.1 赋值和局部状态
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 3.1.1 局部状态变量
;;;
;;; 支取账户
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
	(begin (set! balance (- balance amount))
		   balance)
	"Insufficient funds"))

;;; 重写 withdraw 过程， balance 作为内部变量
(define new-withdraw
  (let ((balance 100))
	(lambda (amount)
	  (if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))))

;;; 创建"提款处理器"
(define (make-withdraw balance)
  (lambda (amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "Insufficient funds")))

;;; 添加存款功能，并能够选择提款或存款功能
(define (make-account balance)
  (define (withdraw amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch m)
	(cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT"
					   m))))
  dispatch)

;;; 一个累加器是一个过程，反复用数值参数调用它，就会使它的各个参数累加到一个
;;; 和数中。每次调用时累加器将返回当前的累加和。 (ex3.1)
(define (make-accumulator initial)
  (lambda (addend)
	(set! initial (+ initial addend))
	initial))

;;; make-monitored 过程，以一个过程 f 作为输入，该过程本身有一个输入。
;;; make-monitored 返回的结果是第三个过程，比如 mf，它将用一个内部计数器维持着
;;; 自己被调用的次数。如果 mf 的输入是特殊符号 how-many-calls?，那么 mf 就返回
;;; 内部计数器的值；如果输入是特殊符号 reset-count，那么 mf 就将计数器重新设置
;;; 为 0，对于任何其他输入，mf 将返回过程 f 应用于这一输入的结果，并将内部计数器
;;; 加一。 (ex3.2)
(define (make-monitored func)
  (let ((counter 0))
    (lambda (e)
      (define (dispatch m)
		(cond ((eq? m 'how-many-calls?) counter)
			  ((eq? m 'reset-count) (set! counter 0) counter)
			  (else (begin (set! counter (+ counter 1))
						   (func m)))))
	  (dispatch e))))

;;; 修改 make-account 过程，使它能创建一种带密码保护的账户。 (ex3.3)
(define (make-account-pw balance secret-password)
  (define (withdraw amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define show-incorrect-password
	(lambda (e)
	  "Incorrect password"))
  (define (dispatch my-password m)
	(cond ((not (eq? secret-password my-password)) show-incorrect-password)
	      ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT-PW" m))))
  dispatch)

;;; 修改 ex3.3 中的 make-account-pw，加上一个局部状态变量，使得如果一个账户被用
;;; 不正确的密码连续访问了 7 次，它将去调用 call-the-cops。 (ex3.4)
(define (make-account-pw-7 balance secret-password)
(let ((counter 7))
  (define (withdraw amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define check-password
	(lambda (my-password)
	  (if (eq? secret-password my-password)
		(begin (set! counter 7)
			   #t)
		(begin (set! counter (- counter 1))
			   (if (= counter 0) (call-the-cops))
			   #f))))
  (define (call-the-cops)
	(print "Call the cops"))
  (define show-incorrect-password
	(lambda (e)
	  "Incorrect password"))
  (define (dispatch my-password m)
	(cond ((not (check-password my-password)) show-incorrect-password)
	      ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT-PW-7" m))))
  dispatch))

;;; 3.1.2 引进赋值带来的利益
;;;
;;; 随机数生成，用线性同余方法（LCG），N(j+1) = (A * N(j) + C) mod M
(define rand-update
  (let ((a 27)
		(b 26)
		(m 127))
	(lambda (x)
	  (remainder (+ (* a x) b) m))))

(define rand
  (let ((x 3))
	(lambda ()
	  (set! x (rand-update x))
	  x)))

;;; 蒙特卡罗模拟，随机数实现
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials))
		  ((experiment)
		   (iter (- trials-remaining 1) (+ trials-passed 1)))
		  (else
			(iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;; 蒙特卡罗积分，通过蒙特卡罗模拟估计定积分值 (ex3.5)
(define (rect-area x1 x2 y1 y2)
  (abs (* (- x2 x1) (- y2 y1))))
(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (random range))))
(define (unit-pred x y)
  (<= (+ (square x) (square y)) 1))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((integral-test
		  (lambda ()
			(P (random-in-range x1 x2)
			   (random-in-range y1 y2)))))
	(* (rect-area x1 x2 y1 y2) (monte-carlo trials integral-test))))

;;; 用 estimate-integral 估算 pi
(define estimate-integral-pi
  (estimate-integral unit-pred 1.0 -1.0 1.0 -1.0 100000))

;;; (rand 'generate) 产生一个新随机数
;;; ((rand 'reset) <new-value>) 将内部状态变量设置为 <new-value> (ex3.6)
(define rand-ex
  (let ((x 3))
	(define generate
	  (lambda ()
		(set! x (rand-update x))
	    x))
	(define reset
	  (lambda (new-value)
		(set! x new-value)))
	(define dispatch
	  (lambda (m)
	    (cond ((eq? m 'generate) (generate))
			  ((eq? m 'reset) reset)
			  (else (error "Unknown request -- RAND-EX"
						   m)))))
	dispatch))

;;; make-joint 由三个参数：1. 有密码保护的账户，2. 原密码， 3. 新密码，可以用
;;; 新密码对账户进行访问。
;;; 如 peter-acc 是一个具有密码 open-sesame 的银行账户，那么
;;; (define paul-acc
;;;		(make-joint peter-acc 'open-sesame 'rosebud))
;;;	将使我们可以通过名字 paul-ac 和密码 rosebud 对账户 peter-acc 做现金交易。
;;;	(ex3.7)
(define make-joint
  (lambda (acc acc-pass new-pass)
	(define (proxy-dispatch password m)
	  (if (eq? password new-pass)
		(acc acc-pass m)
		(error "Wrong joint password -- MAKE-JOINT" password)))
	proxy-dispatch))

;;; 当我们引入赋值之后，对一个过程的各个参数的求值顺序不同就可能导致不同的结果。
;;; 定义一个简单的过程，使得对于 (+ (f 0) (f 1)) 的求值在对实际参数采用从左到右
;;; 的求值顺序式返回 0, 而对实际参数采用从右到左的求值顺序时返回 1。 (ex3.8)
(define f
  (lambda (x)
	(let ((xx 0)
		  (already-run? #f))
	  (if already-run?
		0
		(begin (set! xx x)
			   (set! already-run? #t)
			   xx)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3.3 用变动数据做模拟
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 3.3.1 变动的表结构
;;;
;;; cons 通过创建新序对的方式构造新的表，可以用两个改变函数和一个过程
;;; get-new-pair 实现。
;(define (cons x y)
;  (let ((new (get-new-pair)))
;	(set-car! new x)
;	(set-cdr! new y)
;	new))

;;; 过程 append!，与 append 类似，但它是一个改变函数而不是一个构造函数。
;;; 它将表拼接起来的方式是将两个表粘起来，修改 x 的最后一个序对，使它的
;;; cdr 现在变成 y （对空的 x 调用 append! 将是一个错误）。 (ex3.12)
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
	x
	(last-pair (cdr x))))

;;; 过程 make-cycle，构造循环（链表）。 (ex3.13)
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;; 过程 mystery，(ex3.14)
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

;;; 写一个过程检查一个表，确定其中是否包含环，即，如果某个程序打算通过不断做 cdr 去找到这个表的结尾，是否会陷入无穷循环。 (ex3.18)
(define (check-list-ring pairs)
  (define (check-list-ring-iter pairs memo-list)
    (if (not (pair? pairs))
	  #f
	  (if (memq pairs memo-list)
		#t
		(check-list-ring-iter (cdr pairs) (cons pairs memo-list)))))
  (check-list-ring-iter pairs '()))

;;; 重写上面的算法，只需要常量空间 (ex3.19)
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
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
	(cond ((eq? m 'car) x)
		  ((eq? m 'cdr) y)
		  ((eq? m 'set-car!) set-x!)
		  ((eq? m 'set-cdr!) set-y!)
		  (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)
