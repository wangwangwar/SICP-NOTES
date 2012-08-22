;;; 1.1.7 实例：采用牛顿法求平方根
;;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (good-enough?2 guess x)
  (< (/ (abs(- (improve guess x) guess)) guess) 0.001))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2 过程与它们所产生的计算
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 1.2.1 线性的递归和迭代
;;;
;;; 阶乘 (递归)
(define (factorial n)
  (if (= n 1)
      n
      (* n (factorial (- n 1)))))

;;; 阶乘 (线性迭代)
(define (factorial2 n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
	product
	(fact-iter (* counter product)
		   (+ counter 1)
		   max-count)))
  (fact-iter 1 1 n))

;;; 加起两个正整数的两种方法 (ex1.9)
;第一个定义：
;	(define (+ a b)
;		(if (= a 0)
;			b
;			(inc (+ (dec a) b))))
;
;将 (+ 4 5) 展开是这样的：
;	(+ 4 5)
;	(inc (+ 3 5))
;	(inc (inc (+ 2 5)))
;	(inc (inc (inc (+ 1 5))))
;	(inc (inc (inc (inc (+ 0 5)))))
;	(inc (inc (inc (inc 5))))
;	(inc (inc (inc 6)))
;	(inc (inc 7))
;	(inc 8)
;	9
;
;第二个定义：
;	(define (+ a b)
;		(if (= a 0)
;			b
;			(+ (dec a) (inc b))))
;
;按这个定义展开（+ 4 5）的话：
;	(+ 4 5)
;	(+ 3 6)
;	(+ 2 7)
;	(+ 1 8)
;	(+ 0 9)
;	9
;
;可以看出第一种定义是递归计算过程，而第二种是迭代计算过程。

;;; Ackermann 函数 (ex1.10)
;	(define (A x y)
;		(cond	((= y 0) 0)
;				((= x 0) (* 2 y))
;				((= y 1) 2)
;				(else (A (- x 1)
;						 (A x (- y 1))))))
;
;将(A 1 10)展开：
;	(A 1 10)
;	(A 0 (A 1 9))
;	(* 2 (A 0 (A 1 8)))
;	(* 2 (* 2 (A 0 (A 1 7))))
;	...
;	...
;	(* 2 (* 2 ... (* 2 (A 0 (A 1 1))) ... ))
;	(* 2 (* 2 ... (* 2 (* 2 2)) ... ))
;	...
;	...
;	1024
;
;将(A 2 4)展开：
;	(A 2 4)
;	(A 1 (A 2 3))
;	(A 1 (A 1 (A 2 2)))
;	(A 1 (A 1 (A 1 (A 2 1))))
;	(A 1 (A 1 (A 1 2)))
;	(A 1 (A 1 (A 0 (A 1 1))))
;	(A 1 (A 1 (A 0 2)))
;	(A 1 (A 1 (* 2 2)))
;	(A 1 (A 1 4))
;	(A 1 (A 0 (A 1 3)))
;	(A 1 (A 0 (A 0 (A 1 2))))
;	(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;	(A 1 (A 0 (A 0 (A 0 2))))
;	(A 1 (A 0 (A 0 (* 2 2))))
;	(A 1 (A 0 (A 0 4)))
;	(A 1 (A 0 (* 2 4)))
;	(A 1 (A 0 8))
;	(A 1 (* 2 8))
;	(A 1 16)

;;; 1.2.2 树型递归
;;;
;;; 斐波那契数 (递归)
(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

;;; 斐波那契数 (迭代)
(define (fib n)
  (define (iter a b count)
	(if (= count 0)
	  b
	  (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

;;; 有 1，5，10，25，50 美分的零钱，将 1 美元换成零钱共有多少种方式 
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

;;; f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) (ex1.11)
;;; 递归
(define (f1 n)
  (cond ((< n 3) n)
		(else (+ (f1 (- n 1))
				 (* 2 (f1 (- n 2)))
				 (* 3 (f1 (- n 3)))))))
;;; 迭代
(define (f2 n)
  (define (f2-iter a b c n)
    (if (= n 2)
	  c
	  (f2-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))
  (if (< n 3)
	  n
	  (f2-iter 0 1 2 n)))
;;; 1.2.3 增长的阶
;;;
;;; 三角恒等式 sinx = 3*sin(x/3) + 4*sin(x/3)*self*self  (ex1.15)
;;; x <= 0.1 enough
(define (sine a)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs a) 0.1))
	a
	(p (sine (/ a 3.0)))))

;;; 1.2.4 求幂
;;;
;;; 递归
(define (expt-r b n)
  (if (= n 0)
		1
		(* b (expt1 b (- n 1)))))

;;; 迭代
(define (expt-i b n)
  (define (expt-iter b counter product)
		(if (= counter 0)
			product
			(expt2-iter b
									(- counter 1)
									(* b product))))
  (expt-iter b n 1))


;;; 连续求平方，递归
(define (fast-expt-r b n)
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt-r b (/ n 2))))
		(else (* b (fast-expt-r b (- n 1))))))

;;; 连续求平方，迭代 (ex1.16)
(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
		((even? counter) (fast-expt-iter (square b) (/ counter 2) product))
		(else (fast-expt-iter (square b) (/ (- counter 1) 2) (* b product)))))

;;; 求乘积
(define (multi a b)
  (if (= b 0)
	0
	(+ a (* a (- b 1)))))

;;; 快速乘积 (递归) (ex1.17)
(define (fast-multi-r a b)
  (cond ((= b 0) 0)
		((even? b) (double (fast-multi-r a (halve b))))
		(else (+ a (fast-multi1 a (- b 1))))))

;;; 快速乘积 (迭代)
(define (fast-multi-i a b)
  (fast-multi-iter a b 0))

(define (fast-multi-iter a counter product)
  (cond ((= counter 0) product)
		((even? counter) (fast-multi-iter (double a) (halve counter) product))
		(else (fast-multi-iter (double a) (halve (- counter 1)) (+ a product)))))


;;; 1.2.5 最大公约数
;;;
;;; 欧几里德算法求最大公约数
(define (gcd a b)
  (if (= b 0)
	a
	(gcd b (remainder a b))))


;;; 1.2.6 实例：素数检测
;;;
;;; 检验素数，根号n的复杂度
;;;
(define (prime? n)
	(if (= n 1)
		#f
		(= (smallest-divisor n) n)))

(define (smallest-divisor n)
		(find-divisor n 2))

;;; 步进改进 (ex1.23)
(define (find-divisor n test-divisor)
	(define (next x)
		(if (= x 2)
		3
		(+ x 2)))
	(cond ((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
	(= (remainder b a) 0))

;;; 检验素数，费马小定理，log n 的复杂度
(define (fast-prime? n times)
  (cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
				((even? exp)
		 		(remainder (square (expmod base (/ exp 2) m))
					m))
		 		(else 
		   		(remainder (* base (expmod base (- exp 1) m))
					  m))))

;;; 显示运行时间的素数检验，从n开始，直到检测出一个素数为止 (ex1.22, ex1.24)
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
 
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
	(report-prime (- (runtime) start-time))
	(timed-prime-test (+ n 2))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;; 费马小定理的变形 Miller-Rabin 检查，不会被欺骗 (ex1.28)
(define (fast-prime-miller-rabin? n times)
  (cond ((= times 0) #t)
		((miller-rabin-test n) (fast-prime-miller-rabin? n (- times 1)))
		(else #f)))

(define (miller-rabin-test n)
  (define (try-it a)
	(= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (if (and (or (= (fast-expt2 base (/ exp 2)) 1)
				 	  (= (fast-expt2 base (/ exp 2)) (- m 1)))
				  (= (remainder (square (expmod2 base (/ exp 2) m))) 1))
		 	#f
			(remainder (square (expmod2 base (/ exp 2) m))
					   m)))
		 (else
		   (remainder (* base (expmod2 base (- exp 1) m)) 
					  m))))
			

;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 用高阶函数做抽象
;;;;;;;;;;;;;;;;;;;;;;;;


;;; 1.3.1 过程作为参数
;;;
;;; 计算从 a 到 b 的各整数之和
(define (sum-integers a b)
	(if (> a b)
		0
		(+ a (sum-integers (+ a 1) b))))

;;; 计算给定范围内的整数的立方之和
(define (sum-cubes a b)
	(if (> a b)
		0
		(+ (cube a) (sum-cubes (+ a 1) b))))

;;; 计算下面的序列之和：
;;; 1 / (1 * 3) + 1 / (5 * 7) + 1 / (9 * 11) + ...
;;; 它将 (非常缓慢地)收敛到 pi / 8
(define (pi-sum a b)
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;;; 定义一个求和过程，递归
(define (sum-r term a next b)
  (if (> a b)
	  0
	  (+ (term a)
		 (sum-r term (next a) next b))))

;;; 上述过程的迭代形式  (ex1.30)
(define (sum-i term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (+ result (term a)))))
  (iter a 0))

;;; 计算从 a 到 b 的各整数之和，用 sum 过程改写
(define (sum-integers a b)
  (sum-r identity a inc b))

;;; 计算给定范围内的整数的立方之和，用 sum 过程改写
(define (sum-cubes a b)
  (sum-r cube a inc b))

;;; pi-sum，用 sum 过程改写
(define (pi-sum a b)
  (sum-r (lambda (x) (/ 1.0 (* x (+ x 2))))
	   a
	   (lambda (x) (+ x 4))
	   b))

;;; 用 pi-sum 过程求 pi 值
(define (pi n)
  (* 8 (pi-sum 1 n)))

;;; 函数 f 在范围 a 和 b 之间的定积分的近似值
(define (integral f a b dx)
  (define (add-dx x)
	(+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
	 dx))

;;; 比上面近似更精确的辛普森规则 (ex1.29)
(define (simpson f a b n)
  (define h
	(* (/ (- b a) n) 1.0))
  (define (add-h x)
	(+ x (* 2 h)))
  (* (+ (f a)
		(* (sum f
				(+ a h) 
				add-h 
				(- b h))
			4)
	 	(* (sum f 
				(+ a (* 2 h))
				add-h 
				(- b (* h 2)))
			2)
		(f b))
	 (/ h 3)))

;;; 类似 sum过程的 product 求积过程，递归 (ex1.31a)
(define (product-r term a next b)
  (if (> a b)
	1
	(* (term a)
	   (product-r term (next a) next b))))

;;; 用 product 定义 factorial 过程 (ex1.31a)
(define (factorial n)
  (product-r identity 2 inc n))

;;; 计算 pi (ex1.31a)
(define (pi prec)
  (* 4 (pi-iter prec)))

(define (pi-iter prec)
  (define (inc2 x)
	(+ x 2))
  (define (identity x)
	x)
  (define (double x)
	(* x 2))
  (define (square x)
	(* x x))
  (* (/ (* 2
		   (square (product2 identity 4 inc2 (double (- prec 1))))
		   (double prec))
		(square (product2 identity 3 inc2 (- (double prec) 1))))
	 1.0))
	 
;;; product 迭代形式 ( ex1.31b )
(define (product-i term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (* result (term a)))))
  (iter a 1))

;;; 比 sum 和 product 更高层抽象的 accumulate 运算，递归 ( ex1.32a )
(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
	null-value
	(combiner (term a)
	   (accumulate-r combiner null-value term (next a) next b))))

;;; 上述过程的迭代形式 (ex1.32b)
(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;; 我们用 accumulate 构造 sum 和 product ( ex1.32a )
(define (sum term a next b)
  (accumulate-i + 0 term a next b))

(define (product term a next b)
  (accumulate-i * 1 term a next b))

;;; 我们引进一个处理被组合项的过滤器 (ex1.33)
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
	null-value
	(if (filter a)
	  (combiner (term a)
				(filtered-accumulate combiner null-value term (next a) next b filter))
	  (combiner null-value
				(filtered-accumulate combiner null-value term (next a) next b filter)))))

;;; 我们用上面的过滤＋累加／累乘器做一点事
;;; 求出在区间 a 到 b 中所有素数之和 (ex1.33a)
(define (prime-sum a b)
  (filtered-accumulate + 0 identity a inc b prime?))

;;; 求出小于 n 的所有与 n 互素的正整数（即所有满足 GCD(i,n) = 1 的整数）之乘积 (ex1.33b)
(define (huzhi-product n)
  (define (huzhi? a)
	(= (gcd a n) 1))
(filtered-accumulate * 1 identity 1 inc n huzhi?))


;;; 1.3.2 用 lambda 构造过程
;;;
;;; 计算函数：f(x, y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y) 三种方式
;;; 1. 构建辅助过程
(define (f x y)
  (define (f-helper a b)
	(+ (* x (square a))
	   (* y b)
	   (* a b)))
  (f-helper (* 1 (* x y))
			(- 1 y)))

;;; 2. 用lambda表达式
(define (f2 x y)
  ((lambda (a b)
	 (+ (* x (square a))
		(* y b)
		(* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;;; 3. 用let
(define (f3 x y)
  (let ((a (+ 1 (* x y)))
		(b (- 1 y)))
	(+ (* x (square a))
	   (* y b)
	   (* a b))))

;;; (ex1.34)
(define (f g)
  (g 2))

(f (lambda (x) (* x x)))

(f (lambda (z) (* z (+ z 1))))


;;; 1.3.3 过程作为一般性的方法
;;;
;;; 区间折半寻找方程的根
(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point)
	  mid-point
	  (let ((test-value (f mid-point)))
		(cond ((positive? test-value)
			   (search f neg-point mid-point))
			  ((negative? test-value)
			   (search f mid-point pos-point))
			  (else mid-point))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.00001))

;;; 加入检查符号的过程
(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else
			(error "Values are not of opposite sign" a b)))))

;;; 找到函数的不动点
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(display guess)
	(newline)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

;;; 我们发现，如果函数 f 是 y = x / y，那么找不动点时
;;; 就会在两个猜测间动荡而不收敛，陷入死循环
(define (sqrt-bad x)
  (fixed-point (lambda (y) (/ x y))
			   1.0))

;;; 我们对猜测值做调整为 (1/2)(y + x/y) 而不是 x/y
;;; 这种技术称为平均阻尼
(define (sqrt-good x)
  (fixed-point (lambda (y) (average y (/ x y)))
			   1.0))

;;; 用不动点来求黄金分割率， x -> 1 + 1 / x (ex1.35)
(define (golden-point)
  (display "Find the golden point:")
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
			   1.0))

;;; 找出 x -> log(1000) / log(x) 的不动点即计算 x^x = 1000 的一个根
;;; 比较采用平均阻尼和不用平均阻尼所用计算步骤 ( ex1.36 )
;;; 不用平均阻尼
(define (ex136-bad)
  (display "find fixed point of x -> log(1000)/log(x) with bad method:")
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
			   2.0))

;;; 采用平均阻尼
(define (ex136-good)
  (display "find fixed point of x -> log(1000)/log(x) with good method:")
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
			   2.0))

;;; 我们根据这种方法构造一个过程，计算 x^y = z 的 y。
(define (log2 x z)
  (fixed-point (lambda (y) (average y (/ (log z) (log x))))
			   2.0))

;;; 我们改写成 average-damp 的形式
(define (log3 x z)
  (fixed-point (average-damp (lambda (y) (/ (log z) (log x))))
			   2.0))

;;; 无穷连分式，递归，n 和 d 是根据下标返回 Ni 华 Di 的过程 (ex1.37a)
(define (cont-frac-r n d k)
  ((lambda (n d k i)
	(let ((i (+ i 1)))
	  (if (> k 1)
		(/ (n i) (+ (d i) (cont-frac-r n d (- k 1))))
		(/ (n i) (d i)))))
	n
	d
	k
	0))

;;; 网友的版本
(define (cont-frac-r2 n d k)
  (define (redu i)
	(if (= i k)
	  (/ (n i) (d i))
	  (/ (n i) (+ (d i) (redu (+ i 1))))))
  (redu 1))

;;; 无穷连分式，迭代 (ex1.37b)
;;; 注意它构造的顺序于递归方法刚好相逆！
(define (cont-frac-i n d k)
  (define (iter i result)
	(if (= i 0)
	  result
	  (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

;;; 1737，欧拉， e - 2 的连分式展开 (ex1.38)
(define euler-e
  (cont-frac-i (lambda (i) 1.0)
			   (lambda (i)
				 (if (= (remainder i 3) 2)
				   (* 2.0 (/ (+ i 1) 3))	; 注意是如何处理
				   1))
			   1000))

(define e
  (+ euler-e 2))

;;; 1770，J.H.Lambert ，tan x 的连分式展开 (ex1.39)
(define (tan-cf x k)
  (cont-frac-i (lambda (i)
				 (if (= i 1)
				   x
				   (- (square x))))
			   (lambda (i)
				 (- (* i 2.0) 1.0))
			   k))

;;; average-damp 是一个过程， f 也是一个过程， 返回值也是一个过程
(define (average-damp f)
  (lambda (x) (average x (f x))))

;;; 立方根是函数 y -> x/y^2 的不动点
;;; 用不动点法推广 sqrt 过程为一个提取立方体的过程
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
			   1.0))

;;; 牛顿法
;;; 如果 x -> g(x) 是一个可微函数，
;;; 那么方程 g(x) = 0 的一个解就是 x -> x - g(x)/Dg(x) 的一个不动点

;;; 求导， Dg(x) = ( g(x+dx) - g(x) )/dx
;;; 以过程为参数，返回一个过程值
(define (deriv g)
  (lambda (x)
	(/ (- (g (+ x dx)) (g x))
	   dx)))

(define dx 0.00001)

;;; 有了 deriv 过程，牛顿法如下
(define (newton-transform g)
  (lambda (x)
	(- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; 我们用牛顿法来定义 sqrt 过程
;;; 由于 g(x) = 0 的一个解就是 x -> f(x)的一个不动点
;;; y = sqrt(x) 可以改写为 g(x) = y^2 - x = 0
(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
				  1.0))

;;; 求平方根的两种方法一种是作为不动点搜索过程，
;;; 一种是牛顿法。而牛顿法也是一种不动点搜索。
;;; 每种方法都是从一个函数出发，找出这个函数在
;;; 某种变换下的不动点。
;;; 由此，我们可以抽象出更高层的过程
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;; sqrt 用不动点搜索法构建
(define (sqrt-fp x)
  (fixed-point-of-transform (lambda (y) (/ x y))
							average-damp
							1.0))

;;; sqrt 用牛顿法构建
(define (sqrt-n x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
							newton-transform
							1.0))

;;; 求 x^3 + a * x^2 + b * x + c 的零点 ( ex1.40 )
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (find-cubic a b c)
  (newtons-method (cubic a b c) 1.0))

;;; 定义一个 double 过程，这一过程将原来那个参数过程应用 2 次。( ex1.41 )
(define (double process)
  (lambda (x) (process (process x))))

;;; 函数复合， compose。( ex1.42 )
(define (compose f g)
  (lambda (x) (f (g x))))

;;; n 次重复求值。( ex1.43 )
(define (repeated f n)
  (if (< n 2)
	f
	(compose f (repeated f (- n 1)))))

;;; n 次平滑函数。( ex1.44 )
(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (n-smooth f n)
  (lambda (x) (((repeated smooth n) f) x)))

;;; 用 fixed-point、average-dump、repeated
;;; 以及 ex1.36 写的 log2 或 log3 过程计算n次方根。( ex1.45 )
(define (n-sqrt x n)
  (fixed-point ((repeated average-damp (log3 2 n)) (lambda (y) (/ x (expt y (- n 1))))) 
			   1.0))

;;; 迭代式改进，iterative-improve，它以两个过程为参数，
;;; 一个表示告知某一猜测是否足够好的方法，
;;; 另一个表示改进猜测的方法。
;;; 它返回一个过程，这个过程以某一猜测为参数，
;;; 通过不断改进，直到得到的猜测足够好为止。( ex1.46 )
(define (iterative-improve good-enough? improve)
  (lambda (guess) 
	(if (good-enough? guess)
	  guess
	  ((iterative-improve good-enough? improve) (improve guess)))))

;;; 用 iterative-improve 过程改写 sqrt 过程。
(define (sqrt2-iter guess x)
  ((iterative-improve (lambda (guess)
					   (< (abs (- (expt guess 2) x)) 0.001))
					 (lambda (guess) 
					   ((average-damp (lambda (guess) (/ x guess))) guess)))
   guess
   ))
(define (sqrt2 x)
  (sqrt2-iter 1.0 x))

;;; 用 iterative-improve 过程改写 fixed-point 过程。
(define (fixed-point2 f guess)
  ((iterative-improve (lambda (guess)
						(< (abs (- guess (f guess))) 0.001))
					  f)
   guess))
