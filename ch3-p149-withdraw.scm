#!/usr/bin/env guile
!#
; vim: set shiftwidth=2:
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

;;; 重写 withdraw 过程, balance 作为内部变量
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

;;; 添加存款功能,并能够选择提款或存款功能
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

;;; 一个累加器是一个过程,反复用数值参数调用它,就会使它的各个参数累加到一个
;;; 和数中。每次调用时累加器将返回当前的累加和。 (ex3.1)
(define (make-accumulator initial)
  (lambda (addend)
    (set! initial (+ initial addend))
    initial))

;;; make-monitored 过程,以一个过程 f 作为输入,该过程本身有一个输入。
;;; make-monitored 返回的结果是第三个过程,比如 mf,它将用一个内部计数器维持着
;;; 自己被调用的次数。如果 mf 的输入是特殊符号 how-many-calls?,那么 mf 就返回
;;; 内部计数器的值；如果输入是特殊符号 reset-count,那么 mf 就将计数器重新设置
;;; 为 0,对于任何其他输入,mf 将返回过程 f 应用于这一输入的结果,并将内部计数器
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

;;; 修改 make-account 过程,使它能创建一种带密码保护的账户。 (ex3.3)
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

;;; 修改 ex3.3 中的 make-account-pw,加上一个局部状态变量,使得如果一个账户被用
;;; 不正确的密码连续访问了 7 次,它将去调用 call-the-cops。 (ex3.4)
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
;;; 随机数生成,用线性同余方法（LCG）,N(j+1) = (A * N(j) + C) mod M
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

;;; 蒙特卡罗模拟,随机数实现
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

;;; 蒙特卡罗积分,通过蒙特卡罗模拟估计定积分值 (ex3.5)
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

;;; make-joint 由三个参数：1. 有密码保护的账户,2. 原密码, 3. 新密码,可以用
;;; 新密码对账户进行访问。
;;; 如 peter-acc 是一个具有密码 open-sesame 的银行账户,那么
;;; (define paul-acc
;;;     (make-joint peter-acc 'open-sesame 'rosebud))
;;; 将使我们可以通过名字 paul-ac 和密码 rosebud 对账户 peter-acc 做现金交易。
;;; (ex3.7)
(define make-joint
  (lambda (acc acc-pass new-pass)
    (define (proxy-dispatch password m)
      (if (eq? password new-pass)
        (acc acc-pass m)
        (error "Wrong joint password -- MAKE-JOINT" password)))
    proxy-dispatch))

;;; 当我们引入赋值之后,对一个过程的各个参数的求值顺序不同就可能导致不同的结果。
;;; 定义一个简单的过程,使得对于 (+ (f 0) (f 1)) 的求值在对实际参数采用从左到右
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
