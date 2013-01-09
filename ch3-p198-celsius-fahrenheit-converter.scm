(load "ch3-p203-connector.scm")
(load "ch3-p200-constraint.scm")
;;; 3.3.5 约束的传播
;;;
;;; 约束系统的使用
;;; 华氏温度和摄氏温度之间存在一种关系:
;;; 9C = 5(F-32)
;;; 这样的约束可以看作是一个网络，通过基本加法约束、乘法约束和
;;; 常量约束组成。

;;; 定义华氏--摄氏温度网络
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;;; 连接器 C 和 F
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

;;; 为连接器 C 和 F 装上 probe 过程，与 3.4.4 监视线路的过程类似。
;;; 每次给这个连接器一个值时，就会打印出一个消息
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
