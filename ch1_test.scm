;;; 1.1.7 实例：采用牛顿法求平方根
(display (sqrt 2.0))
(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2 过程与它们所产生的计算
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 1.2.1 线性的递归和迭代
;;;
(display (factorial 9))
(newline)
(display (factorial2  10))
(newline)

;;; 1.2.2 树型递归
;;;
(display (count-change 100))
(newline)
(display (count-change 292))
(newline)

;;; 1.2.3 增长的阶
;;;
(display (sine 12.15))
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 用高阶函数做抽象
;;;;;;;;;;;;;;;;;;;;;;;;


;;; 1.3.1 过程作为参数
;;;
(display (integral cube 0 1 0.01))
(newline)
(display (integral cube 0 1 0.001))
(newline)
