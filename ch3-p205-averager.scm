(load "ch3-p203-connector.scm")
(load "ch3-p200-constraint.scm")

;;; 利用基本的加法、乘法和常量约束定义一个 averager 过程，它以三个连接 a、b 和 c 作为输入，
;;; 建立起一个约束，使得 c 总是 a 和 b 的平均值。
;;; a + b = c * 2
;;; ----a---- a1                            m1 ----v---- 2
;;;                 +   s ----u---- p   *
;;; ----b---- a2                            m2 ----c----
(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "A: " A)
(probe "B: " B)
(probe "C: " C)
