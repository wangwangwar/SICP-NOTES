(load "ch3-p200-constraint.scm")
(load "ch3-p203-connector.scm")

;;; c+, c*, c/, cv 是算术运算的“约束”版。
;;; c+ 以两个连接器为参数，返回另一个连接器，它与那两个连接器具有加法约束
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv v)
  (let ((z (make-connector)))
    (constant v z)
    z))
