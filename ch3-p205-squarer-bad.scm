(load "ch3-p203-connector.scm")
(load "ch3-p200-constraint.scm")

;;; 这是牛逼的平方器，也就是一种带有两个引线的约束装置，使得连接在它的第二条
;;; 引线上的连接器 b 的值是其第一条引线上的值 a 的平方。这是缺陷版本。
;;; 缺陷在哪里呢？
(define (squarer a b)
  (multiplier a a b))
