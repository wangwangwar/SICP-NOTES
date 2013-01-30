(load "ch3-p203-connector.scm")
(load "ch3-p200-constraint.scm")
(load "basic.scm")

;;; 这是牛逼的平方器，也就是一种带有两个引线的约束装置，使得连接在它的第二条
;;; 引线上的连接器 b 的值是其第一条引线上的值 a 的平方。这是正确版本。
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0 -- SQUARER" (get-value b))
        (set-value! a (sqrt (get-value b)) me))
      (if (has-value? a)
        (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- ADDER" request))))
  (connect a me)
  (connect b me)
  me)