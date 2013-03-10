;;; 3.5.2 无穷流
;;;
;;; 前面我们已经看到如何做出一种假象，使我们可以像对待
;;; 完整的实体一样去对流进行各种操作，即使在实际上只
;;; 计算出了有关的流中必须访问的那一部分。我们可以利用
;;; 这种技术有效地将序列表示为流，即使对应的序列非常
;;; 长。我们甚至可以用流去表示无穷长的序列。
;;;
(load "ch3-p223-stream.scm")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

;(define no-sevens
;  (stream-filter (lambda (x) (not (divisible? x 7)))
;                 integers))
