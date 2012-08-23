(load "ch3-p190-adder.scm")
(load "basic.scm")
;;; 一个简单的实例模拟
;;;
;;; 下面过程将一个 "监测器" 放到一个线路上, 用于显示模拟器的活动, 这一过程
;;; 告诉相应的线路, 只要它的值改变了, 就应该打印新的值和当前时间及线路名字
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display (current-time the-agenda))
                 (display "\t")
                 (display name)
                 (display "\t ")
                 (display (get-signal wire))
                 (newline))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(print "time\t wire\t value")
(probe 'input-1 input-1)
(probe 'input-2 input-2)
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
