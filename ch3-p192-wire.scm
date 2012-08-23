(load "ch3-p193-agenda.scm")

;;; 线路的表示
;;;
;;; 一条线路就是一个具有两个局部状态变量的计算对象: 其中一个是信号值 signal-value
;;; (初始值取 0), 另一个是一组过程 action-procedures, 在看信号值改变时, 这些过程
;;; 都需要运行
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
           (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;;; 待处理表,包含一个需要完成的事项的清单 (稍后定义)
;;; (make-agenda), 返回一个新的空的待处理表
;;; (empty-agenda? <agenda>), 在所给待处理表空时为真
;;; (first-agenda-item <agenda>), 返回待处理表第一项
;;; (remove-first-agenda-item! <agenda>), 删除待处理表第一项
;;; (add-to-agenda! <time> <action> <agenda>), 加入一项, 
;;; 要求在特定时间运行给定的动作过程
;;; (current-time <agenda>), 返回当前的模拟时间
;;;
;;; the-agenda 是我们要用的特定待处理表, 过程 after-delay 向
;;; the-agenda 加入一个新元素
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;;; 有关的模拟用 propagate 驱动, 它对 the-agenda 操作, 顺序执行这一待处理表
;;; 中的每个过程. 一般而言, 在模拟运行中, 一些新项目将被加入待处理表,
;;; 只要在这一待处理表里还有项目, 过程 propagate 就会继续模拟下去
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))
