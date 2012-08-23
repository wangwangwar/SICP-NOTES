(load "ch3-p180-queue.scm")

;;; 待处理表的实现
;;;
;;; 这种待处理表有一些时间段组成, 每个时间段是由一个数值 (表示时间) 和一个队列
;;; 组成的序对, 在这个队列里, 保存着那些已经安排好的, 应该在这一时间段运行的过程
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

;;; 我们将用队列操作完成在时间段队列上的操作
;;; 待处理表本身就是时间段的一个一维表格, 特别的是这些时间段应按照时间递增的顺序
;;; 排列. 此外, 我们还需在待处理表的头部保存一个*当前时间* (即此前最后被处理的那个
;;; 动作的时间). 
;;; 一个新构造的待处理表没有时间段, 当前时间为 0
(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

;;; 为了将一个动作加入待处理表, 我们首先要检查这个待处理表是否为空， 如果是， 
;;; 那么将创建一个新的时间段， 并将这个时间段装入待处理表里。
;;; 否则我们就扫描整个待处理表， 检查其中各个时间段的时间。
;;; 如果发现某个时间段具有合适的时间， 那么就吧这个动作加入与之关联的队列里。 
;;; 如果碰到了某个比需要预约的时间更晚的时间， 那么就将一个新的时间段插入待处
;;; 理表， 插入这个位置之前。 
;;; 如果到达了待处理表的末尾， 我们就必须在最后加上一个新的时间段
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr! segments
                    (cons (make-new-time-segment time action)
                          (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments! agenda
                     (cons (make-new-time-segment time action)
                           segments))
      (add-to-segments! segments))))

;;; 从待处理表中删除第一项的过程， 应该删去第一个时间段的队列前端那一项。
;;; 如果删除使这个时间段变空了， 我们就将这个时间段也从时间段的有序表里删去
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

;;; 找出待处理表中第一项， 也就是找出第一个时间段队列里的第一项。
;;; 无论何时提取这个项时， 都需要更新待处理表的当前时间。
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
