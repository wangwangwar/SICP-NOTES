;;; 连接器用带有局部状态变量 value、 informant 和 constraints 的过程对象表示，
;;; value 中保存这个连接器的当前值， informant 是设置连接器值的对象， constraints
;;; 是这一连接器所涉及的所有约束的表
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value! newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Constradiction" (list value newval)))
            (else
              'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant #f)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value!)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

;;; 为连接器基本操作提供接口
;;; 报告说这一连接器是否有值
(define (has-value? connector)
  (connector 'has-value?))

;;; 返回连接器当前值
(define (get-value connector)
  (connector 'value))

;;; 通知说，信息源(informant)要求连接器将其值设置为一个新值
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

;;; 通知说，撤销源（retractor）要求连接器忘记其值
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

;;; 通知连接器参与一个新约束
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
