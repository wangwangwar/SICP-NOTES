;;; 3.3.3 表格的表示
;;;
;;; 从表格里提取信息,用 lookup 过程,它以一个关键码为参数,返回与之相
;;; 关联的值（如果在这个关键码之下没有值就返回假）。lookup 基于 assoc
;;; 操作定义的,这一操作要求一个关键码和一个记录的表作为参数。
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
;;; 在一个表格里某个特定的关键码之下插入一个值,首先用 assoc 查看该表格
;;; 里是否已经有以此作为关键码的记录。如果没有就 cons 起这个关键码和相应
;;; 的值,构造出一个新纪录,插入到记录表的最前面。如果已经有了具有该关键
;;; 码的记录,就将该记录的 cdr 设置为新值。
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)
;;; 构造一个新表格时,只需要创建起一个包含符号 *table* 的表
(define (make-table)
  (list '*table*))

;;; 二维表格
;;; 二维表格里每个值由两个关键码索引。我们将这种表格构造成一个一位表格,
;;; 其中的每个关键码又标识了一个子表格。
;;; 子表格并不需要特殊的头单元符号,因为标识子表格的关键码就能起到这一
;;; 作用。
;;;
;;; 在需要查找一个数据项时,我们先用第一个关键码确定对应的子表格,而后
;;; 用第二个关键码在这个子表格里确定记录。
(define (lookup-2d key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))
;;; 如果需要将一个新数据项插入到一对关键码下,我们首先用 assoc 去查看在
;;; 第一个关键码下是否存在一个子表格。如果没有,那么就构造起一个新的
;;; 子表格,其中只包含一个记录 (key-2, value),并插入到表格的第一个关键码
;;; 之下。如果表格里已经有了对应于第一个关键码的子表格,那么就将新值插入
;;; 该子表格,用的就是在一维表格中插入的方法。
(define (insert!-2d key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key-1
                            (cons key-2 value))
                      (cdr table)))))
  'ok)

;;; 创建局部表格。
;;; 用过程的方式表示表格,将表格表示为一个以局部状态的方式维持着一个
;;; 内部表格的对象。在接到一个适当的消息时,这种“表格对象“将提供相应
;;; 的过程,实现对内部表格的各种操作。
(define (make-table-2d)
  (let ((local-table (list '*table*)))
    (define (lookup-2d key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert-2d! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-2d-proc) lookup-2d)
            ((eq? m 'insert-2d-proc!) insert-2d!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table-2d))
(define get (operation-table 'lookup-2d-proc))
(define put (operation-table 'insert-2d-proc!))

;;; 在上面的表格实现中,对于关键码的检查用 equal? 比较是否相等(它被
;;; assoc 调用). 这一检查方式并不总是合适的。如,我们可能需要一个采用
;;; 数值关键码的表格,对于这种表格,我们需要的不是找到对应数值的准确
;;; 匹配,而可以是有一点容许误差的数值。设计一个表格构造函数 make-table,
;;; 它以一个 same-key? 过程作为参数,用这个过程检查关键码的“相等”与否。
;;;
;;; 这里 same-key? 用 ex2.54 定义的 equal? 稍作修改,将 number 相等由
;;; 绝对等于改为约等于,精确到小数点后两位
;;; (ex3.24)
(define (make-table-2d-new same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup-2d key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert-2d! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-2d-proc) lookup-2d)
            ((eq? m 'insert-2d-proc!) insert-2d!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
;;; 容许数字误差的 same-key?
(define (same-key? item-1 item-2)
  (cond ((and (symbol? item-1) (symbol? item-2))
         (eq? item-1 item-2))
        ((and (number? item-1) (number? item-2))
         (if (< (abs (- item-1 item-2)) 0.01)
           #t
           #f))
        ((and (null? item-1) (null? item-2))
         #t)
        ((and (pair? item-1) (pair? item-2))
         (and (same-key? (car item-1) (car item-2))
              (same-key? (cdr item-1) (cdr item-2))))
        (else
          #f)))
(define operation-table-new (make-table-2d-new same-key?))
(define get-new (operation-table-new 'lookup-2d-proc))
(define put-new (operation-table-new 'insert-2d-proc!))

;;; 推广一维表格和二维表格的概念,说明如何实现一种表格,其中的值可以保存
;;; 在任意个关键码之下,不同的值可能对应于不同数目的关键码。对应的 lookup
;;; 和 insert! 过程以一个关键码的表作为参数去访问这一表格。 (ex3.25)
;;;
