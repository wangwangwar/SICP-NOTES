(load "basic.scm")
;;; 这里是我们的语言的语法规范

;;; 这里的自求值表达式只有数和字符串
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;; 变量用符号表示
(define (variable? exp) (symbol? exp))

;;; 引号表达式的形式是 (quote <text-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;;; quoted? 借助于过程 tagged-list? 定义，它确定一个表的开始
;;; 是不是某个给定符号。
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;;; 赋值的形式是 (set! <var> <value>)。
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;; 定义的形式是 (define <var> <value>) 或者
;;; (define (<var> <parameter1> ... <parameterN>)
;;;   <body>)
;;; 后一形式（标准的过程定义）只是下面形式的一种语法包装：
;;; (define <var>
;;;   (lambda (<parameter1> ... <parameterN>)
;;;     <body>))
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ; formal paramters
                 (cddr exp))))  ; body

;;; lambda 表达式是由符号 lambda 开始的表
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

;;; 我们为 lambda 表达式提供了一个构造函数，用在上面
;;; definition-value 里。
(define (make-lambda paramters body)
  (cons 'lambda (cons paramters body)))

;;; 条件式由 if 开始，有一个谓词部分、一个推论部分和一个
;;; （可缺的）替代部分，如果这一表达式没有替代部分，我们
;;; 就以 false 作为其替代
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp) 
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

;;; 我们也为 if 表达式提供了一个构造函数，它在 cond->if 里
;;; 使用，用于将 cond 表达式变换为 if 表达式
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;; begin 包装起一个表达式序列，在这里提供了对 begin 表达式
;;; 的一组语法操作，以便从 begin 表达式中提取出实际表达式
;;; 序列，还有选择函数返回序列中的第一个表达式和其余表达式
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

;;; 我们还包括了一个构造函数 sequence->exp（用在 cond->if 里），
;;; 他把一个序列变换为一个表达式，如果需要的话就加上 begin
;;; 作为开头
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;; 过程应用就是不属于上述各种表达式类型的任意复合表达式。
;;; 这种表达式的 car 是运算符，其 cdr 是运算对象的表。
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;; 派生表达式
;;; cond 可以实现为一些嵌套的 if 表达式。采用这种方式实现
;;; 对 cond 的求值能简化求值器，因为这样就减少了需要特别
;;; 描述求值过程的特殊形式的数目。
;;; 我们在这里包括了提取 cond 表达式中各个部分的语法过程，
;;; 以及过程 cond->if，它能将 cond 表达式变换为 if 表达式。
;;; 一个分情况分析以 cond 开始，并包含一个谓词-动作子句的
;;; 表。如果一个子句的符号是 else，那么就是一个 else 子句。
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))
