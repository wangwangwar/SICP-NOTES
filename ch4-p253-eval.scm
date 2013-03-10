(load "ch4-p255-expression.scm")

(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))

;;; 过程参数
;;; eval 在处理过程应用时用 list-of-values 去生成实际参数表，
;;; 以便完成这一过程应用。 list-of-values 以组合式的运算对象
;;; 为参数，求值各个运算对象，返回这些值的表
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (m-eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;;; 我们没有办法说循环求值器是从左到右还是从右到左求值各个运算对象，因为这一
;;; 求值顺序是从作为其基础的Lisp那里继承来的：如果上面的list-of-values里的
;;; cons从左到右求值，那么list-of-values也将从左到右求值；反之亦然。
;;; 请写出一个list-of-values版本，使它总是从左到右求值其运算对象，无论作为其
;;; 基础的Lisp采用什么求值顺序。 (ex4.1)
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
    '()
    (let ((first (eval (first-operand exps) env)))
      (let ((rest (list-of-values-lr (rest-operands exps) env)))
        (cons first rest)))))

;;; 条件
;;; eval-if 在给定环境中求值 if 表达式的谓语部分，如果得到
;;; 的结果为真， eval-if 就去求值这个 if 的推论部分，否则
;;; 它就求值其替代部分
(define (eval-if exp env)
  (if (true? (m-eval (if-predicate exp) env))
    (m-eval (if-consequent exp) env)
    (m-eval (if-alternative exp) env)))

;;; 序列
;;; eval-sequence 用在 apply 里，用于求值过程体里的表达式序列。
;;; 它也用在 eval 里，用于求值 begin 表达式里的表达式序列。
;;; 这个过程以一个表达式序列和一个环境为参数，按照序列里表达式
;;; 出现的顺序对他们求值。它返回最后一个表达式的值。
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;; 赋值和定义
;;; eval-assignment（变量赋值）调用 eval 找出需要赋的值，将变量
;;; 和得到的值传给过程 set-variable-value!，将有关的值安置到指定
;;; 环境里。 eval-definition（变量定义）类似。
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (m-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (m-eval (definition-value exp) env)
                    env)
  'ok)
