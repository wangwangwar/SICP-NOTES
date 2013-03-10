(load "basic.scm")
(load "ch4-p255-expression.scm")
;;; 除了需要定义表达式的外部语法形式之外，求值器的实现
;;; 还必须定义好在其内部实际操作的数据结构，作为程序
;;; 执行的一部分。

;;; *谓词检测*
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;; *过程的表示*
;;; 基本过程的处理，见 4.1.4
;;; (apply-primitive-procedure <proc> <args>)
;;; 它能够将给定的过程应用于表 <args> 里的参数值，并返回
;;; 这一应用的结果。
;;; (primitive-procedure? <proc>)
;;; 检查 <proc> 是否为一个基本过程。

;;; 复合过程是由形式参数、过程体和环境，通过构造函数
;;; make-procedure 做出来的。
(define (make-procedure paramters body env)
  (list 'procedure paramters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;; *对环境的操作*
;;; 求值器需要一些对环境的操作。一个环境就是一个框架的
;;; 序列，每个框架都是一个约束的表格，其中的约束关联起
;;; 一些变量和与之对应的值。
