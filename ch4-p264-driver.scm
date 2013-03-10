(load "basic.scm")
(load "ch4-p262-env.scm")
(load "ch4-p253-eval.scm")
(load "ch4-p261-eval-ds.scm")

;;; 基本过程对象的具体表示形式并不重要，只要apply能识别它们，并能通过过程
;;; primitive-procedure?和apply-primitive-procedure去应用它们。我们所选择
;;; 的方式，是将基本过程都表示为以符号primitive开头的表，在其中包含着基础
;;; Lisp系统里实现这一基本过程的那个过程。
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;;; 为了应用一个基本过程，我们只需要简单地利用基础Lisp系统，将相应的实现过程
;;; 应用于实际参数：
(define (apply-primitive-procedure proc args)
  (apply
    (primitive-implementation proc) args))

;;; 这里使用了一个特殊的打印过程user-print，以避免打印出复合过程的环境部分，
;;; 因为它可能是一个非常长的表（而且可能包含循环）。
(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;;; 为了能够很方便地运行这个元循环求值器，我们提供了一个驱动循环，它模拟基础
;;; Lisp系统里的读入-求值-打印循环。这个循环打印出一个提示符，读入输入表达式
;;; ，在全局环境里求值这个表达式，而后打印出得到的结果。
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (m-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;;; 每个基本过程名必须有一个约束，以便当eval求值一个应用基本过程的运算符时，
;;; 可以找到相应的对象，并将这个对象传给apply。为此我们必须创建起一个初始
;;; 环境，在其中建立起基本过程的名字与一个唯一对象的关联，在求值表达式的
;;; 过程中可能遇到这些名字。这一全局环境里还要包含符号true和false的约束，
;;; 这就使它们也可以作为变量用在被求值的表达式里。
(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
