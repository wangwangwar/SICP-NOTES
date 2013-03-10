(load "ch4-p255-expression.scm")
(load "ch4-p253-eval.scm")
;;; 过程应用的修改版本。为了让求值器在检查大部分表达式之前就识别出过程应用，
;;; 每个个过程应用都以call开始。 (ex4.2 p259)
(define (application-call? exp) (tagged-list? exp 'call))

(define (operator-call exp) (cadr exp))

(define (operands-call exp) (cddr exp))

(define no-operands-call? no-operands?)

(define first-operand-call first-operand)

(define rest-operands-call rest-operands)

(define (m-eval-call exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application-call? exp)
         (m-apply (m-eval-call (operator-call exp) env)
                (list-of-values (operands-call exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval-call (cond->if exp) env))
        (else
          (error "Unknown expression type -- EVAL" exp))))


