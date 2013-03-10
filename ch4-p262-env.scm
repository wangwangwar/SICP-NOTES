;;; 我们将环境表示为一个框架的表，一个环境的外围环境就是
;;; 这个表的 cdr，空环境则直接用空表表示。
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;; 在环境里的每个框架都是一个表形成的序对：一个是这一
;;; 框架中的许多变量的表，还有就是它们的约束值的表。
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;; 为了能够用一个（关联了一些变量和值的）新框架去扩充一个环境，我们让框架
;;; 由一个变量的表和一个值的表组成，并将它结合到环境上。如果变量的个数和值
;;; 的个数不匹配，我们就发出一个错误信号。
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

;;; 要在一个环境中查找一个变量，就需要扫描第一个框架里的变量表。如果在这里
;;; 找到了所需的变量，那么就返回与之对应的值表里的对应元素。如果我们不能在
;;; 当前框架里找到这个变量，那么就到其外围环境里去查找，并如此继续下去。如
;;; 果遇到了空环境，那么就发出一个“未约束变量”的错误信号。
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else
              (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;;; 在需要为某个变量在给定环境里设置一个新值时，我们也要扫描这个变量，就像
;;; 在过程 look-variable-value 里一样。在找到这一变量后修改它的值。
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
              (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;;; 为了定义一个变量，我们需要在*第一个*框架里查找该变量的约束，如果找到就修
;;; 改其约束（就像是在 set-variable-value! 里一样。）如果不存在这种约束，
;;; 那么就在*第一个*框架中加入这个约束。
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
              (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
