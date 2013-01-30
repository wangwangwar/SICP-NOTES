;;; 在许多应用中，我们都需要多次迫使同一个延时对象求值，解决这个问题的办法就是设法采用
;;; 一种构造延时对象的方法，使它们在第一次被迫求值之后能保存起求出的值。随后再次遇到被迫
;;; 求值时，这些对象就可以直接返回自己保存的值，而不必重复进行计算。
(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? #t)
               result)
        result))))

;;; 将表达式保存起来
(define (delay expr)
  (memo-proc (lambda () expr)))

;;; 对表达式求值
(define (force delayed-object)
  (delayed-object))
