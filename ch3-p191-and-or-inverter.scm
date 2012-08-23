(load "ch3-p192-wire.scm")

;;; 基本功能块 inverter, and-gate, or-gate,为上层功能的实现如半加器、全加器提供基础设施
;;; 需要一些更基础的过程, 稍后定义
;;; get-signal, 返回连线上信号的当前值
;;; set-signal!, 将连线上的信号修改为新的值
;;; add-action!, 断言,只要在连线上的信号值改变,这里所指定的过程将运行
;;; after-delay, 它的参数是一个时间延迟和一个过程,它将在给定的时延后执行这一过程
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input)))) 
      (after-delay inverter-delay                       
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)                    
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

;;; 将或门定义为一个基本功能块 (ex3.28)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

;;; 构造或门的另一方式是将它作为一种复合的数字逻辑设备,用与门和反门构造 (ex3.29)
;;; (A | B) = ~(~A & ~B) 
(define (or-gate-2 a1 a2 output)
  (let ((b (make-wire)) (c (make-wire)) (d (make-wire)))
    (inverter (and-gate (inverter a1 b) 
                        (inverter a2 c)
                        d)
                        output)
    'ok))
;;; 或门延迟由与门和反门延迟表示出来:
;;; or-gate-delay = and-gate-delay + inverter-delay * 2

;;; 用 n 个全加器串联组成一个*级联进位加法器*. (ex3.30)
;;; 输入 A1, A2, A3, ..., An 与 B1, B2, B3, ..., Bn 是需要求和的两个二进制数, 
;;; 输出和 S1, S2, S3, ..., Sn, 以及最终进位值 C.
;;; 其中 A1 是 most-significant bit, An 是 least-significant bit.
;;; 写出一个过程 ripple-carry-adder 生成这种电路
;;; 这个过程共有 4 个输入, Ak, Bk, Sk, C,
;;; 其中 Ak 是包含 A1, A2, A3, ..., An 线路的一个表, 同理 Bk, Sk 也是,
;;; C 是最终进位线路
(define (ripple-carry-adder list-A list-B list-S C)
  (define (iter A B S value-of-c)
    (if (and (null? A) (null? B) (null? S))
      'ok
      (let ((a (car A))
            (b (car B))
            (s (car S))
            (remain-A (cdr A))
            (remain-B (cdr B))
            (remain-S (cdr S))
            (c (make-wire)))
        (set-signal! c value-of-c)
        (full-adder a b c s C)
        (iter remain-A remain-B remain-S (get-signal C)))))
  (iter list-A list-B list-S (get-signal C)))
