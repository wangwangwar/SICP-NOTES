(load "ch3-p192-wire.scm")
(load "ch3-p191-and-or-inverter.scm")


(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;;; 半加器 
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;;; 全加器
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;;; 用 n 个全加器串联组成一个*级联进位加法器*. (ex3.30)
;;; 输入 A1, A2, A3, ..., An 与 B1, B2, B3, ..., Bn 是需要求和的两个二进制数, 
;;; 输出和 S1, S2, S3, ..., Sn, 以及最终进位值 C.
;;; 其中 A1 是 most-significant bit, An 是 least-significant bit.
;;; 写出一个过程 ripple-carry-adder 生成这种电路
;;; 这个过程共有 4 个输入, Ak, Bk, Sk, C,
;;; 其中 Ak 是包含 A1, A2, A3, ..., An 线路的一个表, 同理 Bk, Sk 也是,
;;; C 是最终进位线路
(define (ripple-carry-adder list-A list-B list-S C) 
  (let ((c-in (make-wire))) 
    (if (null? (cdr list-A)) 
      (set-signal! c-in 0) 
      (ripple-carry-adder (cdr list-A) (cdr list-B) (cdr list-S) c-in)) 
    (full-adder (car list-A) (car list-B) c-in (car list-S) C)))
