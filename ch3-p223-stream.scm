;;; 3.5 流
;;; 3.5.1 流作为延时的表
;;;
;(load "ch3-p225-delay-force.scm")
(load "basic.scm")

;;; 定义关于 stream 的各种操作
(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)

;;; 类似于第 2 章的各种表操作(list-ref, map, for-each)
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map-beta proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map-beta proc (stream-cdr s)))))

;;; 类似 2.2.3 的脚注 78。 (ex3.50)
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred
                             (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high)))) 

;;;
(define (display-stream s)
  (stream-for-each print s))
