#!/usr/bin/guile -s
!#
(define (identity x)
	x)

(define (square x)
	(* x x))

(define (cube x)
  (* x x x))

(define (average x y)
  (/ (+ x y) 2.0))

(define (inc x)
  (+ x 1))

(define (double x)
  (* x 2.0))

(define (halve x)
  (/ x 2.0))
     
(define (print x)
  (display x)
  (newline))

(define (show x)
  (print x)
  x)
