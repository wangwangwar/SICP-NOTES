#!/usr/bin/env guile
!#
(load "ch3-p190-adder.scm")
(load "ch3-p192-wire.scm")
(load "basic.scm")

(print "------------ TEST half-adder -------------------")
(define a (make-wire))
(define b (make-wire))
(define s (make-wire))
(define c (make-wire))
(half-adder a b s c)
(set-signal! a 1)
(set-signal! b 1)
(print "------------ END  half-adder -------------------")
