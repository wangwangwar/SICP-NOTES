#!/usr/bin/env guile
!#
(load "ch3-p205-squarer.scm")

(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "A: " A)
(probe "B: " B)

(set-value! A 2 'user)
(forget-value! A 'user)
(set-value! B 3 'user)
(forget-value! B 'user)
(set-value! A 3 'user)
