#!/usr/bin/env guile
!#
(load "ch3-p205-averager.scm")
(load "basic.scm")

(print "------ TEST averager ---------------------------")
(set-value! A 25 'user)
(set-value! B 15 'user)
(forget-value! A 'user)
(forget-value! B 'user)
(set-value! C 15 'user)
(set-value! A 335 'user)
(print "------ END  averager ---------------------------")
