#!/usr/bin/env guile
!#
(load "ch3-p205-celsius-fahrenheit-converter-better.scm")
(load "basic.scm")

(print "-------- TEST celsius-fahrenheit-converter-better ------")
(probe "C: " C)
(probe "F: " F)

(set-value! C 3 'user)
(forget-value! C 'user)
(set-value! F 3 'user)
(print "-------- END  celsius-fahrenheit-converter-better ------")
