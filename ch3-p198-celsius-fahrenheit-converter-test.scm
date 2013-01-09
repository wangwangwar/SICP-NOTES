#!/usr/bin/env guile
!#
(load "ch3-p198-celsius-fahrenheit-converter.scm")
(load "basic.scm")

(print "------ TEST celsius-fahrenheit-converter -------")
(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)
(print "------ END  celsius-fahrenheit-converter -------")
