#! /usr/bin/guile
!#
(load "ch4-p264-driver.scm")

(define the-global-environment (setup-environment))

(driver-loop)
