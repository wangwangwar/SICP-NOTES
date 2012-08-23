#!/usr/bin/env guile
!#
(load "ch3-p173-variable-list.scm")
(load "basic.scm")

(print "------------ TEST set-car! set-cdr! ------------")
(define x '((a b) c d))
(print x)
 
(define y '(e f))
(set-car! x y)
(print x)
(print "------------ END set-car! set-cdr! -------------")


(print "------------ TEST append! ----------------------")
(define x '(a b))
(define y '(c d))
(define z (append x y))
(print z)
(print (cdr x))
(define w (append! x y))
(print w)
(print (cdr x))
(print "------------ END append! -----------------------")

(print "------------ TEST make-cycle -------------------")
(define z (make-cycle '(a b c)))
(print z)
; (last-pair z)
(print "------------ END make-cycle --------------------")

(print "------------ TEST mystery ----------------------")
(define v '(a b c))
(print v)
(define w (mystery v))
(print w)
(print "------------ END mystery -----------------------")

(print "------------ TEST z1 ---------------------------")
(print z1)
(print z1)
(print z2)
(print z2)
(print "------------ END  z1 ---------------------------")

(print "------------ TEST count-pairs ------------------")
(print (count-pairs pair1))
(print (count-pairs pair2))
(print (count-pairs pair3))
; (print (count-pairs pair4))
(print "------------ END  count-pairs ------------------")

(print "------------ TEST new-count-pairs --------------")
(print (new-count-pairs pair1))
(print (new-count-pairs pair2))
(print "------------ END  new-count-pairs --------------")

(print "------------ TEST check-list-ring --------------")
(print (check-list-ring '(1 2 3)))
(print (check-list-ring pair4))
(print "------------ END  check-list-ring --------------")

(print "------------ TEST check-list-ring-smart --------")
(print (check-list-ring-smart '(1 2 3)))
(print (check-list-ring-smart pair4))
(print "------------ END  check-list-ring-smart --------")
