#!/usr/bin/env guile 
!#
(load "ch3-p183-table.scm")
(load "basic.scm")

(print "------------ TEST table ------------------------")
(define t (make-table))
(insert! 'a 1 t)
(insert! 'b 2 t)
(insert! 'c 3 t)
(insert! 'b 20 t)
(print (lookup 'b t))
(print (lookup 'z t))
(print "------------ END  table ------------------------")

(print "------------ TEST table-2d ---------------------")
(print (get 'woman 'ugly))
(print (put 'woman 'ugly 2))
(print (get 'woman 'ugly))
(print (put 'woman 'ugly 3))
(print (get 'woman 'ugly))
(print (put 'man 'handsome 0))
(print (put 'man 'awesome 1))
(print (put 2.001 3.002 (* 2.001 3.002)))
(print (get 2 3))
(print "------------ END  table-2d ---------------------")

(print "------------ TEST table-2d-new -----------------")
(print (put-new 2.001 3.002 (* 2.001 3.002)))
(print (get-new 2 3))
(print "------------ END  table-2d-new -----------------")
