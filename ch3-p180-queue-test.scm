#!/usr/bin/env guile
!#
(load "ch3-p180-queue.scm")
(load "basic.scm")

(print "------------ TEST queue ------------------------")
(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)
(print "------------ END  queue ------------------------")

(print "------------ TEST front-insert-deque! ----------")
(define d (make-deque))
(print "==== insert 'a ====")
(front-insert-deque! d 'a)
(print-deque d)
(print "==== insert 'b ====")
(front-insert-deque! d 'b)
(print-deque d)
(print "==== insert 'c ====")
(front-insert-deque! d 'c)
(print-deque d)
(print "------------ END  front-insert-deque! ----------")

(print "------------ TEST rear-insert-deque! -----------")
(define d2 (make-deque))
(print "==== insert 1 ====")
(rear-insert-deque! d2 1)
(print-deque d2)
(print "==== insert 2 ====")
(rear-insert-deque! d2 2)
(print-deque d2)
(print "==== insert 3 ====")
(rear-insert-deque! d2 3)
(print-deque d2)
(print "==== front-delete ====")
(front-delete-deque! d2)
(print-deque d2)
(print "==== rear-delete ====")
(rear-delete-deque! d2)
(print-deque d2)
(print "------------ END  rear-insert-deque! -----------")
