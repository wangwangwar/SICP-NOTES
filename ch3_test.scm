(load "ch3.scm")


(print "------------ TEST make-account -----------------")
(print (((make-account 100) 'withdraw) 19))
 
(print (((make-account 100) 'deposit) 19))
(print "------------ END make-account ------------------")

(print "------------ TEST make-accumulator -------------")
(define A (make-accumulator 100))
(print (A 10))
 
(print (A 20))
(print "------------ END make-accumulator --------------")

(print "------------ TEST make-monitored ---------------")
(define s (make-monitored sqrt))
(print (s 100))
 
(print (s 'how-many-calls?))
(print (s 'how-many-calls?))
 
(print (s 'reset-count))
(print "------------ END make-monitored ----------------")

(print "------------ TEST make-account-pw --------------")
(define acc (make-account-pw 100 'pass))
(print ((acc 'pass 'withdraw) 30))
(print ((acc 'other 'deposit) 40))
(print "------------ END make-account-pw ---------------")

(print "------------ TEST make-account-pw-7 ------------")
(define acc-7 (make-account-pw-7 100 'pass))
(print ((acc-7 'pass 'withdraw) 10))
 
(print ((acc-7 'other 'deposit) 100))
 
(print ((acc-7 'other 'deposit) 100))
 
(print ((acc-7 'other 'deposit) 100))
 
(print ((acc-7 'other 'deposit) 100))
 
(print ((acc-7 'other 'deposit) 100))
 
(print ((acc-7 'other 'deposit) 100))
 
(print ((acc-7 'other 'deposit) 100))
(print "------------ END make-account-pw-7 -------------")

(print "------------ TEST rand -------------------------")
(print (rand))
 
(print (rand))
(print "------------ END rand --------------------------")

(print "------------ TEST estimate-pi ------------------")
(print (estimate-pi 100))
 
(print (estimate-pi 1000))
 
(print (estimate-pi 10000))
 
(print (estimate-pi 100000))
(print "------------ END estimate-pi -------------------")

(print "------------ TEST estimate-integral-pi ---------")
(print estimate-integral-pi)
(print "------------ END estimate-integral-pi ---0------")

(print "------------ TEST make-joint -------------------")
(define peter-acc 
  (make-account-pw 100 'open-sesame))
(print ((peter-acc 'open-sesame 'withdraw) 10))
 

(define paul-acc
  (make-joint peter-acc 'open-sesame 'paul))
; (define invalid
;  (make-joint peter-acc 'wrong-password 'none))
(print ((paul-acc 'paul 'withdraw) 30))
(print "------------ END make-joint --------------------")

(print "------------ TEST f ----------------------------")
(print (+ (f 0) (f 1)))
(print "------------ END f -----------------------------")

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
