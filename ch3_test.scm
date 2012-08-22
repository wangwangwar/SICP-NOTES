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
