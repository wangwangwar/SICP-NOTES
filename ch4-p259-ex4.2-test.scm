#! /usr/local/bin/guile
!#
(load "ch4-p259-ex4.2.scm")
(load "ch4-p264-driver.scm")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (m-eval-call input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define the-global-environment (setup-environment))
(driver-loop)
