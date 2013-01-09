(load "ch3-p205-constraint-better.scm")
(load "ch3-p200-constraint.scm")
(load "ch3-p203-connector.scm")

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
