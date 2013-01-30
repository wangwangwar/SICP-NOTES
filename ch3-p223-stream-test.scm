(load "ch3-p223-stream.scm")

(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 2 (cons-stream 4 (cons-stream 6 the-empty-stream))))

(print (cdr s1))

(display-stream
(stream-map + s1 s2)
)
