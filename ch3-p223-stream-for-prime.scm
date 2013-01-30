(load "ch3-p223-stream.scm")
(load "ch2.scm")

(display-stream
(stream-filter prime?
               (stream-enumerate-interval 10 1000))
)
