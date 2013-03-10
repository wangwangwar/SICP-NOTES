#!/usr/local/bin/guile
!#
(load "ch2.scm")
;;;;;;;;;;;;;;;;;;;
;;; 2.1 数据抽象导引
;;;;;;;;;;;;;;;;;;;


;;; 2.1.2 抽象屏障
;;;
(define x (make-point 3 9))
(define y (make-point -2 0))
(define s (make-segment x y))
(display "线段长度：")
(display (length-segment s))
(newline)

(define start-point (make-point 0 2))
(define end-point (make-point 0 0))
(define x-segment (make-segment start-point end-point))
(define start-point (make-point 4 0))
(define end-point (make-point 0 0))
(define y-segment (make-segment start-point end-point))
(define sq (make-square x-segment y-segment))
(newline)
(display "周长：")
(display (circum sq))
(newline)
(display "面积：")
(display (area sq))
(newline)

;;; 2.1.3 数据意味着什么
;;;
(display (car3 (cons3 3 4)))
(newline)
(display (cdr3 (cons3 5 0)))
(newline)

;;; test
(display (car4 (cons4 10 1)))
(newline)
(display (car4 (cons4 1000 1000)))
(newline)
(display (cdr4 (cons4 345 456)))
(newline)

;;; 2.1.4 扩展练习：区间算术
;;;
(define x (make-interval 4 10))
(define y (make-interval -7 13))
(display (add-interval x y))
(newline)
(display (sub-interval x y))
(newline)
(display (mul-interval x y))
(newline)
;;;(display (div-interval2 x y))
;;;(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2 层次性数据和闭包性质
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 2.2.1 序列的表示
;;; 
(define list1 (list 1 2 3 4))
(define list2 (list 4 5 6 78))
(display (append list1 list2))
(newline)
(display (append-r list1 list2))
(newline)


;;; 2.2.2 层次性结构
;;;
(for-each (lambda (x) (display x) (newline))
		  (list 3 4 5))

(define tree1 (cons (list 3 4 5) (list 8 9 10)))
(display (count-leaves tree1))
(newline)

(define x (list (list 1 2) (list 3 4)))
(display (reverse x))
(newline)
(display (deep-reverse x))
(newline)
(define y (list 1 (list 2 3 4) 5 (list 6 (list 7 8))))
(display y)
(newline)
(display (reverse y))
(newline)
(display (deep-reverse y))
(newline)

(define x '(1 (2 3) 4))
(display x)
(newline)
(display (cons 1 (cons (cons 2 (cons 3 '())) (cons 4 '()))))
(newline)
(display (fringe-r x))
(newline)

(define a (make-branch 3 10))
(define b (make-branch 2 8))
(define c (make-branch 8 2))
(define n (make-mobile b c))
(define d (make-branch 3 n))
(define m (make-mobile a d))
(display m)
(newline)
(display "left branch is ")
(display (left-branch m))
(newline)
(display "right branch is ")
(display (right-branch m))
(newline)
(display "length of left branch is ")
(display (branch-length (left-branch m)))
(newline)
(display (total-weight m))
(newline)
(display (check-balance m))
(newline)
(display (square-tree x))
(newline)
(display (square-tree-h x))
(newline)
(display (subsets '(3 4 5)))
(newline)
(display (sum-odd-squares x))
(newline)
(display (list-fib-squares 10))
(newline)
(display (horner-eval 2 '(1 3 0 5 0 1)))
(newline)

;;; 2.2.3 序列作为一种约定的界面
;;;
(define t '(1 (2 3)))
(display (count-leaves-beta t))
(newline)

;;;(define s '((1 2 3) (4 5 6) (7 8 9)))
;;;(display (accumulate-n + 0 s))
(display (cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) '())))
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.3 符号数据
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 2.3.1 引号
;;;
(print "---------- TEST memq --------------------------")
(print (memq 'apple '(pear banana prune)))
(print (memq 'apple '(x (apple sauce) y apple pear)))
(print "---------- END  memq --------------------------")

(print "---------- TEST equal? ------------------------")
(print (equal? 'a 'a))
(print (equal? 3 3))
(print (equal? '(this is a list) '(this is a list)))
(print (equal? '(this is a list) '(this (is a) list)))
(print (equal? '(this 3 is sb) '(this 2 is sb)))
(print "---------- END  equal? ------------------------")

;;; 2.3.2 实例：符号求导
;;;
(print "----------- TEST deriv ------------------------")
(print (deriv '(+ x 3) 'x))
(print "----------- END  deriv ------------------------")

;;; 2.3.3 实例：集合的表示
;;;
(print "----------- TEST operation on set -------------")
(define set1 '(1 2 3))
(define set2 '(1 3 5))
(print "set1 ")
(print set1)
(print "set2 ")
(print set2)
(print "----------- TEST adjoin-set -------------------")
(print (adjoin-set 4 set1))
(print "----------- END  adjoin-set -------------------")
(print "----------- TEST intersection-set -------------")
(print (intersection-set set1 set2))
(print "----------- END  intersection-set -------------")
(print "----------- TEST union-set --------------------")
(print (union-set set1 set2))
(print "----------- END  union-set --------------------")
(print "----------- END  operation on set -------------")

(print "----------- TEST operation on ordered set -----")
(print "----------- TEST adjoin-ordered-set -----------")
(print (adjoin-ordered-set 4 set1))
(print (adjoin-ordered-set 4 set2))
(print "----------- END  adjoin-ordered-set -----------")
(print "----------- TEST intersection-ordered-set -----")
(print (intersection-ordered-set set1 set2))
(print "----------- END  intersection-ordered-set -----")
(print "----------- TEST union-ordered-set ------------")
(print (union-ordered-set set1 set2))
(print "----------- END  union-ordered-set ------------")
(print "----------- END  operation on ordered set -----")

(print "----------- TEST tree->list-1 -----------------")
;       6
;      / \
;     4   7
;    /     \
;   1	   10
(define tree1 (make-tree 6 
						 (make-tree 4 
									(make-tree 1 '() '())
									'())
						 (make-tree 7
									'()
									(make-tree 10 '() '()))))
;        7
;       / \
;      3   9
;     / \   \
;    1   5  11
; list形式如下：
; '(7	(3
;			(1	()	())
;			(5	()	()))
;		(9	()
;			(11	()	())))
(define tree2 (make-tree 7
						 (make-tree 3
									(make-tree 1 '() '())
									(make-tree 5 '() '()))
						 (make-tree 9
									'()
									(make-tree 11 '() '()))))


;       3
;      / \
;     1   7
;        / \
;       5   9
;            \
;            11
(define tree3 (make-tree 3
						 (make-tree 1 '() '())
						 (make-tree 7
									(make-tree 5 '() '())
									(make-tree 9
											   '()
											   (make-tree 11 '() '())))))
;        5
;       / \
;      3   9
;     /   / \
;    1   7  11
(define tree4 (make-tree 5
						 (make-tree 3
									(make-tree 1 '() '())
									'())
						 (make-tree 9
									(make-tree 7 '() '())
									(make-tree 11 '() '()))))
(print (tree->list-1 tree1))
(print (tree->list-1 tree2))
; 1. (tree->list-1	'(7	
;						(3
;							(1	()	())
;							(5	()	()))
;						(9	
;							()
;							(11	()	())))
;
; 2. (append	(tree->list-1	'(3
;									(1	()	())
;									(5	()	())))
;				(cons	7
;						(tree->list-1	'(9
;											()
;											(11	()	())))))
;
; 3. (append	(append	(tree->list-1	'(1	()	()))
;						(cons	3
;								(tree->list-1	'(5	()	()))))
;				(cons	7
;						(tree->list-1	'(9
;											()
;											(11	()	())))))
;
; 4. (append	(append	(append	(tree->list-1	'())
;								(cons	1
;										(tree->list-1	'())))
;						(cons	3
;								(tree->list-1	'(5	()	()))))
;				(cons	7
;						(tree->list-1	'(9
;											()
;											(11	()	())))))
;
; 5. (append	(append	(append	'()
;								(cons	1
;										(tree->list-1	'())))
;						(cons	3
;								(tree->list-1	'(5	()	()))))
;				(cons	7
;						(tree->list-1	'(9
;											()
;											(11	()	())))))
;
; 6. (append	(append	(append	'()
;								(cons	1
;										'()))
;						(cons	3
;								(tree->list-1	'(5	()	()))))
;				(cons	7
;						(tree->list-1	'(9
;											()
;											(11	()	())))))
;
; 7. (append	(append	(append	'()
;								(cons	1
;										'()))
;						(cons	3
;								(append	(tree->list-1	'())
;										(cons	5
;												(tree->list-1	'())))))
;				(cons	7
;						(tree->list-1	'(9
;											()
;											(11	()	())))))
;
; 8. (append	(append	(append	'()
;								(cons	1
;										'()))
;						(cons	3
;								(append	'()
;										(cons	5
;												'()))))
;				(cons	7
;						(tree->list-1	'(9
;											()
;											(11	()	())))))
;
; 9. (append	(append	(append	'()
;								(cons	1
;										'()))
;						(cons	3
;								(append	'()
;										(cons	5
;												'()))))
;				(cons	7
;						(append	(tree->list-1	'())
;								(cons	9
;										(tree->list-1	'(11	()	()))))))
;
;10. (append	(append	(append	'()
;								(cons	1
;										'()))
;						(cons	3
;								(append	'()
;										(cons	5
;												'()))))
;				(cons	7
;						(append	'()
;								(cons	9
;										(append (tree->list-1	'())
;												(cons	11
;														(tree->list-1	'())))))))
;
;11. (append	(append	(append	'()
;								(cons	1
;										'()))
;						(cons	3
;								(append	'()
;										(cons	5
;												'()))))
;				(cons	7
;						(append	'()
;								(cons	9
;										(append '()
;												(cons	11
;														'()))))))
; 注意，我们这儿有多少个 append ？
; 6个，每一个节点会调用一次 append，则 tree->list-1 针对 append 的时间复杂度是 O(n)，而 append 针对节点数的时间复杂度又是 O(n)，则 tree->list-1 的时间复杂度是 O(n^2).
(print (tree->list-1 tree3))
(print (tree->list-1 tree4))
(print "----------- END  tree->list-1 -----------------")
(print "----------- TEST tree->list-2 -----------------")
(print (tree->list-2 tree1))
(print (tree->list-2 tree2))
; 1. (tree->list-2	'(7	
;						(3
;							(1	()	())
;							(5	()	()))
;						(9	
;							()
;							(11	()	())))
;
; 2. (copy-to-list	'(7	
;						(3
;							(1	()	())
;							(5	()	()))
;						(9	
;							()
;							(11	()	())))
;					'())
;
; 3. (copy-to-list	'(3
;						(1	()	())
;						(5	()	()))
;					(cons	7
;							(copy-to-list	'(9
;												()
;												(11	()	()))
;											'())))
;										
; 4. (copy-to-list	'(1	()	())
;					(cons	3
;							(copy-to-list	'(5	()	())
;											(cons	7
;													(copy-to-list	'(9
;																		()
;																		(11	()	()))
;																	'())))))
;
; 5. (copy-to-list	'()
;					(cons	1
;							(copy-to-list	'()
;											(cons	3
;													(copy-to-list	'(5	()	())
;																	(cons	7
;																			(copy-to-list	'(9
;																								()
;																								(11	()	()))
;																							'())))))))
;
; 6. (cons	1
;			(copy-to-list	'()
;							(cons	3
;									(copy-to-list	'(5	()	())
;													(cons	7
;															(copy-to-list	'(9
;																				()
;																				(11	()	()))
;																			'()))))))
; 7. (cons	1
;			(cons	3
;					(copy-to-list	'(5	()	())
;									(cons	7
;											(copy-to-list	'(9
;																()
;																(11	()	()))
;															'())))))
;
; 8. (cons	1
;			(cons	3
;					(copy-to-list	'()
;									(cons	5
;											(copy-to-list	'()
;															(cons	7
;																	(copy-to-list	'(9
;																						()
;																						(11	()	()))
;																					'())))))))
;
; 9. (cons	1
;			(cons	3
;					(cons	5
;							(copy-to-list	'()
;											(cons	7
;													(copy-to-list	'(9
;																		()
;																		(11	()	()))
;																	'()))))))
;
;10. (cons	1
;			(cons	3
;					(cons	5
;							(cons	7
;									(copy-to-list	'(9
;														()
;														(11	()	()))
;													'())))))
;
;11. (cons	1
;			(cons	3
;					(cons	5
;							(cons	7
;									(copy-to-list	'()
;													(cons	9
;															(copy-to-list	'(11	()	())
;																			'())))))))
;
;12. (cons	1
;			(cons	3
;					(cons	5
;							(cons	7
;									(cons	9
;											(copy-to-list	'(11	()	())
;															'()))))))
;
;13. (cons	1
;			(cons	3
;					(cons	5
;							(cons	7
;									(cons	9
;											(copy-to-list	'()
;															(cons	11
;																	(copy-to-list	'()
;																					'()))))))))
;
;14. (cons	1
;			(cons	3
;					(cons	5
;							(cons	7
;									(cons	9
;											(cons	11
;													(copy-to-list	'()
;																	'())))))))
;
;15. (cons	1
;			(cons	3
;					(cons	5
;							(cons	7
;									(cons	9
;											(cons	11
;													'()))))))
;
; 这里产生了 6 个 cons，一般来说，tree->list-2 针对 cons 的时间复杂度为 O(n)，cons 针对节点数的时间复杂度为 O(1)，所以 tree->list-2 针对节点数的时间复杂度为 O(n)。
;
(print (tree->list-2 tree3))
(print (tree->list-2 tree4))
(print "----------- END  tree->list-2 -----------------")

(print "----------- TEST list->tree -------------------")
(define list2 (tree->list-2 tree2))
(print (list->tree list2))
(print "----------- END  list->tree -------------------")

(print "----------- TEST union-tree-set ---------------")
(print (union-tree-set tree1 tree2))
(print "----------- END  union-tree-set ---------------")

(print "----------- TEST intersection-tree-set --------")
(print (intersection-tree-set tree1 tree2))
(print "----------- END  intersection-tree-set --------")


(print "----------- TEST symbols ----------------------")
(print (symbols sample-tree))
(print "----------- END  symbols ----------------------")

(print "----------- TEST encode -----------------------")
(print (encode decoded-message sample-tree))
(print "----------- END  encode -----------------------")

(print "----------- TEST generate-huffman-tree --------")
(define pairs '((A 4) (B 2) (C 1) (D 1)))
(print (generate-huffman-tree pairs))
(print sample-tree)
(print "----------- END  generate-huffman-tree --------")
