(define is-atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(print "Check `is-atom?`")
(print (is-atom? 'hello) " == #t")
(print (is-atom? 1) " == #t")
(print (is-atom? '(1 2 3 4)) " == #f")
