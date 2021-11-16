(load "chapter01-toys.scm")

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or
       (eq? a (car lat))
       (member? a (cdr lat)))))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((is-atom? (car l)) (lat? (cdr l)))
     (else #f))))

(print "Check `member?`")
(print (member? 1 '(1 2 3)) " == #t")
(print (member? 3 '(1 2 3)) " == #t")
(print (member? 4 '(1 2 3)) " == #f")

(print "Check `lat?`")
(print (lat? '(1 2 3 4)) " == #t")
(print (lat? '(1 (2 3) 3 4)) " == #f")
