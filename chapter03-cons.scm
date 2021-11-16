(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons
            (car (car l))
            (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
       (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(print "Check `rember`")
(print (rember 1 '(1 2 3)) " == (2 3)")
(print (rember 2 '(1 2 3)) " == (1 3)")
(print (rember 1 '(1 2 1)) " == (2 1)")

(print "Check `firsts`")
(print (firsts '((1 2) (3 4))) " == (1 3)")
(print (firsts '((1) (2) (3 4))) " == (1 2 3)")

(print "Check `insertR`")
(print (insertR 3 2 `(1 2 4 5)) " == (1 2 3 4 5)")
(print (insertR 3 2 `(1 2 2 5)) " == (1 2 3 2 5)")

(print "Check `insertL`")
(print (insertL 3 2 '(1 2 4 5)) " == (1 3 2 4 5)")
(print (insertL 3 4 '(1 2 4 5)) " == (1 2 3 4 5)")

(print "Check `subst`")
(print (subst 3 2 '(1 2 4 5)) " == (1 3 4 5)")

(print "Check `subst2")
(print (subst2 3 2 4 '(1 2 4 5)) " == (1 3 4 5)")
(print (subst2 3 2 4 '(1 1 4 5)) " == (1 1 3 5)")
(print (subst2 3 4 2 '(1 2 4 5)) " == (1 3 4 5)")

(print "Check `multirember`")
(print (multirember 1 '(1 2 3)) " == (2 3)")
(print (multirember 2 '(1 2 3)) " == (1 3)")
(print (multirember 1 '(1 2 1)) " == (2)")

(print "Check `multiinsertR`")
(print (multiinsertR 3 2 `(1 2 4 5)) " == (1 2 3 4 5)")
(print (multiinsertR 3 2 `(1 2 2 5)) " == (1 2 3 2 3 5)")

(print "Check `multiinsertL`")
(print (multiinsertL 3 2 '(1 2 4 5)) " == (1 3 2 4 5)")
(print (multiinsertL 3 2 '(1 2 2 5)) " == (1 3 2 3 2 5)")

(print "Check `multisubst`")
(print (multisubst 3 2 '(1 2 4 5)) " == (1 3 4 5)")
(print (multisubst 3 2 '(1 2 2 5)) " == (1 3 3 5)")
