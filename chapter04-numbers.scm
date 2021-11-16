(define o+
  (lambda (m n)
    (cond
     ((zero? n) m)
     (else (o+ (add1 m) (sub1 n))))))

(print "Check `o+`")
(print (o+ 17 12) " == 29")
(print (o+ 0 11) " == 11")

(define o-
  (lambda (m n)
    (cond
     ((zero? n) m)
     (else (o- (sub1 m) (sub1 n))))))

(print "Check `o-`")
(print (o- 17 12) " == 5")
(print (o- 11 0) " == 11")

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (o+ (car tup) (addtup (cdr tup)))))))

(print "Check `addtup`")
(print (addtup '(1 2 3)) " == 6")
(print (addtup '(10 20 30 40 50)) " == 150")

(define x
  (lambda (m n)
    (cond
     ((zero? n) 0)
     (else (o+ m (x m (sub1 n)))))))

(print "Check `x`")
(print (x 10 5) " == 50")
(print (x 5 1000) " == 5000")

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons
       (o+ (car tup1) (car tup2))
       (tup+ (cdr tup1) (cdr tup2)))))))

(print "Check `tup+`")
(print (tup+ '(1 2) '(3 4)) " == (4 6)")
(print (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) " == (11 11 11 11 11)")
(print (tup+ '(1 2 3) '(1 2)) " == (2 4 3)")
(print (tup+ '(1 2) '(1 2 3)) " == (2 4 3)")

(define o>
  (lambda (m n)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o> (sub1 m) (sub1 n))))))

(print "Check `o>`")
(print (o> 12 133) " == #f")
(print (o> 133 12) " == #t")
(print (o> 12 12) " == #f")

(define o<
  (lambda (m n)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o< (sub1 m) (sub1 n))))))

(print  "Check `o<`")
(print (o< 12 133) " == #t")
(print (o< 133 12) " == #f")
(print (o< 12 12) " == #f")

(define o=
  (lambda (m n)
    (cond
     ((o> m n) #f)
     ((o< m n) #f)
     (else #t))))

(print "Check `o=`")
(print (o= 12 133) " == #f")
(print (o= 133 12) " == #f")
(print (o= 12 12) " == #t")

(define o^
  (lambda (m n)
    (cond
     ((zero? n) 1)
     (else (x m (o^ m (sub1 n)))))))

(print "Check `o^`")
(print (o^ 1 1) " == 1")
(print (o^ 2 3) " == 8")
(print (o^ 5 3) " == 125")

(define oquotient
  (lambda (m n)
    (cond
     ((< m n) 0)
     (else (add1 (oquotient (o- m n) n))))))

(print "Check `oquotient`")
(print (oquotient 15 4) " == 3")
(print (oquotient 21 3) " == 7")

(define olength
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (olength (cdr lat)))))))

(print "Check `olength`")
(print (olength '(1 2 3)) " == 3")
(print (olength '()) " == 0")

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(print "Check pick")
(print (pick 4 '(lasagne spaghetti ravioli macaroni meatball)) " == macaroni")
(print (pick 3 '(hotdogs with hot mustard)) " == hot")

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(print "Check `rempick`")
(print (rempick 4 '(lasagne spaghetti ravioli macaroni meatball)) " == (lasagne spaghetti ravioli meatball)")
(print (rempick 3 '(hotdogs with hot mustard)) " == (hotdogs with mustard)")

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else (cons (car lat) (no-nums (cdr lat)))))))))

(print "Check `no-nums`")
(print (no-nums '(5 pears 6 plums 9 dates)) " == (pear plums dates)")

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
       (else (all-nums (cdr lat))))))))

(print "Check `all-nums`")
(print (all-nums '(5 pears 6 plums 9 dates)) " == (5 6 9)")
