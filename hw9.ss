;HW9
;Paul Jones
;Laura Watiker

(require "stream.ss")

(define IntsFrom$ (lambda (n) (cons$ n (IntsFrom$ (+ n 1)))))
(define Ints$ (IntsFrom$ 0))
(define Evens$ (map$ (lambda (x) (* 2 x))  Ints$))
(define Ones$ (cons$ 1 Ones$))
(define Odds$ (+$ Evens$ Ones$))

(define rember-all
  (lambda (x s)
    (cond
      [(eq? (car$ s) x)(rember-all x (cdr$ s))]
      [else (cons$ (car$ s) (rember-all x (cdr$ s)))])))

(define subst-all
  (lambda (x y s)
    (cond
      [(eq? (car$ s) x)(cons$ y (subst-all x y (cdr$ s)))]
      [else (cons$ (car$ s) (subst-all x y (cdr$ s)))])))

(define member$    

	                 (lambda (x s) 

	                      (cond 

	                           [(eq? x (car$ s)) #t] 
             
	                           [else (member$ x (cdr$ s))]))) 

;heres where i would put section 2, if i understood it



;exercise 3
(define pairsFrom$
     (lambda (p)
          (cons$ p (pairsFrom$ (nextPair p)))))

(define pairs$ (pairsFrom$ (cons 1 1)))

(define nextPair
  (lambda (p)
    (cond
      [(eq? (cdr p) 1) (cons 1 (+ 1 (car p)))]
      [else (cons (+ 1 (car p)) (- (cdr p) 1))])))


;exercise 4
;doesn't?? wtf is (scale???)
(define S
  (cons$ 1 (merge$ (scale S 2) (merge$ (scale S 3) (scale S 5)))))


;works!
(define merge$
  (lambda (s1 s2)
    (cond
      [(< (car$ s1) (car$ s2)) (cons$ (car$ s1) (merge$ (cdr$ s1) s2))]
      [(< (car$ s2) (car$ s1)) (cons$ (car$ s2) (merge$ (cdr$ s2) s1))]
      [else (cons$ (car$ s1) (merge$ (cdr$ s1) (cdr$ s2)))])))
