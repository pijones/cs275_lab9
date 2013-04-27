;HW9
;Paul Jones
;Laura Watiker

(require "stream.ss")
(load "keyboard.ss")

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

(define hamming Ints$)

(define subst-all
  (lambda (x y s)
    (cond
      [(eq? (car$ s) x)(cons$ y (subst-all x y (cdr$ s)))]
      [else (cons$ (car$ s) (subst-all x y (cdr$ s)))])))

(define member$2    

	                 (lambda (x s) 

	                      (cond 

	                           [(eq? x (car$ s)) #t] 
             
	                           [else (member$2 x (cdr$ s))]))) 

;heres where i would put section 2, if i understood it

(define member$
  (lambda (x s less-than-or-equal)
    (cond
      [(eq? x (car$ s)) #t]
      [else (if (less-than-or-equal x (car$ s)) (member$ x (cdr$ s) less-than-or-equal) #f)])))



;exercise 3
(define pairsFrom$
     (lambda (p)
          (cons$ p (pairsFrom$ (nextPair p)))))

;(print$ (member$ '6 (IntsFrom$ 0)(lambda (x y) (<= x y))))
;section 3


(define pairs$ (pairsFrom$ (cons 1 1)))

(define nextPair
  (lambda (p)
    (cond
      [(eq? (cdr p) 1) (cons 1 (+ 1 (car p)))]
      [else (cons (+ 1 (car p)) (- (cdr p) 1))])))


;exercise 4
;doesn't?? wtf is (scale???)


;works!
(define merge$
  (lambda (s1 s2)
    (cond
      [(< (car$ s1) (car$ s2)) (cons$ (car$ s1) (merge$ (cdr$ s1) s2))]
      [(< (car$ s2) (car$ s1)) (cons$ (car$ s2) (merge$ (cdr$ s2) s1))]
      [else (cons$ (car$ s1) (merge$ (cdr$ s1) (cdr$ s2)))])))
;
;(define s3
;  (append '( 1 1 1) (map$ (lambda (x) (* 3 x))  s5)))

(define s2
  (map$ (lambda (x) (* 2 x)) hamming))
(define s3
  (map$ (lambda (x) (* 3 x)) hamming))
(define s5
  (map$ (lambda (x) (* 5 x)) hamming))

(define 2sq
  (map$ (lambda (x) (* 2 x)) s2))
(define 3sq
  (map$ (lambda (x) (* 3 x)) s3))
(define 5sq
  (map$ (lambda (x) (* 5 x)) s5))

(define s3-2
  (append '(2 3) (map$ (lambda (x) (* 3 x))  2sq)))
(define trial
  (map$ (lambda (x) (* 5 x)) s3-2))
(define s5-2
  (map$ (lambda (x) (* 5 x)) trial))
(define s5-3
  (map$ (lambda (x) (* 5 x)) s5-2))


;exercise 5
(define *$

                   (lambda (s1 s2) 

                            (cons$ (* (car$ s1) (car$ s2)) (*$ (cdr$ s1) (cdr$ s2)))))

(define fact-stream$    
     (cons$ 1 (*$ fact-stream$ (IntsFrom$ 1))))  

;Section 6

;Excercise 7
(define grune-a-b
  (lambda (s)
     (cond
       [(eq? 'the-empty-stream (car$ s)) 'done]
       [(eq? 'a (car$ s)) (if (eq? (car$ (cdr$ s)) 'a) 
                              (cons$ 'b (grune-a-b (cdr$ (cdr$ s))))
                              (cons$ (car$ s) (cons$ (car$ (cdr$ s)) (grune-a-b (cdr$ (cdr$ s)) ))))]
       [else (cons$ (car$ s) (grune-a-b (cdr$ s)))])))

;Exercise 8
(define grune
  (lambda (a b)
    (letrec ([f 
    (lambda (s)
      (cond
        [(eq? 'the-empty-stream (car$ s)) 'done]
       [(eq? a (car$ s)) (if (eq? (car$ (cdr$ s)) a) 
                              (cons$ b (f(cdr$ (cdr$ s))))
                              (cons$ (car$ s) (cons$ (car$ (cdr$ s)) (f (cdr$ (cdr$ s)) ))))]
       [else (cons$ (car$ s) (f (cdr$ s)))]))])
      f)))