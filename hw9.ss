;HW9
;Paul Jones
;Laura Watiker

(require "stream.ss")
(load "words.ss")
(load "keyboard.ss")

(define IntsFrom$ (lambda (n) (cons$ n (IntsFrom$ (+ n 1)))))
(define Ints$ (IntsFrom$ 0))
(define Evens$ (map$ (lambda (x) (* 2 x))  Ints$))
(define Ones$ (cons$ 1 Ones$))
(define Odds$ (+$ Evens$ Ones$))

;Section 2
;Exercise 1
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

;Section 3
;Exercise 2
(define member$
  (lambda (x s less-than-or-equal)
    (cond
      [(eq? x (car$ s)) #t]
      [else (if (less-than-or-equal x (car$ s)) (member$ x (cdr$ s) less-than-or-equal) #f)])))



;Section 4
(define pairsFrom$
  (lambda (p)
    (cons$ p (pairsFrom$ (nextPair p)))))

;Exercise 3
(define pairs$ (pairsFrom$ (cons 1 1)))

(define nextPair
  (lambda (p)
    (cond
      [(eq? (cdr p) 1) (cons 1 (+ 1 (car p)))]
      [else (cons (+ 1 (car p)) (- (cdr p) 1))])))


;Exercise 4

(define merge$
  (lambda (s1 s2)
    (cond
      [(< (car$ s1) (car$ s2)) (cons$ (car$ s1) (merge$ (cdr$ s1) s2))]
      [(< (car$ s2) (car$ s1)) (cons$ (car$ s2) (merge$ (cdr$ s2) s1))]
      [else (cons$ (car$ s1) (merge$ (cdr$ s1) (cdr$ s2)))])))

;Exercise 4 - part 2

(define scale
  (lambda (s n)
    (map$ (lambda (x) (* n x)) s)))

(define S
  (cons$ 1 (merge$ (merge$ (scale S 2) (scale S 3)) (scale S 5))))

;Exercise 5
(define *$
  
  (lambda (s1 s2) 
    
    (cons$ (* (car$ s1) (car$ s2)) (*$ (cdr$ s1) (cdr$ s2)))))

(define fact-stream$    
  (cons$ 1 (*$ fact-stream$ (IntsFrom$ 1))))

(define e-summands    
  (lambda (s)      
    (cons$ (/ 1 (car$ s)) (e-summands (cdr$ s)))))    

(define e-series$ (e-summands fact-stream$))

(define onezero$
  (cons$ 1 (cons$ 0 (cons$ -1 (cons$ 0 onezero$)))))

(define zeroone$
  (cons$ 0 (cons$ 1 (cons$ 0 (cons$ -1 zeroone$)))))

(define /$
  
  (lambda (s1 s2) 
    
    (cons$ (/ (car$ s1) (car$ s2)) (/$ (cdr$ s1) (cdr$ s2)))))

(define subsine$
  (/$ Ones$ fact-stream$))

(define sin-series$
  (*$ zeroone$ subsine$))

(define cos-series$
  (*$ onezero$ subsine$))


;Excercise 7
(define grune-a-b
  (lambda (s)
    (cond
      [(eq? 'the-empty-stream s) s]
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
                  [(eq? 'the-empty-stream s) s]
                  [(eq? a (car$ s)) (if (eq? (car$ (cdr$ s)) a) 
                                        (cons$ b (f(cdr$ (cdr$ s))))
                                        (cons$ (car$ s) (cons$ (car$ (cdr$ s)) (f (cdr$ (cdr$ s)) ))))]
                  [else (cons$ (car$ s) (f (cdr$ s)))]))])
      f)))