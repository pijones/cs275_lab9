;HW9
;Paul Jones
;Dan Barella
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