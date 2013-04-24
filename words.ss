(require "stream.ss")

(define dict (open-input-file "words.txt"))

(define integer$ (lambda (z) (cons$ z (integer$ (add1 z)))))

(define words$ 
  (map$ (lambda (z) (symbol->string (read dict))) (integer$ 0)))

(define reset-words
  (lambda ()
    (close-input-port dict)
    (set! dict (open-input-file "words"))
    (set! words$ (map$ (lambda (z) (symbol->string (read dict))) (integer$ 0)))))