(require "stream.ss")

(define keyboard-stream                 ; (keyboard-stream) is a stream
  (lambda ()                            ; of s-expressions typed by the
    (display "? ")                      ; user
    (let ((this (read)))
      (if (eof-object? this)
	  'the-empty-stream
	  (cons$ this (keyboard-stream))))))

(define output$
  (lambda (s)
    (cond
     ((empty-stream? s) 'done)
     (else
      (display (car$ s))
      (newline) 
      (output$ (cdr$ s))))))

(define empty-stream?
  (lambda (s)
    (eq? s 'the-empty-stream)))