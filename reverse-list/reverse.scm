(define data '(1 2 3 4 5 6 7 8 9))

(define (Reverse lst)
  (if (null? lst) '()
    (append (Reverse(cdr lst)) (list (car lst)))
  )
)

(display (Reverse data))