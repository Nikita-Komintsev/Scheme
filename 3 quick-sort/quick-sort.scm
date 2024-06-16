(define (less-part lst pivot)
    (cond
    ((null? lst) '())
    ((> pivot (car lst)) (cons (car lst) (less-part (cdr lst) pivot)))
    (else (less-part (cdr lst) pivot)))
)

(define (eq-part lst pivot)
    (cond
    ((null? lst) '())
    ((= pivot (car lst)) (cons (car lst) (eq-part (cdr lst) pivot)))
    (else (eq-part (cdr lst) pivot)))
)

(define (more-part lst pivot)
    (cond
    ((null? lst) '())
    ((< pivot (car lst)) (cons (car lst) (more-part (cdr lst) pivot)))
    (else (more-part (cdr lst) pivot)))
)

(define (quicksort lst)
     (cond
        ((null? lst) '())
        (else (let ((pivot (car lst)))
               (append (append (quicksort (less-part lst pivot))
                                (eq-part lst pivot))
                       (quicksort (more-part lst pivot))))
        )
    )
)

(display (quicksort '(4 5 2 4 1 6 7 8 1 2 4 7 8 3 2)))