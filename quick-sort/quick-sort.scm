(define data '( 7 8 9 4 5 6 1 2 3 4))

(define (QuickSort lst)
    (if (null? lst) '()
        (let ((pivot (car lst)) (rest (cdr lst)))
            (append (QuickSort(filter (lambda (x) (< x pivot)) rest)) ; элементы < pivot
                (list pivot)
                (QuickSort(filter (lambda (x) (>= x pivot)) rest)) ; элементы >= pivot
            )
        )
    )
)

(display (QuickSort data))
