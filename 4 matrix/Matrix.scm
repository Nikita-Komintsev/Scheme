(define (create-matrix m)
    (let ((M m))
        (define (print)
            M
        )
        (define (row i)
            (if (= i 1) (car M) (car (cdr M)))
        )
        (define (column i)
            (if (= i 1)
                (list (car (row 1)) (car (row 2)))
                (list (car (cdr (row 1))) (car (cdr (row 2))))
            )
        )                                                               ;     j1 j2
        (define (element i j)                                           ;  i1 1   2
            (if (= j 1)                                                 ;  i2 3   4
                (car (row i))
                (car (cdr (row i)))
            )
        )
        (define (multM N)
            (list
                (list
                    (apply + (map * (row 1) (N 'column 1)))
                    (apply + (map * (row 1) (N 'column 2)))
                )
                (list
                    (apply + (map * (row 2) (N 'column 1)))
                    (apply + (map * (row 2) (N 'column 2)))
                )
            )
        )
        (define (sumM N)
            (list
                (map + (row 1) (N 'row 1))
                (map + (row 2) (N 'row 2))
            )
        )
        (define (multN n)
            (list
                (map * (row 1) (list n n))
                (map * (row 2) (list n n))
            )
        )
        (lambda args
            (apply
                (case (car args)
                    ((print) print)
                    ((row) row)
                    ((column) column)
                    ((element) element)
                    ((multM) multM)
                    ((multN) multN)
                    ((sumM) sumM)
                    ((transpon) transpon)
                    (else (begin (display "Invalid method\n") (exit 1)))
                )
                (cdr args)
            )
        )
    )
)

(define m1 (create-matrix(list (list 1 2) (list 3 4))))
(define m2 (create-matrix(list (list 1 1) (list 1 1))))

(display "Matrix 1: ")(display (m1 'print))
(newline)
(display "row 1: ")(display (m1 'row 1))
(newline)
(display "element 1 1: ")(display (m1 'element 1 1))
(newline)

(display "element 2 2: ")(display (m1 'element 2 2))
(newline)

(display "Matrix 2: ")(display (m2 'print))
(newline)

(display (m1 'print))(display "*")(display (m2 'print) )(display "= ") (display (m1 'multM m2))
(newline)

(display (m1 'print))(display "*")(display 5 )(display "= ") (display (m1 'multN 5))
(newline)

(display (m1 'print))(display "+")(display (m2 'print) )(display "= ")(display (m1 'sumM m2))
(newline)










; Функция row принимает индекс строки i.
; Если i равно 1, возвращается первая строка (первый элемент списка M).
; Если i равно 2, возвращается вторая строка (второй элемент списка M).

; Функция column принимает индекс столбца i.
; Если i равно 1, возвращается первый элемент первой строки и первый элемент второй строки.
; Если i равно 2, возвращается список из вторых элементов каждой строки.

; Функция element принимает индексы строки i и столбца j и возвращает элемент на пересечении этих индексов.
; если первый стоблец, то первый элемент строки
; если второй столбец, то второй элемент

; Функция multM
; возвращает результат умножения текущей матрицы на матрицу N.
; Умножение матриц происходит путем вычисления скалярных произведений строк и столбцов.

; Функция sumM
; возвращает результат сложения текущей матрицы с матрицей N.

; Функция multN возвращает результат умножения текущей матрицы на число n.

; lambda args
; лямбда-функция, которая принимает список аргументов args.
; В зависимости от первого элемента списка (команды) вызывается соответствующая функция (например, print, row, column, и т.д.),
; а оставшиеся элементы списка args передаются как аргументы этой функции.
