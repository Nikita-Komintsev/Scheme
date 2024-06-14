(define pi 3.141592653589793)

(define (round-to n digits)
  (let ((factor (expt 10 digits)))
    (/ (round (* n factor)) factor)))

(define (PrintLine)
  (display "+----------+------------+------------+------------+")
  (newline))

(define (PrintRow deg precision)
  (display "| ")
  (display (round-to deg 2))
  (display " | ")
  (let ((sin-value (sin (* (/ pi 180) deg)))
        (cos-value (cos (* (/ pi 180) deg)))
        (tan-value (tan (* (/ pi 180) deg))))
    (display (round-to sin-value precision))
    (display " | ")
    (display (round-to cos-value precision))
    (display " | ")
    (if (= (round-to cos-value precision) 0)
        (display "    inf   ")
        (display (round-to tan-value precision)))
    (display " |"))
  (newline))

(define (TrigTable start end step precision)
  (define (PrintHeader)
    (PrintLine)
    (display "| Degrees | sin(x) | cos(x) | tan(x) |")
    (newline)
    (PrintLine))

  (PrintHeader)

  (let loop ((deg start))
    (when (<= deg end)
      (PrintRow deg precision)
      (loop (+ deg step))))

  (PrintLine))

;; Пример вызова функции для вывода таблицы с шагом 5 градусов от 0 до 90 с точностью до 4 знаков после запятой
(TrigTable 0 90 5 4)
