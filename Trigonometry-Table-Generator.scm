(define pi 3.141592653589793)

(define (degrees-to-radians deg)
  (* (/ pi 180) deg))

(define (round-to n digits)
  (let ((factor (expt 10 digits)))
    (/ (round (* n factor)) factor)))

(define (format-number num width precision)
  (let ((formatted (number->string (round-to num precision))))
    (string-append (make-string (max 0 (- width (string-length formatted))) #\space) formatted)))

(define (print-trig-table start end step precision)
  (define (print-row deg)
    (display "| ")
    (display (format-number deg 8 2))
    (display " | ")
    (let ((sin-value (sin (degrees-to-radians deg)))
          (cos-value (cos (degrees-to-radians deg)))
          (tan-value (tan (degrees-to-radians deg))))
      (display (format-number sin-value 10 precision))
      (display " | ")
      (display (format-number cos-value 10 precision))
      (display " | ")
      (if (not (infinite? tan-value))
          (display (format-number tan-value 10 precision))
          (display "  inf  "))
      (display " |"))
    (newline))
  
  (define (print-line)
    (display "+----------+------------+------------+------------+")
    (newline))
  
  (print-line)
  (display "| Degrees  |   sin(x)   |   cos(x)   |   tan(x)   |")
  (newline)
  (print-line)
  
  (let loop ((deg start))
    (when (<= deg end)
      (print-row deg)
      (loop (+ deg step))))
  
  (print-line))

;; Пример вызова функции для вывода таблицы с шагом 5 градусов от 0 до 90 с точностью до 4 знаков после запятой
(print-trig-table 0 90 5 4)

