(define pi 3.141592653589793)

(define (round-to n digits)
  (let ((factor (expt 10 digits)))
    (/ (round (* n factor)) factor)))

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
        (display " inf ")
        (display (round-to tan-value precision)))
    (display " |"))
  (newline))

(define (TrigTable start end step precision)
  (display "-------------------------------------")
  (newline)
  (display "| Degrees | sin(x) | cos(x) | tan(x) |")
  (newline)
  (display "-------------------------------------")
  (newline)

  (let loop ((deg start))
    (when (<= deg end)
      (PrintRow deg precision)
      (loop (+ deg step))))

  (display "-------------------------------------"))

;; от 0 до 90  с шагом 5  с точностью до 4 знаков после запятой
(TrigTable 0 90 5 4)










;; Функция round-to
;; округляет число n до digits знаков после запятой.

;; Функция PrintRow
;; функция выводит строку таблицы для заданного угла deg с указанной точностью precision.
; Она рассчитывает синус, косинус и тангенс угла (угол переводится в радианы) и округляет эти значения до нужного количества знаков после запятой.

;; Функция TrigTable
;; с помощью рекурсивной внутренней функции loop вызывает PrintRow для каждого угла от start до end с шагом step.