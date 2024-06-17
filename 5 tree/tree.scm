(define Tree '(
	 (
		(() 4 ())
		2
		(() 5 ())
	 )
	 1
	 (
		(() 6 ())
		3
		(() 7 ())
	 )
	)
)
(define Tree2 '(
	 (
		(()4 ())
		2
		(() 5 ())
	 )
	 1
	 (
		(() 3 ())
	 )
	)
)

(define left-branch car)
(define right-branch caddr)
(define node-value cadr)

(define (make-tree-iterator tree)
 (let ((caller #f))
	  (letrec ((traverse
			(lambda ()
			 (let loop ((tree tree))
				(if (not (null? tree))
				 (begin
					(loop (left-branch tree))
					(call/cc (lambda (rest-of-tree)								;call/cc  сохраняет текущее состояние обхода.
							(set! traverse (lambda () (rest-of-tree 'dummy)))
							(caller (node-value tree))							; caller вызывает значение узла.
						 )
					)
					(loop (right-branch tree))
				 )
				)
			 )
			 (caller 'end)

			)
		)) ;; letrec

	 (lambda () ;; iterator procedure
		(call/cc (lambda (k) (set! caller k) (traverse)))				;сохраняет текущее продолжение в caller и вызывает traverse.
	 ) ;; iterator proc end
	)
 )
)
;Создает итератор для дерева:
;
;caller инициализируется значением #f.
;traverse - рекурсивная функция для обхода дерева. letrec — это определение рекурсивных функций.
;loop рекурсивно обходит левое поддерево, вызывает call/cc для сохранения состояния обхода и возвращает значение узла, затем обходит правое поддерево.
;Если дерево пусто, возвращает 'end'.
;Итератор вызывается с использованием call/cc для установки продолжения и вызова traverse.

(define (make-list-iterator lst)
 (let ((caller #f))
	  (letrec ((traverse
			(lambda ()
			 (let loop ((lst lst))
				(if (not (null? lst))
				 (begin
					(call/cc (lambda (rest-of-list)
							(set! traverse (lambda () (rest-of-list 'dummy))) ; продолжить обход
							(caller (car lst))
						 )
					)
					(loop (cdr lst))
				 )
				)
			 )
			 (caller 'end)
			)
		))
	 (lambda ()
		(call/cc (lambda (k) (set! caller k) (traverse)))
	 ) ;; iterator proc end
	)
 )
)

(define (compare-tree-to-list t1 t2)
	(let ((caller #f) (iter1 (make-tree-iterator t1)) (iter2 (make-list-iterator t2)))  ;Создаем итераторы для обоих деревьев iter1 и iter2.
		(letrec ((loop (lambda (v1 v2)
			(cond																		; cond сравнивает значения узлов:
				((eq? v1 'end) (caller (eq? v2 'end)))									;Если оба значения 'end', деревья равны.
				((eq? v2 'end) (caller #f))												;Если только одно значение 'end', деревья не равны.
				((eq? v1 v2) (loop (iter1) (iter2)))									;Если значения узлов равны, продолжаем обход.
				(else (caller #f))														;Если значения узлов не равны, деревья не равны.
			))))
			(call/cc																	;Используем call/cc для сохранения состояния и вызова loop.
				(lambda (k)
					(set! caller k)														;set! изменяет значение существующей переменной.
					(loop (iter1) (iter2))
				)
			)
		)
	)
)


(define List '(4 2 5 1 6 3 7))
(display List)(newline)

(let ((iter (make-tree-iterator Tree)))
    (let loop ((val (iter)))
      (if (not (eq? val 'end))
          (begin
            (display val)(newline)
            (loop (iter))))))

(display (compare-tree-to-list Tree List))(newline)