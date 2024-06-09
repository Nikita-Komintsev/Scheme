
(define-syntax memoize
  (syntax-rules ()
    ((memoize (define func (lambda (args ...) body ...)))
      (begin 
        (define real_func (lambda (args ...) body ...))  
        (define func
          (let ((cache '()))
            (lambda (args ...)
              (if (eq? (assoc (list args ...) cache) #f)
                (begin
                  (display "Calculating function with args ")
                  (display (list args ...))
                  (newline) 
                  (set! cache (cons (list (list args ...) (real_func args ...)) cache))
                )
                '()
              )
              (cadr (assoc (list args ...) cache))
            )
          )
        )
      )
    )
  )
)


(memoize 
(define fib
  (lambda (n i)
    (if (or (= n 1)
            (= n 2))
        1
        (+ (fib (- n 1) i)
           (fib (- n 2) i)
           i)))))

(display (fib 10 1))
(newline) 
(display (fib 10 1))
(newline) 
