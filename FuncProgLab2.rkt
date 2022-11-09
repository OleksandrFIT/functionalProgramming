
(display "Task 1 \n")
(define (logarithm x . n)
  (cond ((not (null? n))
          (cond ((< 50 (car n))
                 0)
                (else
                 (+(*
                    (expt -1 (- (car n) 1))
                    (/(expt (- x 1) (car n)) (car n)))
                   (logarithm x (+ 1 (car n)))))))
          (else (logarithm x 1))))

(define (y x max-x step)
  (begin
    (if (and (> x 0) (<= x 2))
        (begin
          (display (+ (logarithm x) (logarithm (/ x 2)))) ; обчислення натурального логарифму при 0 < x <= 2
          (display "\n")
          (y (+ step x) max-x step))
        (if (and (> x 2) (< x max-x))
            (begin
              (display (logarithm(- (/ x 2) 1))) ; обчислення натурального логарифму при 2 < x < 3
              (display "\n")
              (y (+ step x) max-x step))))
        (if (<= x 0)
            (y (+ step x) max-x step)))) ; рекурсивний виклик з новим значенням x

(y -1 3 0.5)

; Task 2
(display "\nTask 2 ")
(display "\nЗадайте значення точності для виклику функції: ")
(define (calculate-fraction n m accurancy)
  (if (>= 0 accurancy) ; умова виходу з рекурсивної процедури
      1
      (+ n (/ m (calculate-fraction n m (- accurancy 1)))) ; обчислення дробу згідно умови
  )
)

(+ 1 (/ 1 (calculate-fraction 2 1 (read))))