#lang racket

; Task 5.1

(define (vector-max-negative v)
  (apply max
         (vector->list
          (vector-filter negative? v)
         )
  )
)

(define (vector-min-positive v)
  (apply min
         (vector->list
          (vector-filter positive? v)
         )
  )
)

(define (find-index user_vector value index)
  (if (> index (vector-length user_vector))
      -1
      (if (= (vector-ref user_vector index) value)
          index
          (find-index user_vector value (+ index 1))
      )
  )
)

(display "Task 1")
(newline)

(define user_vector (list->vector (list 6 -3 0 34 -18 -99 -1 5 766 62 54 12 -8)))
(display (~a "Користувацький вектор вектор є вектором: " (vector? user_vector))) ; перевірка чи це дійсно вектор
(newline)
(display (~a "Користувацький вектор: " user_vector))
(newline)
(define min-positive (vector-min-positive user_vector))
(define max-negative (vector-max-negative user_vector))
(display (~a "Найбільше від'ємне значення вектора: " max-negative "\n" "Найменше додатне значення вектора: " min-positive ))
(newline)
(display (~a "Індекс найбільш від'ємного значення: " (find-index user_vector max-negative 0) "\n"))
(display (~a "Індекс найменшого додатнього значення: " (find-index user_vector min-positive 0) "\n"))


; Task 5.2
(display "\nTask 2")
(newline)

(define (merge-queues first-queue second-queue)
  (if (and (null? first-queue) (null? second-queue))
      '()
      (if (null? first-queue)
            (append (list (car second-queue))
                    (merge-queues '() (cdr second-queue))) ; додаємо елемент лише з другої черги
            
            (if (null? second-queue)
                     (append (list (car first-queue))
                             (merge-queues (cdr first-queue) '())) ; додаємо елемент лише з першої черги
                     (append (list (car first-queue) (car second-queue))
                             (merge-queues (cdr first-queue) (cdr second-queue))) ; додаємо елементи з обох черг
            )
      )
  )
)

(define first-queue (list 1 2 3 4 5 6)) ; перша черга
(define second-queue (list 7 8 9 10 11 12)) ; друга черга

(display (~a "Перша черга: " first-queue "\n"))
(display (~a "Друга черга: " second-queue "\n"))
(define third-queue (merge-queues first-queue second-queue)) ; результуюча об'єднана третя черга
(display (~a "Фінальна черга: " third-queue))