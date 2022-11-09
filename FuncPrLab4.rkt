#lang racket

(define (odd-indexed-values input-list)
  (define (inner-recursion input-list i)
    (cond ((null? input-list) '())
          ((odd? i) (cons (car input-list) (inner-recursion (cdr input-list) (+ i 1))))
          (else (inner-recursion (cdr input-list) (+ i 1)))))
  (inner-recursion input-list 0))

(define (user-reverse input-list)
   (if (null? input-list)
       '()
       (append (user-reverse (cdr input-list))
               (list (car input-list)))))

(define (sum-list input-list)
  (if (null? input-list)
      0
      (+ (car input-list) (sum-list(cdr input-list)))))

(define (is-in-list input-list value)
 (cond
  [(null? input-list) false]
  [(= (car input-list) value) true]
  [else (is-in-list (cdr input-list) value)]))

(define (remove-from-list input-list values)
  (if (null? input-list) '()
      (begin (if (is-in-list values (car input-list))
                 (append (list (car input-list)) (remove-from-list (cdr input-list) values))
                 (remove-from-list (cdr input-list) values)))))


; Task 5.1
(display "\nTask 1\n")
(display "Довжина списку:")
(define list-length (read))

(define user-list (build-list list-length (lambda (x) (expt 2 x))))
(begin (display "Вихідний масив: ")(display user-list)(display "\n"))

; a)
(set! user-list (remove-from-list user-list (odd-indexed-values user-list)))
(begin (display "Масив після видалення елементів з парними індексами: ")(display user-list)(display "\n"))

; б)
(display "Позиція для старту інвертування:")
(define reverse-position (read))

(set! user-list (append (take user-list reverse-position) (user-reverse (list-tail user-list reverse-position))))
(begin (display "Список після інвертування з заданої позиції: ")(display user-list)(display "\n"))

; в)
(begin (display "Сума елементів з непарними індексами: ") (display (sum-list (odd-indexed-values user-list))) (display "\n")) 

; Task 5.2
(display "\nTask 2\n")
(define buyers (list 500 320 100 234 11 111 9)) ; список покупців, задані цифровими ідентифікаторами
(begin (display "Покупці: ") (display buyers) (display "\n"))
(define time 180) ; сталий відомий час обслуговування касиром кожного покупця
(define buyers-time (map (lambda (element) (* time (+ (index-of buyers element) 1))) buyers))
(begin (display "Час перебування кожного покупця у черзі: ") (display buyers-time) (display "\n"))
(begin (display "Номер покупця, обслуговування якого потребує найменше часу: ") (display (list-ref buyers (index-of buyers (car buyers))))) ; Очевидно, шо перший покупець буде обслугований найпершим