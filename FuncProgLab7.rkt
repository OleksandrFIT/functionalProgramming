#lang racket
; Task 5.1
(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'text))

(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
      '()  
      (begin(displayln line)
      (append (list line) (next-line-it file))) ; зберігаємо порядково записи файлу в список
     )
   )
)

(define (capitalize-first-letter str)
  (cond
    [(non-empty-string? str)
     (define first-letter-str
       (substring str 0 1))
     (define rest-str
       (substring str 1 (string-length str)))
     (string-append (string-upcase first-letter-str)
                    rest-str)]
    [else
     ""]))

(define (capitalize-sentences-of-list user-list)
  (if (null? user-list)
    '()
    (append (list (capitalize-first-letter (car user-list))) (capitalize-sentences-of-list (cdr user-list))) 
 ))


(define words-list (list
                    "як умру, то поховайте Мене на могилі."
                    "серед степу широкого На Вкраїні милій."
                    "щоб лани широкополі,І Дніпро, і кручі."
                    "було видно, було чути, Як реве ревучий."
                   )
)

(list->file words-list "/Users/oleksandrdragan/Desktop/func/sevenLab_INPUT.txt") ; записуємо текст до файлу
(displayln "Вміст файлу: \n")
(define words-list-updated (call-with-input-file "/Users/oleksandrdragan/Desktop/func/sevenLab_INPUT.txt" next-line-it)) ; виводимо його вміст
(set! words-list-updated (capitalize-sentences-of-list words-list-updated)); Робимо перші літери в верхньому регістрі
(list->file words-list-updated "/Users/oleksandrdragan/Desktop/func/sevenLab_INPUT.txt") ; записуємо модифікований текст до результуючого файлу
(display "\nРезультат обробки тексту успішно записано у текстовий файл.")