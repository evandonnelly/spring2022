#lang racket


; QUESTION 4
; define function
(define (gen-list start end)
  (if (> start end)
      ; base case
      empty
      ; recursive case
      (cons start (gen-list (+ start 1) end))))

; QUESTION 5
; define function
(define (sum list)
  (if (empty? list)
      ; base case
      0
      ; recursive case -- add first item in current list to rest of list
      (+ (car list) (sum (cdr list)))))

; QUESTION 6
; define function
(define (retrieve-first-n n list)
  ; test if n is too big
  (cond
    [(> n (length list)) (-1)])
  ; test if n is too small
  (cond
    [(< n 1) (-1)])
  ; recursive statements
  (if (= n 1)
      ; base case
      (cons (car list) '())
      ; recursive case
      (cons (car list) (retrieve-first-n (- n 1) (cdr list)))))

; QUESTION 7
; define function
(define (pair-sum? list val)
  ; check if list is valid
  (if (< (length list) 2)
      #f
      (if (= (+ (car list) (car (cdr list))) val)
          ; base case
          #t
          ; recursive vase
          (pair-sum? (cdr list) val))))