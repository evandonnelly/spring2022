#lang racket

; Question 1
(define (member? x list)
  ; check failed
  (if (empty? list)
      #f
      ; still checking
      (if (eq? x (car list))
          ; base case
          #t
          ; recursive case
          (member? x (cdr list)))))

; Question 2
(define (subset? List1 List2)
  ; base case
  (if (empty? List1)
      #t
      ; recursive case
      (if (member? (car List1) List2)
          ; check passed
          (subset? (cdr List1) List2)
          ; check failed
          #f)))

; Question 3
(define (set-equal? List1 List2)
  ; base case
  (if (subset? List1 List2)
      (if (subset? List2 List1)
          #t
          #f)
      #f))

; Question 4 - Union
(define (union Set1 Set2)
  (if (empty? Set2)
      ; base case
      Set1
      ; recursive case
      (if (member? (car Set2) Set1)
          (union Set1 (cdr Set2))
          (union (flatten (append Set1 (car  Set2))) (cdr Set2)))))

; Question 4 - Intersect
(define (intersect Set1 Set2)
  (if (subset? Set1 Set2)
      ; base case
      Set1
      ; recursive case
      (if (member? (car Set1) Set2)
          ; append (car Set1) to (cdr Set1)
          ; recur
          (intersect (flatten (append (cdr Set1) (car  Set1))) Set2)
          ; append nothing to (cdr Set1)
          ; recur
          (intersect (cdr Set1) Set2))))