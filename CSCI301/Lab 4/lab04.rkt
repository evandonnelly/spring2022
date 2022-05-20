#lang racket

; Helper method
(define (member? x list)
  ; check failed
  (if (empty? list)
      #f
      ; still checking
      (if (list? x)
          ; x is a list
          (if (list? (car list))
              ; if first element of list is a list
              (if (subset? x (car list))
                  #t
                  (member? x (cdr list)))
              (member? x (cdr list)))
          ; x is a number
          (if (eq? x (car list))
              ; base case
              #t
              ; recursive case
              (member? x (cdr list))))))

; Helper method
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


; ------------------------------------


; Quesition 1
(define (set-equal? List1 List2)
  ; base case
  (if (subset? List1 List2)
      (if (subset? List2 List1)
          #t
          #f)
      #f))

; Question 2 - Union
(define (union Set1 Set2)
  (if (< (length Set2) 3)
      ; base case
      (append Set1 (cons (car Set2) '()))
      ; recursive case
      (if (member? (car Set2) Set1)
          (union Set1 (cdr Set2))
          (union (append Set1 (car  Set2)) (cdr Set2))
       )
   )
)

; Question 2 - Intersect
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