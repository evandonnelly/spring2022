#lang racket

; Question 1: REFLEXIVE?
; Step 1: Create a list of twin pairs from S
; Step 2; Check if dummy list is equal to L
(define (Reflexive? L S)
  (define dummyList (makeDummyList S))
  (if (set-equal? dummyList L)
      #t
      #f
      )
  )

; Question 2: SYMETRIC?
; Step 1: Create a dummy list from the original list where all elements are reversed
; Step 2: Check if both lists are equal
(define (Symmetric? list)
  (define dummyList (makeReverseList '() list))
  (if (set-equal? dummyList list)
      #t
      #f
      )
  )


; Question 3: TRANSITIVE?
; Step 1: Find all potentially transitive pairs (a b) (b c)
; Step 2: Make dummy list of pairs (first-of-first second-of-second)  (a c)
; Step 3: Check if dummy list is subset of main list
(define (Transitive? list)
  (if (empty? list)
      #t
      (if (subset? (makeTransitivePairList list list '()) list)
          #t
          #f
          )
      )
  )

; HELPER METHODS

; builds transitive pair list of all required transitive pairs
(define (makeTransitivePairList listOne listTwo transitiveList)
  (if (empty? listOne)
      transitiveList
      (makeTransitivePairList (cdr listOne) listTwo (append transitiveList (comPairToList (car listOne) listTwo '())))
      )
  )

; compare pair to list and create transitive list
(define (comPairToList pair list transitiveList)
  (if (empty? list)
      transitiveList
      (comPairToList pair (cdr list) (append transitiveList (transitivePair pair (car list))))
      )
  )


; Build individual transitive pair dependent on the input
(define (transitivePair firstPair secondPair)
  (if (equal? (car (cdr firstPair)) (car secondPair))
      (if (equal? (car (cdr secondPair)) (car firstPair))
          ; both true
          (list (list (car firstPair) (car (cdr secondPair))) (list (car secondPair) (car (cdr firstPair))))
          ; only first true
          (list (list (car firstPair) (car (cdr secondPair))))
          )
      '()
      )
  )

; Make Reverse List
(define (makeReverseList newList originalList)
  ; check if list is empty
  (if (empty? originalList)
      newList
      (makeReverseList (append newList (reversePair (car originalList))) (cdr originalList))
      )
  )

; Reverse pair
(define (reversePair original)
  (define first (car original))
  (define second (car (cdr original)))
  (list (list second first))
  )


; checks if pair is a twin pair
(define (twinPair? pair)
  (if (eq? (car pair) (car (cdr pair)))
      #t
      #f
   )
)

; check if a pair is a member of a list of pairs
(define (pairMember? pair list)
  (if (empty? list)
      #f
      (if (equal? pair (car list))
          #t
          (pairMember? pair (cdr list)))
      )
  )
  

; check if List1 is a subset of List2
(define (subset? List1 List2)
  ; base case
  (if (empty? List1)
      #t
      ; recursive case
      (if (pairMember? (car List1) List2)
          ; check passed
          (subset? (cdr List1) List2)
          ; check failed
          #f)
      )
  )

; Check if sets are equal
(define (set-equal? List1 List2)
  ; base case
  (if (subset? List1 List2)
      (if (subset? List2 List1)
          #t
          #f)
      #f)
  )


; make twin pairs from single element
(define (makeTwinPair x)
  (cons (list x x) '())
  )

; make dummy list of twin pairs
(define (makeDummyList list)
  ; check if list is empty
  (if (empty? list)
      '()
      (if (list? (car list))
          list
          (makeDummyList (append (cdr list) (makeTwinPair (car list))))
          )
      )
  )