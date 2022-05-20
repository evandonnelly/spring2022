#lang racket


; ------------------------------------------------------------------------------------------------------------------------------

; REFLEXIVE-CLOSURE
; Step 1: Add elements of L to new list
; Step 2: Add non-duplicates of dummyList to new list
; Step 3: Return new list
(define (Reflexive-Closure L S)
  (define dummyList (makeDummyList S))
  (removeDupes dummyList L)
  )

; HELPER METHODS

; Add non-duplicates of both sets
(define (removeDupes FROM TO)
  (if (empty? FROM)
      TO
      (if (pairMember? (car FROM) TO)
          (removeDupes (cdr FROM) TO)
          (removeDupes (cdr FROM) (append TO (cons (car FROM) '())))
          )
      )
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

; ------------------------------------------------------------------------------------------------------------------------------

; SYMMETRIC-CLOSURE

(define (Symmetric-Closure)
  #t)

; HELPER METHODS

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

; ------------------------------------------------------------------------------------------------------------------------------

; TRANSITIVE-CLOSURE



; HELPTER METHODS

; Builds transitive pair list of all required transitive pairs
(define (makeTransitivePairList listOne listTwo transitiveList)
  (if (empty? listOne)
      transitiveList
      (makeTransitivePairList (cdr listOne) listTwo (append transitiveList (comPairToList (car listOne) listTwo '())))
      )
  )

; Compare pair to list and create transitive list
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

; ------------------------------------------------------------------------------------------------------------------------------

; MISC HELPER METHODS

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

; check if a pair is a member of a list of pairs
(define (pairMember? pair list)
  (if (empty? list)
      #f
      (if (equal? pair (car list))
          #t
          (pairMember? pair (cdr list)))
      )
  )