#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here
; 1
(print (equal? (sequence 3 11 2) '(3 5 7 9 11)))
(print (equal? (sequence 3 8 3) '(3 6)))
(print (equal? (sequence 3 2 1) '()))
(newline)
 
; 2
(print (equal? (string-append-map '() "suffix") '()))
(print (equal? (string-append-map '("eblo" "sobachka") "suffix")
'("eblosuffix" "sobachkasuffix")))
(newline)
 
; 3
(print (equal? (list-nth-mod '(1 2 3 4 5) 14) 5))
(print (equal? (list-nth-mod '(1 2 3) 14) 3))
(print (equal? (list-nth-mod '(1 2 3 4 5 6 7) 25) 5))
(newline)

; 4
(define ones (λ () (cons 1 ones)))
(define onetwos
(letrec ([ones (λ () (cons 1 twos))]
[twos (λ () (cons 2 ones))])
ones))
 
(print (equal? (stream-for-n-steps ones 5) '(1 1 1 1 1)))
(print (equal? (stream-for-n-steps onetwos 5) '(1 2 1 2 1)))
(newline)
 
; 5
(print (equal? (stream-for-n-steps funny-number-stream 10)
'(1 2 3 4 -5 6 7 8 9 -10)))
(newline)
 
; 6
(print (equal? (stream-for-n-steps dan-then-dog 5)
'("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg")))
(newline)
 
; 7
(print (equal? (stream-for-n-steps (stream-add-zero ones) 3)
'((0 . 1) (0 . 1) (0 . 1))))
(print (equal? (stream-for-n-steps (stream-add-zero onetwos) 3)
'((0 . 1) (0 . 2) (0 . 1))))
(newline)

; 8
(print (equal? (stream-for-n-steps (cycle-lists '(1 2 3) '("a" "b")) 8)
'((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a") (2 . "b"))))
(newline)

;9 
(define vec  (vector (cons 5 2) (cons 3 6) 2))
(print (equal? (vector-assoc 2 vec) #f))
(print (equal? (vector-assoc 3 vec) '(3 . 6)))
(define xs '#((0 . "a") 4 (1 . "b") (2 . "c")))
(print (equal? (vector-assoc 1 xs) '(1 . "b")))
(define ys (vector ))
(print (equal? (vector-assoc 3 ys) #f))
(print (equal? (vector-assoc 5 (list->vector '((2 . 8) (5 . 7)))) '(5 . 7)))
(print (equal? (vector-assoc 5 (list->vector '((2 . 8) (5 . 3) (5 . 7)))) '(5 . 3)))
(print (equal? (vector-assoc 5 (list->vector '((2 . 8)))) #f))
(newline)

;10
(print (equal? ((cached-assoc '((2 . 8) (5 . 7)) 3) 5) '(5 . 7)))
(print (equal? ((cached-assoc '((2 . 8) (5 . 3) (5 . 7)) 1) 5) '(5 . 3)))
(print (equal? ((cached-assoc '((2 . 8)) 3) 5) #f))
(print (equal? ((cached-assoc '((2 . 3) (4 . 5) (5 . 6) (6 . 7)) 4) 2) '(2 . 3)))
(print (equal? ((cached-assoc '() 4) 2) #f))

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-n-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
  (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 27))
