#lang racket

;; conectivity of a pentakis dodecahedron

;;      0     -     0     -     0     -     0     -     0     -    5  0
;;      |           |           |           |           |
;;  0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 -   15  1
;;  |       |   |       |   |       |   |       |   |       |
;;  0 - 0 - 0-0-0 - 0 - 0-0-0 - 0 - 0-0-0 - 0 - 0-0-0 - 0 - 0-0-  20  2
;;      |     |     |     |     |     |     |     |     |     |
;;  0 - 0 - 0-0-0 - 0 - 0-0-0 - 0 - 0-0-0 - 0 - 0-0-0 - 0 - 0-0-  20  3
;;  |       |   |       |   |       |   |       |   |       |
;;  0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 - 0 -   15  4
;;      |           |           |           |           |
;;      0     -     0     -     0     -     0     -     0     -    5  5


(define (vec2 x y) (list x y))
(define (vec2-x v) (list-ref v 0))
(define (vec2-y v) (list-ref v 1))
(define (vec2->str v)
  (string-append (number->string (vec2-x v)) "-" (number->string (vec2-y v))))

(define (face loc a b c) (list loc a b c))
(define (face-loc f) (list-ref f 0))
(define (face-a f) (list-ref f 1))
(define (face-b f) (list-ref f 2))
(define (face-c f) (list-ref f 3))
(define (face->dot f)
  (string-append
   "\"" (vec2->str (face-loc f)) "\" -- \"" (vec2->str (face-a f)) "\"\n" 
   "\"" (vec2->str (face-loc f)) "\" -- \"" (vec2->str (face-b f)) "\"\n" 
   "\"" (vec2->str (face-loc f)) "\" -- \"" (vec2->str (face-c f)) "\"\n"))

    
(define level2-stitch (list 0 0 2 4 0 6 8 0 10 12 0 14 16 0 18))
(define level3-stitch (list 0 0 2 0 3 0 5 0 6 0 8 0 9 0 11 0 12 0 14 0))

(define connect (list
                 (build-list 5 (lambda (i) (face (vec2 0 i)
                                                 (vec2 0 (modulo (- i 1) 5))
                                                 (vec2 0 (modulo (+ i 1) 5))
                                                 (vec2 1 (modulo (+ (* i 3) 1) 15)))))
                 (build-list 15 (lambda (i) (face (vec2 1 i)
                                                  (vec2 1 (modulo (- i 1) 15))
                                                  (vec2 1 (modulo (+ i 1) 15))
                                                  (if (zero? (modulo (+ i 2) 3))
                                                      (vec2 0 (quotient i 3))
                                                      (vec2 2 (list-ref level2-stitch i))))))
                 (build-list 20 (lambda (i) (face
                                             (vec2 2 i)
                                             (vec2 2 (modulo (- i 1) 20))
                                             (vec2 2 (modulo (+ i 1) 20))
                                             (if (zero? (modulo i 2))
                                                 (vec2 1 (list-ref level3-stitch i)) ;; up
                                                 (vec2 3 i))
                                             )))
                 (build-list 20 (lambda (i) (face
                                             (vec2 3 i)
                                             (vec2 3 (modulo (- i 1) 20))
                                             (vec2 3 (modulo (+ i 1) 20))
                                             (if (zero? (modulo i 2))
                                                 (vec2 4 (list-ref level3-stitch i)) ;; up
                                                 (vec2 2 i))
                                             )))
                 (build-list 15 (lambda (i) (face (vec2 4 i)
                                                  (vec2 4 (modulo (- i 1) 15))
                                                  (vec2 4 (modulo (+ i 1) 15))
                                                  (if (zero? (modulo (+ i 2) 3))
                                                      (vec2 5 (quotient i 3))
                                                      (vec2 3 (list-ref level2-stitch i))))))
                 (build-list 5 (lambda (i) (face (vec2 5 i)
                                                 (vec2 5 (modulo (- i 1) 5))
                                                 (vec2 5 (modulo (+ i 1) 5))
                                                 (vec2 4 (modulo (+ (* i 3) 1) 15)))))



                 ))

;;(display
;;(string-append
;; (foldl (lambda (f v) (display f) (string-append v (face->dot f))) "" (list-ref connect 0))
;; (foldl (lambda (f v) (string-append v (face->dot f))) "" (list-ref connect 1))
;; (foldl (lambda (f v) (string-append v (face->dot f))) "" (list-ref connect 2))
;; (foldl (lambda (f v) (string-append v (face->dot f))) "" (list-ref connect 3))
;; (foldl (lambda (f v) (string-append v (face->dot f))) "" (list-ref connect 4))
;; (foldl (lambda (f v) (string-append v (face->dot f))) "" (list-ref connect 5))))

(define (find-face connect loc)
  (list-ref
   (list-ref connect (vec2-x loc))
   (vec2-y loc)))

(define values
  (list
   (build-list 5 (lambda (i) (list (vec2 0 i) (random 2))))
   (build-list 15 (lambda (i) (list (vec2 1 i) (random 2))))
   (build-list 20 (lambda (i) (list (vec2 2 i) (random 2))))
   (build-list 20 (lambda (i) (list (vec2 3 i) (random 2))))
   (build-list 15 (lambda (i) (list (vec2 4 i) (random 2))))
   (build-list 5 (lambda (i) (list (vec2 5 i) (random 2))))))

(define (build-units connect values)
  (map
   (lambda (row)
     (map
      (lambda (value)
        (let ((loc (car value))
              (v (cadr value)))
          (let ((face (find-face connect loc))) ;; connectivity face
            (string-append
             ;;(number->string v)
             (number->string (cadr (find-face values (face-a face))))
             (number->string (cadr (find-face values (face-b face))))
             (number->string (cadr (find-face values (face-c face))))))))
      row))
   values))



(build-units connect values)

