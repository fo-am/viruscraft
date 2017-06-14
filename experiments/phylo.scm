#lang racket

(define next-id 0)
(define (get-id!) (set! next-id (+ next-id 1)) next-id)

(define (population id size parent age) (list id size parent age))
(define (population-id p) (list-ref p 0))
(define (population-size p) (list-ref p 1))
(define (population-parent p) (list-ref p 2))
(define (population-age p) (list-ref p 3))

(define (population->string p)
  (string-append (number->string (population-id p)) " "
                 (number->string (population-size p)) " "
                 (number->string (population-parent p)) " "
                 (number->string (population-age p))))

(define branch-prob-mul 0.5)
(define start-population-size 1)
(define max-population-size 10)
(define max-populations 20)

(define (population-step p size)
  (population (population-id p) size (population-parent p) (+ (population-age p) 1)))

(define (population-update p)
  (let ((branch-prob (* (/ (population-size p) max-population-size) branch-prob-mul)))
    (cond
      ((< (population-size p) 1) 'extinct)
      ((< (random) branch-prob) 'branch)
      ((and (< (random) 0.5) (< (population-size p) max-population-size))
       (population-step p (+ (population-size p) 1)))
      (else
       (population-step p (- (population-size p) 1))))))

(define (update-populations ps)
  (foldl
   (lambda (p r)
     (let ((res (population-update p)))
       (cond
         ((eq? res 'extinct)
          (if (> (length ps) 1) r (cons p r)))
         ((eq? res 'branch)
          (if (< (length ps) max-populations)
              (append (list p (population (get-id!) start-population-size (population-id p) 0)) r)
              (cons p r)))
         (else (cons res r)))))
   '() ps))

(define (populations->string ps)
  (foldl
   (lambda (p r)
     (string-append r (population->string p) "\n"))
   "" ps))

(define p (build-list 10 (lambda (_) (population (get-id!) start-population-size 0 0))))
(display p)(newline)

(define (loop p c)
  (when (not (zero? c))
    (display (populations->string p))
    (loop (update-populations p) (- c 1))))

(loop p 200)