#lang racket

(define (vector x y z) (list x y z))
(define (square x) (* x x))

(define (vx v) (list-ref v 0))
(define (vy v) (list-ref v 1))
(define (vz v) (list-ref v 2))
(define (vw v) (list-ref v 3))

(define (vector-clone v)
  (vector (vx v) (vy v) (vz v)))

(define (vadd a b)
  (vector (+ (vx a) (vx b))
          (+ (vy a) (vy b))
          (+ (vz a) (vz b))))

(define (vmag v)
  (sqrt (+ (square (vx v))
           (square (vy v))
           (square (vz v)))))

(define (vsub a b)
  (vector (- (vx a) (vx b))
          (- (vy a) (vy b))
          (- (vz a) (vz b))))

(define (vneg a)
  (vector (- 0 (vx a))
          (- 0 (vy a))
          (- 0 (vz a))))

(define (vmul v a)
  (vector (* (vx v) a) (* (vy v) a) (* (vz v) a)))

(define (vdiv v a)
  (vector (/ (vx v) a) (/ (vy v) a) (/ (vz v) a)))

(define (vdist a b)
  (vmag (vsub a b)))

(define (vnormalise v)
  (vdiv v (vmag v)))

(define (vdot a b)
  (+ (* (vx a) (vx b)) 
     (* (vy a) (vy b))
     (* (vz a) (vz b))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(define (point2-in-triangle? point a b c)
  (define (sign2 a b c)
    (- (* (- (vx a) (vx c)) 
	  (- (vy b) (vy c))) 
       (* (- (vx b) (vx c)) 
	  (- (vy a) (vy c)))))
  (let ((b1 (< (sign2 point a b) 0))
	(b2 (< (sign2 point b c) 0))
	(b3 (< (sign2 point c a) 0)))
    (and (eq? b1 b2) (eq? b2 b3))))

(define (point3->barycentric point a b c)
  (let ((v0 (vsub b a))
	(v1 (vsub c a))
	(v2 (vsub point a)))
    (let ((d00 (vdot v0 v0))
	  (d01 (vdot v0 v1))
	  (d11 (vdot v1 v1))
	  (d20 (vdot v2 v0))
	  (d21 (vdot v2 v1)))
      (let ((denom (- (* d00 d11) (* d01 d01))))
	(let ((v (/ (- (* d11 d20) (* d01 d21)) denom))
	      (w (/ (- (* d00 d21) (* d01 d20)) denom)))
	  (list v w (- 1 v w)))))))

(define (barycentric->trilinear point a b c)
  (vector
   (/ (vx point) (vdist b c))
   (/ (vy point) (vdist c a))
   (/ (vz point) (vdist a b))))

(define a (vector 0.86603 -0.5 0))
(define b (vector -0.86603 -0.5 0))
(define c (vector 0 1 0))

(for ((x (in-range -20 20)))
  (for ((y (in-range -20 20)))
    (let ((x (* x 0.06)) (y (* y 0.06)))
      (let ((bary (point3->barycentric (vector x y 0) a b c)))
        (let ((tri (barycentric->trilinear bary a b c)))
          (display
           (if (point2-in-triangle? (list x y) a b c)
               (if (and (> (vx bary) 0.2) (< (vx bary) 0.6)) "||" "==")
               " "))))))
  (newline))

