(clear)

(define (setup p)
  (with-primitive
   p
   (pdata-add "bary")
   (pdata-map! (lambda (v) (vector 1 0 1)) "bary")
   (pdata-add "face_attr")
   (pdata-index-map! 
    (lambda (i v) 
      (vector 
       (+ 0.1 (/ (modulo (+ 20 (quotient i 3)) 50) 50))
       (+ 0.1 (/ (modulo (+ 20 (quotient i 3)) 50) 50)) 0)) 
    "face_attr")
   (pdata-index-map! 
    (lambda (i v) 
      (cond 
       ((eqv? (modulo i 3) 0) (vector 1 0 0))
       ((eqv? (modulo i 3) 1) (vector 0 1 0))
       (else (vector 0 0 1))))
    "bary")
   (shader (slurp "shaders/virus.vert.glsl")
	   (slurp "shaders/virus.frag.glsl"))
   (rotate (vector 12 23 0))   
   (scale (vector 2.7 2.7 2.7))
   ))

(define p (list
	   ;(load-obj "models/iso8.obj")
	   ;(load-obj "models/iso9.obj")
	   (load-obj "models/iso6.obj")
	   ))

(define i -3)
(for-each
 (lambda (p) 
   (set! i (+ i 3))
   (setup p)) p)

(clear-colour (vector 0 1 0 1))

;(every-frame
; (with-primitive
;  p
;  (rotate (vector 0.5 0.3 0.2))))
