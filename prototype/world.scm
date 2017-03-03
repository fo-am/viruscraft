
;(define butterfly-colour (vector 1 1 0))
;(define plant-colour (vector 1 1 0))
;(define ground-1 (vector 1 0 0))
;(define ground-2 (vector 0 1 0))
;(define sky-colour (vector 1.0 0.5 0.5))

(define butterfly-colour (vector 0.5 0.5 1))
(define plant-colour (vector 0 1 0.5))
(define ground-1 (vector 0 0.2 0))
(define ground-2 (vector 0.3 0.6 0.9))
(define sky-colour (vector 0.5 0.5 1.0))

(define (build-plane)
  (let ((p (build-polygons 6 'triangles)))
    (with-primitive 
     p
     (pdata-map! (lambda (p) (vector 1 1 1)) "c")
     (pdata-set! "p" 0 (vector  0 1 0))
     (pdata-set! "p" 1 (vector  1 1 0))
     (pdata-set! "p" 2 (vector  0 0 0))
     (pdata-set! "p" 3 (vector  1 1 0))
     (pdata-set! "p" 4 (vector  1 0 0))
     (pdata-set! "p" 5 (vector  0 0 0))
     (pdata-set! "t" 0 (vector  0 1 0))
     (pdata-set! "t" 1 (vector  1 1 0))
     (pdata-set! "t" 2 (vector  0 0 0))
     (pdata-set! "t" 3 (vector  1 1 0))
     (pdata-set! "t" 4 (vector  1 0 0))
     (pdata-set! "t" 5 (vector  0 0 0))
     (pdata-set! "n" 0 (vector  0 0 1))
     (pdata-set! "n" 1 (vector  0 0 1))
     (pdata-set! "n" 2 (vector  0 0 1))
     (pdata-set! "n" 3 (vector  0 0 1))
     (pdata-set! "n" 4 (vector  0 0 1))
     (pdata-set! "n" 5 (vector  0 0 1))
     p)))

(define (butterfly body wingl wingr pos dir)
  (list body wingl wingr pos dir (rndf)))

(define (butterfly-body b) (list-ref b 0))
(define (butterfly-wingl b) (list-ref b 1))
(define (butterfly-wingr b) (list-ref b 2))
(define (butterfly-pos b) (list-ref b 3))
(define (butterfly-modify-pos b v) (list-replace b 3 v))
(define (butterfly-dir b) (list-ref b 4))
(define (butterfly-modify-dir b v) (list-replace b 4 v))
(define (butterfly-nonce b) (list-ref b 5))

(define (build-butterfly)
  (with-state
   (scale (vector 0.5 0.5 0.5))
   (hint-nozwrite)
   (blend-mode blend-src-alpha blend-one)
   (texture (load-texture "textures/bwmorph.png"))
   (rotate (vector 90 -45 0))
   (let ((body (build-locator)))
     (parent body)
     (with-state
      (hint-unlit)
      (translate (vector 0 0 -0.5))
      (let ((wingl (build-plane))
	    (wingr (with-state
		    (scale (vector 1 -1 1))
		    (build-plane))))

	(let ((c (vmul butterfly-colour (rndf))))
	  (with-primitive 
	   wingl
 	   (pdata-map! (lambda (p) c) "c"))
	  (with-primitive 
	   wingr
	   (pdata-map! (lambda (p) c) "c")))

	(butterfly body wingl wingr
		   (vadd (vmul (crndvec) 6) (vector 0 0 2)) 
		   (let ((rd (srndvec)))
		     (vector (vx rd) (vy rd) 0))))))))

(define (update-butterfly butterfly)
  (butterfly-modify-pos 
   (butterfly-modify-dir 
    butterfly
    (let ((t (* (+ (time) (* (butterfly-nonce butterfly) 100)) 
		(* (butterfly-nonce butterfly) 0.01))))
      (vmul (vector (sin t) (cos t) 0) 0.1)))
   (vadd (butterfly-pos butterfly) 
	 (vmul (butterfly-dir butterfly) 0.1))))

(define (animate-butterfly butterfly)
  (let ((a (* 6 (sin (+ (* (butterfly-nonce butterfly) 3) 
			(* (* (butterfly-nonce butterfly) 0.4)
			   (time)))))))
    (with-primitive (butterfly-body butterfly)
		    (identity)
		    (translate (butterfly-pos butterfly))
		    (aim (butterfly-dir butterfly) (vector 0 0 1))
		    (rotate (vector 0 0 90)))
    (with-primitive (butterfly-wingl butterfly)
		    (rotate (vector a 0 0))) 
    (with-primitive (butterfly-wingr butterfly)
		    (rotate (vector a 0 0)))))

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

   (shader-set! "DiffuseColour1" ground-1)
   (shader-set! "DiffuseColour2" ground-2)
   ))

(define (build-plant)
  (let ((p (build-polygons 12 'triangles)))
    (with-primitive
     p
     (hint-unlit)
     ;;(hint-vertcols)
     ;;(hint-wire)
     (hint-nozwrite)
     ;;(blend-mode blend-src-alpha blend-one)
     (texture (choose (list (load-texture "textures/bwtree-1.png")
			    (load-texture "textures/bwtree-2.png"))))
     ;;(pdata-map! (lambda (p) (rndvec)) "p")
     (let ((c (vmul plant-colour (rndf))))
       (pdata-map! (lambda (p) c) "c"))
     (pdata-set! "p" 0 (vector -1 1 0))
     (pdata-set! "p" 1 (vector  1 1 0))
     (pdata-set! "p" 2 (vector -1 0 0))
     (pdata-set! "p" 3 (vector  1 1 0))
     (pdata-set! "p" 4 (vector  1 0 0))
     (pdata-set! "p" 5 (vector -1 0 0))

     (pdata-set! "p" 6 (vector 0 1 -1))
     (pdata-set! "p" 7 (vector 0 1  1))
     (pdata-set! "p" 8 (vector 0 0 -1))
     (pdata-set! "p" 9 (vector  0 1  1))
     (pdata-set! "p" 10 (vector 0 0 1))
     (pdata-set! "p" 11 (vector 0 0 -1))

     (pdata-set! "t" 0 (vector 0 0 0))
     (pdata-set! "t" 1 (vector 1 0 0))
     (pdata-set! "t" 2 (vector 0 1 0))
     (pdata-set! "t" 3 (vector 1 0 0))
     (pdata-set! "t" 4 (vector 1 1 0))
     (pdata-set! "t" 5 (vector 0 1 0))

     (pdata-set! "t" 6 (vector 0 0 0))
     (pdata-set! "t" 7 (vector 1 0 0))
     (pdata-set! "t" 8 (vector 0 1 0))
     (pdata-set! "t" 9 (vector 1 0 0))
     (pdata-set! "t" 10 (vector 1 1 0))
     (pdata-set! "t" 11 (vector 0 1 0))

     (translate (vector 0 1 0))
     (scale (vector (* (rndf) 0.3) (rndf) (* (rndf) 0.3)))
     (scale (vector 0.5 0.5 0.5))
     (apply-transform)
     (rotate (vmul (rndvec) 360))
     p)))

(define (build-world)
  (with-state
   (define plants (build-list (lambda (i) (build-plant)) 500))
   (define w (load-obj "models/world.obj"))
   ;;(with-primitive w (colour (vector 0 1 0)))
   (setup w)
   (for-each (lambda (p) (with-primitive p (parent w))) plants)
   
   (with-primitive 
    w
    ;(translate (vector 0 -3 0))
    (scale (vector 2.5 2.5 2.5)))
   w))

(clear)
(clear-colour sky-colour)

(define b (build-list (lambda (i) (build-butterfly)) 50))
(define w (build-world))
 
(every-frame 
 (begin
   (with-primitive 
    w (rotate (vector 0 0.1 0)))   
   (set! b (map update-butterfly b))
   (for-each animate-butterfly b)))
