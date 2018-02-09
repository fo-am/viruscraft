(clear)
(set-fov 15 0.01 1000)
(show-axis 0)
(define p (with-state
        (translate (vector 0 -0.4 0))
  ;;      (hint-none)
        (hint-wire)
        (hint-vertcols)
        (scale 5)
        (wire-colour (vector 0 0 1))
        (line-width 10)
        (hint-unlit)
        (load-primitive "ico.obj")))

(with-primitive p
    (pdata-map! (lambda (c) (rndvec)) "c"))

(define (render)
    (with-primitive p
        (pdata-index-map! (lambda (i c) 
            (vector (sin (+ i (time))) (cos (+ i (time))) 0)) "c")))

(every-frame (render))