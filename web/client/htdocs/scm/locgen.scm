(clear)

(define p
    (with-state
        (scale 3)
        (load-primitive "../models/planet-test.obj")))

(define (close? p l thresh)
    (cond
        ((null? l) #f)
        ((< (vdist p (car l)) thresh) #t)
        (else (close? p (cdr l) thresh))))

(define (rpos l n thresh)
    (let ((p (pdata-ref "p" (inexact->exact (round (* (rndf) (pdata-size)))))))
        (cond
            ((zero? n) (vector 0 0 0))
            ((and (> (vmag p) 1.0001) (not (close? p l thresh))) p)
            (else (rpos l (- n 1) thresh)))))

(define (build-locations size thresh l)
    (if (>= (length l) size)
        l
        (build-locations size thresh (cons (rpos l 100 thresh) l))))


(let ((f (open-output-file "landpoints.scm" #:exists 'replace)))
    (with-primitive p
        (let ((loc (build-locations 200 0.1 '())))
            (for-each 
                (lambda (pos)
                    (when (equal? pos (vector 0 0 0)) (display pos)(newline))
                    (with-state
                        (colour (vector 0 1 0))
                        (translate (vmul pos 3.1))
                        (scale 0.1)
                        (build-cube)))
                loc)
            (display loc f)
            ))
    (close-output-port f))