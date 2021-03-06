;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-popup text type time) (list text type time))
(define (popup-text p) (list-ref p 0))
(define (popup-type p) (list-ref p 1))
(define (popup-time p) (list-ref p 2))

(define popups-list '())

(define (clear-popups!)
  (set! popups-list '()))

(define (fire-popup! type text)
  ;; new position score, clean out old ones first
  (when (eq? type "position-score")
	(set! popups-list
	      (filter (lambda (p) (not (eq? (popup-type p) "position-score")))
		      popups-list)))
  ;; clean list if a bad message arrives
  (when (eq? type "bad")
	(set! popups-list '()))
  (set! popups-list 
	(append 
	 popups-list 
	 (list (make-popup text type popup-lifespan)))))

(define popup-x 300)
(define popup-y 650)
(define bubble-image #f)

(define (init-popups!)
  (set! bubble-image (find-image "bubble.png" image-lib)))

(define (render-popup! p)
  (ctx.drawImage bubble-image popup-x popup-y)
  (wrap-text ctx (popup-text p) popup-x popup-y 20 250))
  
(define popup-lifespan 5)

(define (render-popups!)
  (when (not (null? popups-list))
	(render-popup! (car popups-list))
	;; just update the timer on the first popup in list
	(set! popups-list
	      (cons
	       (make-popup 
		(popup-text (car popups-list)) 
		(popup-type (car popups-list)) 
		(- (popup-time (car popups-list)) canvas-delta))
	       (cdr popups-list)))
	(when (< (popup-time (car popups-list)) 0)
	      (set! popups-list (cdr popups-list)))))

