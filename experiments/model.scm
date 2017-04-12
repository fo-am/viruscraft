#lang racket

(define (rndf) (random))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIS model

(define (make-model infection recovery immunity)
  (list infection recovery immunity))

(define (model-infection m) (list-ref m 0))
(define (model-recovery m) (list-ref m 1))
(define (model-immunity m) (list-ref m 2))

(define (model-update m incidence state)
  (cond
   ((< (rndf) 0.001) 'infected)
   ((eq? state 'susceptable)
    (if (< (rndf) (* (model-infection m) incidence)) 'infected 'susceptable))
   ((eq? state 'infected)
    (if (< (rndf) (model-recovery m)) 'recovered 'infected))
   ((eq? state 'recovered)
    (if (< (rndf) (model-immunity m)) 'susceptable 'recovered))
   (else state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spatial part

(define (make-node id state pos connections) (list id state pos connections))
(define (node-id n) (list-ref n 0))
(define (node-state n) (list-ref n 1))
(define (node-pos n) (list-ref n 2))
(define (node-connections n) (list-ref n 3))

(define (node-update node world model)
  (let ((incidence
	 (foldl  
	  (lambda (id r)
	    (if (eq? (node-state (world-find-node world id)) 'infected) 1 r))
	  0
	  (node-connections node))))
    (make-node
     (node-id node)
     (model-update model incidence (node-state node))
     (node-pos node)
     (node-connections node))))

(define (make-directed-world n-nodes n-connections infection-rate)
  (build-list
   n-nodes
   (lambda (i)
     (make-node i (if (< (rndf) infection-rate) 'infected 'susceptable) (list 0 0)
                (build-list
                 n-connections
                 (lambda (_) 
                   (random n-nodes)))))))

(define (list-contains? l v)
  (cond
   ((null? l) #f)
   ((eq? (car l) v) #t)
   (else (list-contains? (cdr l) v))))

(define (connected-nodes world id)
  (foldl
   (lambda (cn r)
     (if (list-contains? (node-connections cn) id)
	 (cons (node-id cn) r) r))
   '()
   world))

(define (make-world-undirected world)
  (map
   (lambda (n)
     (make-node 
      (node-id n)
      (node-pos n)
      (node-state n)
      (append (node-connections n) (connected-nodes world (node-id n)))))
   world))
     
(define (world-find-node world id)
  (cond
   ((null? world) #f)
   ((eq? (node-id (car world)) id) (car world))
   (else (world-find-node (cdr world) id))))

(define (world-update w model)
  (map (lambda (n)
         (node-update n w model))
       w))


(define w (make-world-undirected (make-directed-world 100 1 0)))
(define m (make-model 0.9 0.1 0.02))

