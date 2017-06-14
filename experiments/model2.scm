#lang racket

(define (dbg x) (display x)(newline) x)
(define (msg x) (display x)(newline))

(define (infection-matrix-lookup matrix num-types type infected-types)
  (foldl
   (lambda (other r)
     (if (and (not r) (< (random 100) (list-ref matrix (+ (* type num-types) other))))
         #t r))
   #f
   infected-types))
     
(define (run-model state type infect-matrix num-types infected-types immune-prob susceptible-prob)
  (cond
    ((and (eq? state 'susceptible)
          (infection-matrix-lookup infect-matrix num-types type infected-types)) 'infected)
    ((and (eq? state 'infected)
          (< (random 100) immune-prob)) 'immune)
    ((and (eq? state 'immune)
          (< (random 100) susceptible-prob)) 'susceptible)
    (else state)))

(define (make-node id connections organisms) (list id connections organisms))
(define (node-id n) (list-ref n 0))
(define (node-connections n) (list-ref n 1))
(define (node-organisms n) (list-ref n 2))

(define (make-random-world size num-connections)
  (build-list size (lambda (id)
                     (make-node id (build-list num-connections (lambda (_) (random size)))
                                '()))))

(define (make-organism id type state) (list id type state))
(define (organism-id o) (list-ref o 0))
(define (organism-type o) (list-ref o 1))
(define (organism-state o) (list-ref o 2))

              

(define (world-populate world organisms-per-node infect-rate)
  (map
   (lambda (node)
     (make-node (node-id node)
                (node-connections node)
                (build-list organisms-per-node
                            (lambda (id)
                              (make-organism id 0 (< (random 100) infect-rate))))))
   world))

(define (node-infected-types node)
  (foldl
   (lambda (o r)
     (if (eq? (organism-state o) 'infected)
         (cons (organism-type o) r) r))
   '()
   (node-organisms node)))

(define (node-run node infect-matrix num-types immune-prob susceptible-prob)
  (let ((infected-types (node-infected-types node)))
    (msg infected-types)
    (make-node
     (node-id node)
     (node-connections node)
     (map
      (lambda (o)
        (make-organism (organism-id o)
                       (organism-type o) 
                       (run-model (organism-state o) (organism-type o)
                                  infect-matrix num-types
                                  infected-types
                                  immune-prob susceptible-prob)))
      (node-organisms node)))))
        
(define (world-run world infect-matrix num-types immune-prob susceptible-prob)
  (map
   (lambda (node)
     (node-run node infect-matrix num-types immune-prob susceptible-prob))
   world))

;; instructions are (org src dst
(define (make-instruction organism src dst)
  (list organism src dst "ins"))
(define (instruction-organism i) (list-ref i 0))
(define (instruction-src i) (list-ref i 1))
(define (instruction-dst i) (list-ref i 2))

(define (choose l) (list-ref l (random (length l))))

(define (node-build-instructions node move-prob)
  (foldl
   (lambda (o r)
     (if (< (random 100) move-prob)
         (cons (make-instruction o (node-id node) (choose (node-connections node))) r)
         r))
   '()
   (node-organisms node)))
                                    
(define (world-build-instructions world move-prob)
  (foldl
   (lambda (node r)
     (append r (node-build-instructions node move-prob)))
   '()
   world))

(define (organism-in-instructions? o il node-id)
  (cond
    ((null? il) #f)
    ((and
      (eq? node-id (instruction-src (car il)))
      (eq? (organism-id o) (organism-id (instruction-organism (car il)))))
     #t)
    (else (organism-in-instructions? o (cdr il) node-id))))

(define (organisms-here il node-id)
  (foldl
   (lambda (i r)
     (if (eq? node-id (instruction-dst i))
         (cons (instruction-organism i) r) r))
   '()
   il))

(define (node-instructions-remove node instructions)
  (make-node
   (node-id node)
   (node-connections node)
   (filter
    (lambda (o)
      (not (organism-in-instructions? o instructions (node-id node))))
    (node-organisms node))))

(define (node-instructions-place node instructions)
  (make-node
   (node-id node)
   (node-connections node)
   (append
    (node-organisms node)
    (organisms-here instructions (node-id node)))))
        
(define (world-instructions-remove world instructions)
  (map (lambda (n) (node-instructions-remove n instructions)) world))

(define (world-instructions-place world instructions)
  (map (lambda (n) (node-instructions-place n instructions)) world))
 
(define (world-move-organisms world move-prob)
  (let ((instructions (world-build-instructions world move-prob)))
    (world-instructions-place
     (world-instructions-remove world instructions)
     instructions)))

(define (node-stats node)
  (list (length (node-organisms node))
        (foldl
         (lambda (o r)
           (if (eq? (organism-state o) 'infected) (+ r 1) r))
         0
         (node-organisms node))))
                
(define (world-stats world)
  (foldl
   (lambda (node r)
     (let ((stats (node-stats node)))
       (list (+ (list-ref r 0) (list-ref stats 0))
             (+ (list-ref r 1) (list-ref stats 1)))))
   (list 0 0)
   world))

(define (world-print world)
  (for-each
   (lambda (node)
     (display (node-id node))(display ":[")
     (for-each
      (lambda (o)
        (display (organism-type o))
        (when (eq? (organism-state o) 'susceptible) (display "s"))
        (when (eq? (organism-state o) 'infected) (display "i"))
        (when (eq? (organism-state o) 'immune) (display "m"))
        (display " "))
      (node-organisms node))
     (display "] ")
     )
   world)(newline))

(define (make-world)
  (world-populate (make-random-world 10 2) 2 50))

(define (simple-world)
  (list (make-node 0 '(1) (cons (make-organism 9 0 'infected)
                                (build-list 9 (lambda (id) (make-organism id 0 'susceptible)))))
        (make-node 1 '(0 2) (build-list 10 (lambda (id) (make-organism (+ id 20) 1 'susceptible))))
        (make-node 2 '(1) (build-list 10 (lambda (id) (make-organism (+ id 50) 2 'susceptible))))))

(define num-types 3)
(define infect-matrix (list 10 0  0
                            10 10 0
                            0  10 10))


(define (world-step world)
  (world-print world)
  (display (world-stats world))(newline)
  (world-run (world-move-organisms world 2) infect-matrix num-types 0 0))

(define (go w n)
  (when (not (zero? n))
    (go (world-step w) (- n 1))))

(define w (simple-world))
(go w 100)