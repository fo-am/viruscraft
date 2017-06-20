#lang racket

(define (dbg x) (display x)(newline) x)
(define (msg x) (display x)(newline))
(define (choose l) (list-ref l (random (length l))))
(define (list-contains? l v)
  (cond
    ((null? l) #f)
    ((eq? (car l) v) #t)
    (else (list-contains? (cdr l) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define genome-size 40)
(define species-distance 10)

(define genes (list "A" "T" "G" "C"))

(define (random-genome)
  (build-list genome-size (lambda (_) (choose genes))))

(define (genome-mutate genome mutation-rate)
  (map
   (lambda (gene)
     (if (< (random 100) mutation-rate)
         (choose genes) gene))
   genome))

(define (genome-distance a b)
  (foldl
   (lambda (a b r)
     (+ r (if (eq? a b) 0 1)))
   0 a b))

(define (genome-fitness genome)
  (let ((count (foldl
                (lambda (g r)
                  (list (+ (car r) (if (equal? g "A") 1 0))
                        (+ (cadr r) (if (equal? g "T") 1 0))))
                (list 0 0)
                genome)))
    (if (zero? (cadr count))
        1
        (/ (car count) (cadr count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fitness-percent-mul 10)

(define (run-model state genome infection-present)
  (let ((fitness (* fitness-percent-mul (genome-fitness genome))))
    (cond
      ((eq? state 'susceptible)
       (if (< (random 100) 1) 'dead ;; natural death
           (if (or (< (random 100) 5) ;; background infection
                   (and infection-present
                        (< (random 100) fitness))) 'infected state)))
      ((eq? state 'infected)
       (cond ((< (random 100) fitness) 'dead)
             ((< (random 100) (- fitness-percent-mul fitness)) 'immune)
             (else state)))
      ((eq? state 'immune)
       (if (< (random 100) 1) 'dead 'immune)) ;; natural death
      (else state))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-node id connections organisms) (list id connections organisms))
(define (node-id n) (list-ref n 0))
(define (node-connections n) (list-ref n 1))
(define (node-organisms n) (list-ref n 2))

(define (make-random-world size num-connections)
  (build-list size (lambda (id)
                     (make-node id (build-list num-connections (lambda (_) (random size)))
                                '()))))

(define cur-organism-id 0)
(define (generate-organism-id) (set! cur-organism-id (+ cur-organism-id 1)) cur-organism-id)
(define (make-organism id genome state parent) (list id genome state parent))
(define (organism-id o) (list-ref o 0))
(define (organism-genome o) (list-ref o 1))
(define (organism-state o) (list-ref o 2))
(define (organism-parent o) (list-ref o 3))

(define (new-organism) (make-organism (generate-organism-id) (random-genome) 'susceptible 0))

(define (organism-spawn o mutation-rate)
  (make-organism (generate-organism-id)
                 (genome-mutate (organism-genome o) mutation-rate)
                 'susceptible ;; all start as susceptible
                 (organism-id o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-node-infected? node)
  (foldl
   (lambda (o r)
     (if (eq? (organism-state o) 'infected) #t r))
   #f
   (node-organisms node)))

(define (node-run node)
  (let ((infection-present (is-node-infected? node)))
    (make-node
     (node-id node)
     (node-connections node)
     (map
      (lambda (o)
        (make-organism (organism-id o)
                       (organism-genome o) 
                       (run-model (organism-state o) (organism-genome o) infection-present)
                       (organism-parent o)))
      (node-organisms node)))))
        
(define (world-run world)
  (map
   (lambda (node)
     (node-run node))
   world))

;; instructions are (org src dst
(define (make-instruction organism src dst)
  (list organism src dst "ins"))
(define (instruction-organism i) (list-ref i 0))
(define (instruction-src i) (list-ref i 1))
(define (instruction-dst i) (list-ref i 2))

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

(define max-organisms-in-node 100)

(define (world-spawn-organisms world spawn-prob mutation-rate)
  (map
   (lambda (node)
     (make-node
      (node-id node)
      (node-connections node)
      (if (< (length (node-organisms node)) max-organisms-in-node)
          (foldl
           (lambda (o r)
             (cons o
                   (if (< (random 100) spawn-prob)
                       (cons (organism-spawn o mutation-rate) r)
                       r)))
           '()
           (node-organisms node))
          (node-organisms node))))
   world))

;; remove dead organisms
(define (world-remove-organisms world)
  (map
   (lambda (node)
     (make-node
      (node-id node)
      (node-connections node)
      (filter
       (lambda (o)
         (not (eq? (organism-state o) 'dead)))
       (node-organisms node))))
   world))

(define (world-average-fitness world)
  (/ (foldl
      (lambda (node r)
        (+ r
           (if (> (length (node-organisms node)) 0)
               (/ (foldl
                   (lambda (o r)
                     (+ r (genome-fitness (organism-genome o))))
                   0
                   (node-organisms node)) (length (node-organisms node)))
               0)))
      0
      world) (length world)))
  
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
        (display (apply string-append (organism-genome o)))
        (when (eq? (organism-state o) 'susceptible) (display "s"))
        (when (eq? (organism-state o) 'infected) (display "i"))
        (when (eq? (organism-state o) 'immune) (display "m"))
        (display " "))
      (node-organisms node))
     (display "] ")
     )
   world)(newline))

(define (world-print-fasta world)
  (for-each
   (lambda (node)
     (for-each
      (lambda (o)
        (display "> ")(display (organism-id o))(display " ")(display (exact->inexact (genome-fitness (organism-genome o))))(newline)
        (display (apply string-append (organism-genome o)))
        (newline))
      (node-organisms node))
     )
   world))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cur-phylo-id 0)
(define (generate-species-id) (set! cur-phylo-id (+ cur-phylo-id 1)) cur-phylo-id)

(define (make-species id parent genome organism-ids) (list id parent genome organism-ids))
(define (species-id p) (list-ref p 0))
(define (species-parent p) (list-ref p 1))
(define (species-genome p) (list-ref p 2))
(define (species-organism-ids p) (list-ref p 3))

(define (species-parent-search species-list id)
  (cond
    ((null? species-list) 0)
    ((list-contains? (species-organism-ids (car species-list)) id)
     (species-id (car species-list)))
    (else (species-parent-search (cdr species-list) id))))

(define (build-phylo species-list organism)
  (define (_ l)
    (let ((genome (organism-genome organism)))
      (cond
        ((null? l)        
         (list (make-species (generate-species-id)
                             (species-parent-search species-list (organism-parent organism))
                             genome (list (organism-id organism)))))
        ((< (genome-distance (species-genome (car l)) genome) species-distance)
         (cons (make-species (species-id (car l))
                             (species-parent (car l))
                             (species-genome (car l))
                             (if (list-contains? (species-organism-ids (car l)) (organism-id organism))
                                 (species-organism-ids (car l))
                                 (cons (organism-id organism) (species-organism-ids (car l)))))
               (cdr l)))
        (else
         (cons (car l) (_ (cdr l)))))))
  (_ species-list))

(define (world->phylo phylo world)
  (foldl
   (lambda (node r)
     (foldl
      (lambda (o r)
        (build-phylo r o))
      r
      (node-organisms node)))
   phylo
   world))

(define (printtrunc v) (/ (floor (* v 1000)) 1000))

(define (phylo->dot phylo)
  (string-append
   (foldl
    (lambda (species r)
      (let* ((size (length (species-organism-ids species)))
             (colour (cond ((< size 11) "white")
                           ((and (> size 10) (< size 51)) "green")
                           ((and (> size 50) (< size 201)) "yellow")
                           ((and (> size 200) (< size 501)) "orange")
                           (else "red"))))
        (if (eq? size 1)
            r
            (string-append
             r (if (zero? (species-parent species))
                   ""
                   (string-append
                    (number->string (species-parent species)) "->"
                    (number->string (species-id species)) "\n"))
             (number->string (species-id species)) " [label=\""
             (number->string (species-id species)) " : "
             (number->string (printtrunc (exact->inexact (genome-fitness (species-genome species)))))
             " : " (number->string size) "\" style=filled fillcolor=" colour " ]\n"))))
    "digraph{\nrankdir=\"LR\";\n"
    phylo)
   "}\n"))

(define (phylo->fasta phylo)
  (foldl
   (lambda (species r)
     (let* ((size (length (species-organism-ids species))))
       (if (eq? size 1)
           r
           (string-append
            r "> " (number->string (species-id species)) " "
            (number->string (exact->inexact (genome-fitness (species-genome species)))) " "
            (number->string size) "\n"
            (apply string-append (species-genome species)) "\n"))))
   ""
   phylo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simple-world)
  (list (make-node 0 '(1) (build-list 4 (lambda (id) (new-organism))))
        (make-node 1 '(0 2) (build-list 4 (lambda (id) (new-organism))))
        (make-node 2 '(1) (build-list 4 (lambda (id) (new-organism))))))

(define (world-step wp)
  (let ((world (car wp)) (phylo (cadr wp)))
    (display (world-stats world))(newline)
    (display (exact->inexact (world-average-fitness world)))(newline)
    (display (length phylo))(newline)
    (list (world-run (world-spawn-organisms (world-move-organisms (world-remove-organisms world) 2) 2 1))
          (world->phylo phylo world))))

(define (go wp n)
  (if (zero? n)
      wp
      (go (world-step wp) (- n 1))))

(define w (simple-world))
(define p '())
(define phy (cadr (go (list w p) 5000)))
(display (phylo->dot phy))(newline)
(display (phylo->fasta phy))(newline)         