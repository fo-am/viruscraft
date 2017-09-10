#lang racket

;;;;;;;;;;;;;;;;;;;
;; minimal geometry

(define (vx v) (vector-ref v 0))
(define (vy v) (vector-ref v 1))

(define (sq x) (* x x))
(define (vec2-sq-dist a b)
  (+ (sq (- (vx a) (vx b)))
     (sq (- (vy a) (vy b)))))

(define (rndf) (exact->inexact (/ (random 10000) 10000)))

(define two-pi (* 2 3.141))
(define (make-circle pos radius) (list pos radius))
(define (circle-pos c) (list-ref c 0))
(define (circle-radius c) (list-ref c 1))

(define (circle-random-point-on-circumference circle)
  (vector
   (+ (vx (circle-pos circle)) (* (circle-radius circle) (sin (* (rndf) two-pi))))
   (+ (vy (circle-pos circle)) (* (circle-radius circle) (cos (* (rndf) two-pi))))))

(define (circle-intersect? a b)
  (< (vec2-sq-dist (circle-pos a) (circle-pos b))
     (+ (circle-radius a) (circle-radius b))))

(define (test-geom)
  (define a (make-circle (vector 0 0) 1))
  (define b (make-circle (vector 1.5 0) 1))
  (when (circle-intersect? a b) (display "test failed")(newline))
  (when (not (circle-intersect? a (make-circle (vector 1.5 0) 10))) (display "test failed")(newline)))

(test-geom)

;;;;;;;;;;;;;;;;;;;
;; receptor types

(define receptor-types (list 'circle 'square 'triangle 'donut))

(define (pick-random-receptor-type)
  (list-ref receptor-types (random (length receptor-types))))

;;;;;;;;;;;;;;;;;;;
;; virus
;; our virus simply consists of a set of keys for receptors we lock on to

(define (make-virus keys) (list keys))
(define (virus-keys v) (list-ref v 0))

(define (make-random-virus size)
  (make-virus (build-list size (lambda (_) (pick-random-receptor-type)))))

;;;;;;;;;;;;;;;;;;;
;; host
;; a host species, which may be susceptible to a virus

(define (make-host receptor infected pos radius)
  (list receptor infected pos radius))

(define (host-receptor h) (list-ref h 0))
(define (host-infected? h) (list-ref h 1))
(define (host-location h) (list-ref h 2))
;; size is both used for radius of visualisation circle and population size
(define (host-size h) (list-ref h 3))

;;;;;;;;;;;;;;;;;;;
;; host model

;; returns the susceptibility of the host of infection to the supplied virus
;; susceptibility will range from 0..1 with 0 being completely immune
(define (host-susceptibility host virus)
  (/ (foldl 
      (lambda (key r)
        ;; loop over the keys, counting the ones that match
        ;; we need to continously do this as the virus is being
        ;; mutated by the player
        (+ r (if (eq? key (host-receptor host)) 1 0)))
      0
      (virus-keys virus))
     (length (virus-keys virus))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reproduction and death of host species

;; the host's spatial constants 
(define min-host-size 0)
(define max-host-size 100)
(define host-size-growth 1)
(define host-size-start 1)

;; returns an updated radius based on infection and within constraints
(define (host-calculate-size host)
  (cond
    ((> (host-size host) max-host-size) max-host-size)
    ((host-infected? host) (max min-host-size (- (host-size host) host-size-growth)))
    ((+ (host-size host) host-size-growth))))

(define (host-alive? host)
  (> (host-size host) min-host-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creating new species
(define new-species-likelihood 5)

(define (host-can-branch-new-species? host)
  (if (host-infected? host)
      #f ;; infected hosts can't reproduce
      (< (random 100) new-species-likelihood)))

(define mutation-rate 5)

;; returns a new, mutated host species
(define (host-new-species host)
  (make-host
   (if (< (random 100) mutation-rate)
       (pick-random-receptor-type)
       (host-receptor host))
   #f ;; infected should always be false as infected
      ;; species can't generate new species (at the moment)
   ;; pick a new position on the edge of the parent species
   (circle-random-point-on-circumference (make-circle (host-location host) (host-size host)))
   host-size-start))

(define host-infection-factor 1)

;; calculate infection
;; optimise by prechecking susceptibility before checking infection present
(define (host-calculate-infection host virus infection-present)
  (or (host-infected? host)
      (and infection-present
           (< (rndf) (* (host-susceptibility host virus)
                        host-infection-factor)))))
 
(define (host-update host virus infection-present)
  (make-host
   (host-receptor host) ;; doesn't change
   (host-calculate-infection host virus infection-present)
   (host-location host) ;; can't move
   (host-calculate-size host)))

;; enforce an infection
(define (host-infect host)
  (make-host
   (host-receptor host) 
   #t
   (host-location host)
   (host-size host)))

(define (make-random-host)
  (make-host 
   (pick-random-receptor-type)
   #f
   (vector (random 100) (random 100))
   (+ host-size-start (random (- max-host-size host-size-start)))))

(define (host-print host)
  (when (host-infected? host) (display "X")(display " "))
  (display (host-receptor host))(display " ")
  (display (host-size host))(display " ")(display (host-location host))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main population

(define (make-random-population size)
  (build-list size (lambda (_) (make-random-host))))

(define (population-collect-new-species population)
  (foldl
   (lambda (host r)
     (if (host-can-branch-new-species? host)
         (cons (host-new-species host) r) r))
   '()
   population))

(define (population-is-infection-present? host population)
  (foldl
   (lambda (other-host infection)
     (if (and (not infection)
              (host-infected? other-host)
              (circle-intersect?
               (make-circle (host-location host) (host-size host))
               (make-circle (host-location other-host) (host-size other-host))))
         #t infection))
   #f
   population))

(define max-population-size 40)

(define (population-update population virus)
  (append
   (if (< (length population) max-population-size)
       (population-collect-new-species population) ;; add new species
       '())
   (filter
    host-alive? ;; remove dead host species
    (map
     (lambda (host)
       (host-update
        host virus
        (population-is-infection-present? host population))) ;; update species
     population))))

(define (population-infection-present? population)
  (foldl
   (lambda (host r)
     (if (and (not r) (host-infected? host)) #t r))
   #f
   population))

(define (population-print population)
  (for-each
   (lambda (host)
     (host-print host))
   population))

(define (population-infect-one population)
  (cons
   (host-infect (car population))
   (cdr population)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(define virus (make-virus (list 'triangle 'triangle 'triangle)))
(display virus)(newline)
(define population (population-infect-one (make-random-population 10)))
(population-print population)

(define (crank)
  (display "--")(newline)
  (set! population (population-update population virus))
  (population-print population))

(crank)

(crank)
(crank)
(crank)
(crank)

