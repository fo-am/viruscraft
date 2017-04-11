#lang racket
(define (string-split str . rest)
  ; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (add1 i) yet-to-split-count))
        (else (scan-beg-word (add1 i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i)
            (skip-ws (add1 i) (- yet-to-split-count 1))))
        (else (scan-word (add1 i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

  ;; maxsplit is a positive number
  ;; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i)
            (scan-beg-word (add1 i) (- yet-to-split-count 1))))
        (else (scan-word (add1 i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

  ;; resolver of overloading...
  ;; if omitted, maxsplit defaults to
  ;; (inc (string-length str))
  (if (eq? (string-length str) 0) '()
    (if (null? rest)
      (split-by-whitespace str (add1 (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (add1 (string-length str)))))
        (cond
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)

(define (trim-front str)
  (define (_ i)
    (cond
      ((>= i (string-length str)) "")
      ((char-whitespace? (string-ref str i)) (_ (+ i 1)))
      (else (substring str i (string-length str)))))
  (_ 0))

(define (trim-end str)
  (define (_ i)
    (cond
      ((< i 0) "")
      ((char-whitespace? (string-ref str i)) (_ (- i 1)))
      (else (substring str 0 (+ i 1)))))
  (_ (- (string-length str) 1)))

(define (trim str)
  (trim-front (trim-end str)))

;(define (convert str)
;  (cond
;    ((char=? (string-ref str 0) #\") (substring str 1 (- (string-length str) 1)))
;    ((string->number str) (string->number str))
;    (else (string->symbol str))))

(define (convert str)
  (cond
   ((char=? (string-ref str 0) #\") (substring str 1 (- (string-length str) 1)))
   (else str)))

(define csv "\"test-num\",1,1,1,\" \"\n\"one\",34,\"32\", one two, \n \"three four\", 4, 5" )

(define (csv->list csv)
  (map
   (lambda (line)
     (foldl
      (lambda (cell r)
        (if (or (equal? cell "") (equal? cell " "))
            r (append r (list (convert (trim cell))))))
      '()
      (string-split line '(#\,))))
   (string-split csv '(#\newline))))

(display "(define i18n-text (list") (newline)
(display (foldl
          (lambda (l r)
            (if (null? l) r
                (string-append
                 r
                 "(list \"" (car l) "\" (list "
                 (apply string-append
                        (map
                         (lambda (s) (string-append "\"" (trim s) "\" "))
                         (cdr l)))
                 "))\n" )))
          ""
          (csv->list (file->string "translations.csv"))))
(display "))") (newline)

;(csv->list csv)

;(word-gen)

;------------------------------------------------------

(define (list->string l)
  (foldl
   (lambda (l r)
     (string-append r ", \"" l "\""))
   ""
   l))

(define (list->csv l)
  (foldl
   (lambda (l r)
     (string-append r (symbol->string (car l)) (list->string (cadr l)) "\n"))
   ""
   l))


(define i18n-text
  (list



   ))


;(display (list->csv i18n-text))
