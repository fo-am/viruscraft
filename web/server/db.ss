;; Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket
(require (planet jaymccarthy/sqlite:5:1/sqlite))
(provide (all-defined-out))
(require "logger.ss")

(define (setup db)
  (exec/ignore db "CREATE TABLE virus ( id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age REAL, infections INTEGER, deaths INTEGER, jumps INTEGER, time TEXT);")
  )

(define (nuke db)
  (exec/ignore db "delete from virus"))

(define (table->csv db table)
  (define (list->csv l)
    (cond
     ((null? l) "")
     (else (string-append
            (if (string? (car l))
                (string-append "\"" (car l) "\"")
                (number->string (car l))) ", " (list->csv (cdr l))))))
  (define (rows->csv d)
    (cond
     ((null? d) "")
     (else (string-append (list->csv (vector->list (car d))) "\n" (rows->csv (cdr d))))))
  (let ((s (select db (string-append "select * from " table))))
    (if (null? s)
        ""
        (rows->csv s))))

(define (insert-virus db name age infections deaths jumps time)
  (insert db "insert into virus values (NULL, ?, ?, ?, ?, ?, ?)"
          name age infections deaths jumps time))

(define (get-hiscores db)
  (map
   (lambda (i)
     (list (vector-ref i 0) (vector-ref i 1)))
   (cdr (select db "select name, age, infections, deaths, jumps from virus order by age desc limit 100;"))))


