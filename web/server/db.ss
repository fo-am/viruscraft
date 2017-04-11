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
  (exec/ignore db "CREATE TABLE player ( id INTEGER PRIMARY KEY AUTOINCREMENT, played_before INTEGER, age_range INTEGER)")
  (exec/ignore db "CREATE TABLE eaten ( id INTEGER PRIMARY KEY AUTOINCREMENT, player_id INTEGER, morph TEXT, toxic INTEGER, time_stamp INTEGER, game INTEGER, toxic_morph TEXT )")
  (exec/ignore db "CREATE TABLE morph ( id INTEGER PRIMARY KEY AUTOINCREMENT, texture_name TEXT, probability INTEGER, active INTEGER, can_be_toxic INTEGER, wing_shape INTEGER, type TEXT )")
  (exec/ignore db "CREATE TABLE player_name ( id INTEGER PRIMARY KEY AUTOINCREMENT, player_id INTEGER, player_name TEXT )")
  (exec/ignore db "create table hiscores ( id INTEGER PRIMARY KEY AUTOINCREMENT, player_id INTEGER, score real)")
  (exec/ignore db "create table game_params ( id INTEGER PRIMARY KEY AUTOINCREMENT, key TEXT, value TEXT)")
  )

(define (nuke db)
  (exec/ignore db "delete from player")
  (exec/ignore db "delete from eaten")
  ;;(exec/ignore db "delete from morph")
  (exec/ignore db "delete from player_name")
  (exec/ignore db "delete from hiscores"))

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

(define (insert-player db played_before age_range)
  (insert db "insert into player values (NULL, ?, ?)"
          played_before
          age_range))

(define (insert-player-name db player_id player_name)
  (log "player name " player_id " " player_name)
  (insert db "insert into player_name VALUES (NULL, ?, ?)"
          player_id player_name ))

(define (set-player-score db player-id score)
  (exec/ignore
   db "insert into hiscores values (NULL, ?, ?)"
   player-id score))

(define (get-hiscores db)
  (map
   (lambda (i)
     (list (vector-ref i 0) (vector-ref i 1)))
   (cdr (select db "select n.player_name, p.score from hiscores as p join player_name as n on p.player_id=n.player_id order by p.score desc limit 100;"))))

(define (insert-eaten db player_id morph toxic time_stamp game toxic_morph)
  (insert db "INSERT INTO eaten VALUES (NULL, ?, ?, ?, ?, ?, ?)"
          player_id morph toxic time_stamp game toxic_morph))

(define (insert-morph db texture_name probability active can_be_toxic wing_shape type)
  (insert db "INSERT INTO morph VALUES (NULL, ?, ?, ?, ?, ?, ?)"
          texture_name probability active can_be_toxic wing_shape type))

(define (update-morph db id probability active) ;can_be_toxic wing_shape
  (exec/ignore
   db "update morph set probability = ?, active = ? where id = ?"
   probability active id))

(define (delete-morph db id)
  (exec/ignore db "delete from morph where id = ?" id))

(define (get-morphs db type)
  (let ((s (select db "SELECT id, texture_name, probability, active, can_be_toxic, wing_shape from morph where type=?" type)))
    (if (null? s)
        '()
        (map
         (lambda (i)
           (list
            (vector-ref i 0)
            (vector-ref i 1)
            (vector-ref i 2)
            (vector-ref i 3)
            (vector-ref i 4)
            (vector-ref i 5)
            ))
         (cdr s)))))

;(define (get-player-averages db)
;  (let ((players (cdr (select db "SELECT * from player"))))
;    (filter
;     (lambda (av)
;       (not (false? av)))
;     (map
;      (lambda (player)
;        (get-player-average-min db (vector-ref player 0)))
;      players))))

(define (get-player-averages db)
  (map
   (lambda (i) (vector-ref i 0))
   (cdr (select db "SELECT score from player"))))

(define (get-player-average db player-id)
  (let ((v (cadr
            (select db (string-append
                        "SELECT avg(time_stamp), count(time_stamp) from click where success = 1 and player_id = "
                        (number->string player-id))))))
    (when (> (vector-ref v 1) 5)
          (exec/ignore
           db (string-append
               "UPDATE player SET score = "
               (number->string (vector-ref v 0))
               " where id = " (number->string player-id))))
    (vector-ref v 0)))

(define (get-player-count db player-id)
  (let ((v (cadr (select db (string-append
                             "SELECT count(time_stamp) from click where success = 1 and player_id = "
                             (number->string player-id))))))
    (vector-ref v 0)))

(define (get-position v ol)
  (define (_ n l)
    (cond
      ((null? l) n)
      ((> (car l) v) n)
      (else (_ (+ n 1) (cdr l)))))
  (_ 1 ol))

(define (get-player-rank db av)
  (if av
      (let ((rank (sort (get-player-averages db) <)))
        (get-position av rank))
      999))

(define (set-game-param db key value)
  ;; lots of arguments about upsert - do it the long way
  (let ((s (select db "select * from game_params where key = ?" key) ))
    (if (null? s)
        (insert db "insert into game_params values (NULL, ?, ?)" key value)
        (exec/ignore
         db "update game_params set value = ? where key = ?" value key))))

(define (get-game-param db key value)
  (let ((s (select db "select value from game_params where key=?" key)))
    (if (null? s)
        (begin (set-game-param db key value) value)
        (vector-ref (car (cdr s)) 0))))

(define (get-game-params db)
  (let ((s (select db "select key, value from game_params")))
    (if (null? s) '() (map vector->list (cdr s)))))
