#!/usr//bin/env mzscheme
#lang scheme/base
;; Naked on Pluto Copyright (C) 2010 Aymeric Mansoux, Marloes de Valk, Dave Griffiths
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

(require scheme/system
         scheme/foreign
         scheme/cmdline
         web-server/servlet
         web-server/servlet-env
         web-server/http/response-structs
         racket/match
         "server/request.ss"
         "server/logger.ss"
         "server/json.ss"
         "server/db.ss")

(require (planet jaymccarthy/sqlite:5:1/sqlite))

; a utility to change the process owner,
; assuming mzscheme is called by root.
;;(unsafe!)
;;(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))

(define db-name "viruscraft.db")
(define db #f)

(if (file-exists? (string->path db-name))
    (begin
      (display "open existing db")(newline)
      (set! db (open (string->path db-name))))
    (begin
      (display "makine new db")(newline)
      (set! db (open (string->path db-name)))
      (setup db)))

(open-log "log.txt")

(define (pluto-response txt)
  (let ((p (response/full
   200                                  ; code
   #"Okay"                              ; message
   (current-seconds)                    ; seconds
   #"text/javascript"                   ; mime type
   '()                                  ; headers
   (list (string->bytes/utf-8 txt)))))   ; body
    (display p)(newline) p))

(define registered-requests
  (list
   (register
    (req 'ping '())
    (lambda (req)
      (pluto-response (scheme->json '("hello")))))

   (register
    (req 'virus '(name age infections deaths jumps time))
    (lambda (req name age infections deaths jumps time)
      (let* ((id (insert-virus db name age infections deaths jumps time)))
	(pluto-response (scheme->json (list id))))))
   
   (register
    (req 'hiscores '())
    (lambda (req)
      (pluto-response
       (scheme->json (get-hiscores db)))))

   (register
    (req 'scores '())
    (lambda (req)
      (pluto-response
       (scheme->json (get-scores db)))))

   ))

(define (start request)
  (let ((values (url-query (request-uri request))))
    (log (format "~a" values))
    (if (not (null? values)) ; do we have some parameters?
        (let ((name (assq 'fn values)))
          (if name ; is this a well formed request?
                (request-dispatch
                 registered-requests
                 (req (string->symbol (cdr name))
                      (filter
                       (lambda (v)
                         (not (eq? (car v) 'fn)))
                       values))
                 request)
                "error"))
        "hello")))

(printf "server is running...~n")

; Here we become the user 'nobody'.
; This is a security rule that *only works* if nobody owns no other processes
; than mzscheme. Otherwise better create another dedicated unprivileged user.
; Note: 'nobody' must own the state directory and its files.

;(setuid 65534)

;;

(serve/servlet
 start
 ;; port number is read from command line as argument
 ;; ie: ./server.scm 8080
;; #:listen-ip "10.42.0.1"
 #:listen-ip "localhost"
 #:port (string->number (command-line #:args (port) port))
 #:command-line? #t
 #:servlet-path "/game"
 #:server-root-path
 (build-path "client"))
