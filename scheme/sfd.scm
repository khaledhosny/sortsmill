;; -*- coding: utf-8 -*-

;; Copyright (C) 2012 Barry Schwartz
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(define-module (sortsmill-fontforge sfd)
  #:use-module (system base lalr)
  #:export (sfd-test))

;; Like peek-char but skips past backslash-newline.
(define (sfd:peek-char input)
  (let ((c (peek-char input)))
    (if (char=? #\\ c)
        (begin
          (read-char input)
          (if (char=? #\newline (peek-char input))
              (begin
                (read-char input)
                (sfd:peek-char input))
              (begin
                (unread-char c)
                c)))
        c)))

;; Like peek-char but skips past backslash-newline.
(define (sfd:peek-char input)
  (let ((c (peek-char input)))
    (if (char=? #\\ c)
        (begin
          (read-char input)
          (if (char=? #\newline (peek-char input))
              (begin
                (read-char input)
                (sfd:peek-char input))
              (begin
                (unread-char c)
                c)))
        c)))

(define (sfd:skip-whitespace input)
  (if (char-whitespace? (sfd:peek-char input))
      (begin
        (read-char input)
        (sfd:skip-whitespace input))))

(define sfd:special-chars (string->char-set "[]{}<%"))

(define (sfd:read-keyword input-name input)
  (letrec ((scan-string
            (lambda (s)
              (let ((c (peek-char input)))
                (cond ((or (eof-object? c)
                           (char-whitespace? c)
                           (char-set-contains? sfd:special-chars c))
                       s)
                      (else
                       (read-char input)
                       (scan-string (string-append s (string c)))))))))
    (sfd:skip-whitespace input)
    (scan-string "")))

;(define (fontforge-sfd-read input-name input)
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  (display (read-char input))
;  (display (make-source-location input-name (port-line input) (port-column input) -1 -1))
;  )

(define (sfd-test)
  (let ((input-file "/home/trashman/src/sortsmill/fanwood/Fanwood-Italic.sfd"))
    (with-input-from-file input-file
      (lambda ()
        (write (sfd:read-keyword input-file (current-input-port)))
        (write (sfd:read-keyword input-file (current-input-port)))
        (write (sfd:read-keyword input-file (current-input-port)))
        (write (sfd:read-keyword input-file (current-input-port)))
        (write (sfd:read-keyword input-file (current-input-port)))
        (write (sfd:read-keyword input-file (current-input-port)))
        ))))
