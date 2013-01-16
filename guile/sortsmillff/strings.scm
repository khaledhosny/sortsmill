;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2013 Barry Schwartz
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

(define-module (sortsmillff strings))

(export enable-hash-guillemet-strings
        disable-hash-guillemet-strings)

(define (read-chars-until s port)
  (letrec ((read-more
            (lambda (prior)
              (let* ((c (read-char port))
                     (new-string (string-append prior (string c))))
                (if (string-suffix? s new-string)
                    (string-drop-right new-string (string-length s))
                    (read-more new-string))))))
    (read-more "")))

(define (enable-hash-guillemet-strings)
  (read-hash-extend #\« (lambda (c port)
                          (read-chars-until "»#" port))))

(define (disable-hash-guillemet-strings)
  (read-hash-extend #\« #f))
