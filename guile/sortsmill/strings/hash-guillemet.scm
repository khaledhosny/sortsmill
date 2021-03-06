;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2013 Khaled Hosny and Barry Schwartz
;; This file is part of the Sorts Mill Tools.
;; 
;; Sorts Mill Tools is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Sorts Mill Tools is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sortsmill strings hash-guillemet)
  
  (export enable-hash-guillemet-strings
          disable-hash-guillemet-strings)

  (import (rnrs)
          (except (guile) error))

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

  (enable-hash-guillemet-strings)

  ) ;; end of library.
