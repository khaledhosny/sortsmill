;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

(define-module (ff-internal generate-types))

(import (ice-9 popen)
        (ice-9 match))

(export read-instructions-from-program-input
        read-instruction-sources
        read-instructions
        underscores->hyphens
        underscore->hyphen)

(define (read-instructions-from-program-input)
  (let* ((sources (cdr (command-line)))
         (instructions (if (null? sources)
                           (read-instruction-sources)
                           (read-instruction-sources sources))))
    instructions))

(define* (read-instruction-sources
          #:optional
          (sources (list (current-input-port)))
          (prior '()))
  (match sources
         (() prior)
         ((source . more-sources)
          (let ((instructions
                 (cond
                  ((string? source) (with-input-from-file source
                                      (lambda () (read-instructions))))
                  ((port? source) (read-instructions source)))))
            (read-instruction-sources more-sources
                                      (append prior instructions))))))

(define* (read-instructions
          #:optional
          (port (current-input-port))
          (prior '()))
  (let ((instruction (read port)))
    (if (eof-object? instruction)
        (reverse prior)
        (read-instructions port (cons instruction prior)))))

;; Convert ‘_’ to ‘-’, because hyphens are more conventional in
;; Scheme.
(define (underscores->hyphens arg)
  (match arg
         ((? string? s) (string-map underscore->hyphen s))
         ((? symbol? s) (string->symbol
                         (underscores->hyphens (symbol->string s))))
         ((a . b) (cons (underscores->hyphens a) (underscores->hyphens b)))
         (_ arg)))

(define (underscore->hyphen c)
  (if (char=? c #\_) #\- c))
