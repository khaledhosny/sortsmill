;; -*- mode: bee; coding: utf-8 -*-

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

(define-module (ff-internal generate-types))

(use-modules
   (ice-9 popen)
   (ice-9 match)
   )

(export
   read-instruction-sources
   read-instructions
   underscores->hyphens
   underscore->hyphen
   )

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
      ((a . b) (cons (underscores->hyphens a) (underscores->hyphens b)))
      (_ arg)))

(define (underscore->hyphen c)
   (if (char=? c #\_) #\- c))
