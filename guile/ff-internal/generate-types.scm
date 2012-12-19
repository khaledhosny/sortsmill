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
   )

(define (read-instruction-sources sources)
   (match sources
      (() *unspecified*)
      ((h . t)
       (let ((port (open-input-pipe h)))
          (read-instructions port)
          (close-pipe port)
          (read-instruction-sources t)))))

(define (read-instructions port)
   (do ((instruction (read port) (read port)))
       ((eof-object? instruction))
       (write (list 'define-ff:interface
                 (underscores->hyphens instruction)))
       (newline)))

;; Convert ‘_’ to ‘-’, because hyphens are more conventional in
;; Scheme. (Maybe we should generate BOTH forms, though probably not.)
(define (underscores->hyphens instruction)
   (map (lambda (s) (if (string? s) (string-map underscore->hyphen s) s))
      instruction))

(define (underscore->hyphen c)
   (if (char=? c #\_) #\- c))
