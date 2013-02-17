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

(library (sortsmill argv)

  (export string-list->argv-and-argc)

  (import (sortsmill machine)
          (rnrs)
          (only (srfi :1) iota)
          (system foreign))

  ;; The argv is returned first because it is the more likely value to
  ;; be needed.
  (define (string-list->argv-and-argc args)
    (let* ([n (length args)]
           [pointer-size (sizeof '*)]
           [num-bytes (* pointer-size (+ 1 n))]
           [argv (make-bytevector num-bytes 0)])
      (for-each
       (lambda (i s)
         (bytevector-pointer-native-set! argv (* i pointer-size)
                                         (string->pointer s)))
       (iota n) args)
      (values argv n)))

  ) ;; end of library.
