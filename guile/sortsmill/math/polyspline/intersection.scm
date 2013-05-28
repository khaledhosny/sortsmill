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

(library (sortsmill math polyspline intersection)

  (export poly:intersect-with-line-mono)

  (import (sortsmill math matrices)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (define (poly:intersect-with-line-mono xspline yspline line)
    (match line
      [(C A B) (let* ([Ax (vector-map (lambda (t) (* A t)) (row-matrix->vector xspline))]
                      [By (vector-map (lambda (t) (* B t)) (row-matrix->vector yspline))]
                      [n (vector-length Ax)]
                      [Ax+By+C (make-vector n)])
                 (vector-set! Ax+By+C 0 (+ (vector-ref Ax 0) (vector-ref By 0) C))
                 (do ([i 1 (1+ i)]) ([= i n])
                   (vector-set! Ax+By+C i (+ (vector-ref Ax i) (vector-ref By i))))
                 Ax+By+C)]
      [other (assertion-violation 'poly:intersect-with-line-mono
                                  (_ "expected (C A B) for the line Ax+By+C=0")
                                  other)] ))

  ) ;; end of library.
