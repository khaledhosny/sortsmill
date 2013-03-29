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

(library (sortsmill math geometry lines)

  (export line:implicit-equation
          line:implicit-equation-mono
          line:implicit-equation-bern
          line:implicit-equation-sbern
          line:implicit-equation-spower

          line:normal-through-point)

  (import (sortsmill math matrices)
          (sortsmill kwargs)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (define* (line:implicit-equation #:key x0 x1 y0 y1)
    "Find an implicit equation for a line. No normalization is
done. If the equation is

     Ax + By + C = 0

then the values are returned as the list (C A B).  (This order is
chosen to be consistent with how bivariate polynomials are represented
elsewhere in Sorts Mill Tools.)"
    (let* ([A (- y1 y0)]
           [B (- x0 x1)]
           [C (- (+ (* A x0) (* B y0)))])
      (list C A B)))

  (define (line:implicit-equation-mono xspline yspline)
    "Find an implicit equation for a line given as a degree-one spline
in monomial basis."
    (let* ([xspline (matrix-0based (row-matrix->vector xspline))]
           [yspline (matrix-0based (row-matrix->vector yspline))])
      (assert (= 2 (vector-length xspline)))
      (assert (= 2 (vector-length yspline)))
      (let ([x0 (vector-ref xspline 0)]
            [y0 (vector-ref yspline 0)])
        (line:implicit-equation #:x0 x0 #:y0 y0
                                #:x1 (+ x0 (vector-ref xspline 1))
                                #:y1 (+ y0 (vector-ref yspline 1))))))

  (define (line:implicit-equation-bern xspline yspline)
    "Find an implicit equation for a line given as a degree-one spline
in Bernstein basis."
    (let* ([xspline (matrix-0based (row-matrix->vector xspline))]
           [yspline (matrix-0based (row-matrix->vector yspline))])
      (assert (= 2 (vector-length xspline)))
      (assert (= 2 (vector-length yspline)))
      (line:implicit-equation #:x0 (vector-ref xspline 0)
                              #:y0 (vector-ref yspline 0)
                              #:x1 (vector-ref xspline 1)
                              #:y1 (vector-ref yspline 1))))

  (define (line:implicit-equation-sbern xspline yspline)
    "Find an implicit equation for a line given as a degree-one spline
in scaled Bernstein basis."
    (line:implicit-equation-bern xspline yspline))

  (define (line:implicit-equation-spower xspline yspline)
    "Find an implicit equation for a line given as a degree-one spline
in s-power basis."
    (line:implicit-equation-bern xspline yspline))

  (define/kwargs (line:normal-through-point implicit-equation x y)
    "Given (C A B) representing an implicit equation Ax + By + C = 0,
and a point (x,y), find an implicit equation of the line through (x,y)
normal to the given line."
    (match implicit-equation
      [(C A B)
       (let* ([D B]
              [E (- A)]
              [F (- (+ (* D x) (* E y)))])
         (list F D E))]
      [_ (assertion-violation
          'line:normal-through-point
          (_ "expected (C A B) representing the line Ax + By + C = 0")
          implicit-equation)]))

  ) ;; end of library.
