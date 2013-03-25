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

(library (sortsmill math polyspline inversion)

  (export poly:invert-spline-mono)

  (import (sortsmill math polyspline add)
          (sortsmill math polyspline eval)
          (sortsmill math polyspline reduce)
          (sortsmill math matrices)
          (sortsmill math math-constants)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error))

  (define (poly:invert-spline-mono xspline yspline x y)
    "Given a spline in monomial basis, and a point (x,y), try to find
the values of the parameter t corresponding to (x,y). The results,
which are returned as a list, are approximate in general, may fall
outside [0,1]. If the spline effectively is a point, then the symbol
@code{point} is returned instead of a list of t values."
    (let* ([min-degree (max (poly:min-degree-scm-mono xspline)
                            (poly:min-degree-scm-mono yspline))]
           [xspline (poly:reduce-degree-scm-mono xspline min-degree)]
           [yspline (poly:reduce-degree-scm-mono yspline min-degree)])
      (case min-degree
        [(0) (invert-point)]
        [(1) (invert-linear xspline yspline x y)]
        [(2) (invert-quadratic xspline yspline x y)]
        [(3) (invert-cubic xspline yspline x y)]
        [else (assertion-violation
               'poly:invert-spline-mono
               (_ "inversion is not implemented for splines of degree greater than 3")
               xspline yspline)])))

  (define (invert-point)
    "‘Invert’ a spline that effectively is degenerated to a point. But
do not actually invert it; instead return a symbol that indicates the
situation."
    (quote point))

  (define (invert-linear xspline yspline x y)
    "Invert a linear spline given in monomial basis."
    ;; Draw a horizontal or vertical line through (x,y) and catch its
    ;; intersection with the spline. Measure how far along the spline
    ;; that intersection lies.
    (let* ([xspline (zero-based (row-matrix->vector xspline))]
           [yspline (zero-based (row-matrix->vector yspline))]
           [x0 (- (vector-ref xspline 0) x)]
           [y0 (- (vector-ref yspline 0) y)]
           [x1 (vector-ref xspline 1)]
           [y1 (vector-ref yspline 1)])
      (if (< (abs x1) (abs y1))
          (- (/ y0 y1))
          (- (/ x0 x1)))))

  (define (invert-quadratic xspline yspline x y)
    (assertion-violation 'invert-quadratic
                         "not yet implemented"))
    #|
    ;; Construct a Bézout matrix and find its null space.
    (let ([x-xspline (zero-based
                      (row-matrix->vector
                       (poly:sub-scm-mono (make-vector 1 x) xspline)))]
          [y-yspline (zero-based
                      (row-matrix->vector
                       (poly:sub-scm-mono (make-vector 1 y) yspline)))])
      (let ([B (bezout-matrix x-xspline y-yspline)])
        B)))
    |#

  (define (invert-cubic xspline yspline x y)
    (assertion-violation 'invert-cubic
                         "not yet implemented"))

  ) ;; end of library.
