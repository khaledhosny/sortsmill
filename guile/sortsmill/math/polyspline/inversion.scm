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
          (sortsmill math polyspline roots)
          (sortsmill math matrices)
          (sortsmill math math-constants)
          (sortsmill kwargs)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (define/kwargs (poly:invert-spline-mono xspline yspline t1 t2 x y)
    "Given a spline in monomial basis, and a point (x,y), try to find
the values of the parameter t that correspond to (x,y) and lie in the
interval [t1,t2]. The results, which are returned as a list, in
general are approximate and not confined within brackets, so you may
want to set [t1,t2] a little wide. If the spline effectively is a
point, then the symbol @code{point} is returned instead of a list of t
values."
    (assert (<= t1 t2))
    (let* ([min-degree (max (poly:min-degree-scm-mono xspline)
                            (poly:min-degree-scm-mono yspline))]
           [xspline (poly:reduce-degree-scm-mono xspline min-degree)]
           [yspline (poly:reduce-degree-scm-mono yspline min-degree)])
      (case min-degree
        [(0) (invert-point)]
        [(1) (invert-linear xspline yspline t1 t2 x y)]
        [(2) (invert-quadratic xspline yspline t1 t2 x y)]
        [(3) (invert-cubic xspline yspline t1 t2 x y)]
        [else (assertion-violation
               'poly:invert-spline-mono
               (_ "inversion is not implemented for splines of degree greater than 3")
               xspline yspline)])))

  (define (invert-point)
    "‘Invert’ a spline that effectively is degenerated to a point. But
do not actually invert it; instead return a symbol that indicates the
situation."
    (quote point))

  (define (invert-linear xspline yspline t1 t2 x y)
    "Invert a linear spline given in monomial basis."
    ;; Draw a horizontal or vertical line through (x,y) and catch its
    ;; intersection with the spline. Measure how far along the spline
    ;; that intersection lies.
    (let* ([x0 (- (matrix-0ref xspline 0 0) x)]
           [y0 (- (matrix-0ref yspline 0 0) y)]
           [x1 (matrix-0ref xspline 0 1)]
           [y1 (matrix-0ref yspline 0 1)])
      (let ([root (if (< (abs x1) (abs y1))
                      (- (/ y0 y1))
                      (- (/ x0 x1)))])
        (if (<= t1 root t2)
            (list root)
            '()))))

  (define (invert-quadratic xspline yspline t1 t2 x y)
    ;; Construct a Bézout matrix and compute its singular value
    ;; decomposition.
    (let ([x-xspline (poly:sub-scm-mono (scalar->matrix x) xspline)]
          [y-yspline (poly:sub-scm-mono (scalar->matrix y) yspline)])
      (let ([B (bezout-matrix x-xspline y-yspline)])
        (let-values ([(U S V) (f64matrix-svd (matrix->f64matrix B))])
          (case (matrix-svd-effective-rank S)
            [(0) (invert-rank0 xspline yspline t1 t2 x y)]
            [(1 2)
             ;; If the effective rank is 2, assume we have a lot of
             ;; round-off and the rank really is 1.
             (invert-quadratic-rank1 V t1 t2)])))))

  (define (invert-rank0 xspline yspline t1 t2 x y)
    (let ([degree  (- (row-matrix-size xspline) 1)])
      (if (< (abs (matrix-0ref xspline 0 degree))
             (abs (matrix-0ref yspline 0 degree)))
          [let ([y-yspline (matrix-inexact->exact
                            (poly:sub-scm-mono (scalar->matrix y) yspline))])
            (poly:find-roots-scm-mono y-yspline t1 t2)]
          [let ([x-xspline (matrix-inexact->exact
                            (poly:sub-scm-mono (scalar->matrix x) xspline))])
            (poly:find-roots-scm-mono x-xspline t1 t2)] )))

  (define (invert-quadratic-rank1 V t1 t2)
    "The right column of V is a basis of the null space of the Bézout
matrix, and thus (for our definition of that matrix) is proportional
to the transpose of @code{#(1 t)}. (Other definitions of the Bézout
matrix may vary by a scalar factor or in the order of entries.)"
    (let ([root (/ (matrix-1ref V 2 2) (matrix-1ref V 1 2))])
      (if (<= t1 root t2)
          (list root)
          '())))

  (define (invert-cubic xspline yspline t1 t2 x y)
    ;; Construct a Bézout matrix and compute its singular value
    ;; decomposition.
    (let ([x-xspline (poly:sub-scm-mono (scalar->matrix x) xspline)]
          [y-yspline (poly:sub-scm-mono (scalar->matrix y) yspline)])
      (let ([B (bezout-matrix x-xspline y-yspline)])
        (let-values ([(U S V) (f64matrix-svd (matrix->f64matrix B))])
          (case (matrix-svd-effective-rank S)
            [(0) (invert-rank0 xspline yspline t1 t2 x y)]
            [(1) (invert-cubic-rank1 V t1 t2)]
            [(2 3)
             ;; If the effective rank is 3, assume we have a lot of
             ;; round-off and the rank really is 2.
             (invert-cubic-rank2 V t1 t2)] )))))

  (define (invert-cubic-rank1 V t1 t2)
    "Solve (v₁₁ v₁₂ v₁₃)⋅(1 t t²)ᵀ = 0."
    (let ([poly (matrix->matrix (matrix-row V 1))])
      (poly:find-roots-scm-mono poly t1 t2)))

  (define (invert-cubic-rank2 V t1 t2)
    "The right column of V is a basis of the null space of the Bézout
matrix, and thus (for our definition of that matrix) is proportional
to the transpose of @code{#(1 t t²)}. (Other definitions of the Bézout
matrix may vary by a scalar factor or in the order of entries.)"
    (let ([root (/ (matrix-1ref V 2 3) (matrix-1ref V 1 3))])
      (if (<= t1 root t2)
          (list root)
          '())))

  ) ;; end of library.