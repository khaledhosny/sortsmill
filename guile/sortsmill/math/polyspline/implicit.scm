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

(library (sortsmill math polyspline implicit)

  (export poly:bezout-matrix
          poly:implicit-equation
          poly:pretty-print-implicit-equation)

  (import (sortsmill math multivariate-polynomials)
          (sortsmill math matrices)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) iota))

;;;;; FIXME: Will this be needed?
  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_implicit"))

  ;;----------------------------------------------------------------------

  (define (poly:bezout-matrix xspline yspline)
    "Make the Bézout matrix for implicitization of a parametric planar
Bézier spline, given in monomial basis."
    (let ([xspline (zero-based (row-matrix->vector (matrix-inexact->exact xspline)))]
          [yspline (zero-based (row-matrix->vector (matrix-inexact->exact yspline)))])
      (assert (= (vector-length xspline) (vector-length yspline)))
      (bezout-matrix (x-xspline xspline) (y-yspline yspline)
                     #:sum multipoly+ #:difference multipoly-
                     #:product multipoly*)))

  (define (x-xspline xspline)
    (let* ([n (vector-length xspline)]
           [sp (make-vector n)])
      (do ([i 0 (+ i 1)]) ([= i n])
        (vector-set! sp i (make-array 0 2 2))
        (array-set! (vector-ref sp i) (- (vector-ref xspline i)) 0 0))
      (array-set! (vector-ref sp 0) 1 1 0) ; Represents ‘x’.
      sp))

  (define (y-yspline yspline)
    (let* ([n (vector-length yspline)]
           [sp (make-vector n)])
      (do ([i 0 (+ i 1)]) ([= i n])
        (vector-set! sp i (make-array 0 2 2))
        (array-set! (vector-ref sp i) (- (vector-ref yspline i)) 0 0))
      (array-set! (vector-ref sp 0) 1 0 1) ; Represents ‘y’.
      sp))

  (define (poly:implicit-equation matrix)
    "Given the Bézout matrix made by poly:bezout-matrix, find its
determinant (the Bézout resultant), which is the implicit equation of
the spline."
    (bezout-resultant matrix #:sum multipoly+ #:difference multipoly-
                      #:product multipoly*))

  (define (poly:pretty-print-implicit-equation eq)
    "Not yet really implemented."
    (let ([terms (poly:pretty-print-implicit-equation-terms eq)])
      terms))

  (define (poly:pretty-print-implicit-equation-terms eq)
    (assert (apply = (matrix-dimensions eq)))
    (let ([n (car (matrix-dimensions eq))])
      (fold-left
       (lambda (prior degree)
         (cons (fold-left
                (lambda (prior^ i)
                  (let ([j (- degree i)])
                    (cons (array-ref eq i j) prior^)))
                '()
                (iota (+ degree 1) 0))
          prior))
       '()
       (iota n (- n 1) -1))))

  ;;----------------------------------------------------------------------

  ) ;; end of library.
