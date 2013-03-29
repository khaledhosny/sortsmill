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
          poly:pretty-print-bezout-matrix
          poly:implicit-equation
          poly:pretty-print-implicit-equation
          poly:plug-into-implicit-equation)

  (import (sortsmill math polyspline add)
          (sortsmill math polyspline mul)
          (sortsmill math multivariate-polynomials)
          (sortsmill math matrices)
          (sortsmill dynlink)
          (sortsmill kwargs)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) iota list-tabulate take zip)
          (ice-9 format)
          (ice-9 match))

;;;;; FIXME: Will this be needed?
  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_implicit"))

  ;;----------------------------------------------------------------------

  (define/kwargs (poly:bezout-matrix xspline yspline (allow-inexact? #f))
    "Make the Bézout matrix for implicitization of a parametric planar
Bézier spline, given in monomial basis."
    (let* ([force-exact (if allow-inexact? identity matrix-inexact->exact)]
           [xspline (matrix-0based (row-matrix->vector (force-exact xspline)))]
           [yspline (matrix-0based (row-matrix->vector (force-exact yspline)))])
      (assert (= (vector-length xspline) (vector-length yspline)))
      (let ([deg-max (max (max-degree xspline) (max-degree yspline))])
        (let ([xspline (make-shared-array xspline list (+ deg-max 1))]
              [yspline (make-shared-array yspline list (+ deg-max 1))])
          (bezout-matrix (x-xspline xspline) (y-yspline yspline)
                         #:sum multipoly+ #:difference multipoly-
                         #:product multipoly*)))))

  (define (poly:pretty-print-bezout-matrix B)
    (let* ([B (matrix-1based B)]
           [n (car (matrix-dimensions B))]
           [strings
            (fold-left
             (lambda (i-prior i)
               (cons
                 (fold-left
                  (lambda (j-prior j)
                    (cons
                     (multipoly:pretty-print (array-ref B i j))
                     j-prior))
                  '()
                  (iota n 1))
                 i-prior))
             '()
             (iota n 1))]
           [max-length (apply max (map string-length (apply append strings)))]
           [column-width (+ max-length 3)])
      (string-append
       (string-join
        (map
         (lambda (row)
           (fold-left
            (lambda (row-so-far entry)
              (format #f "~a~va" row-so-far column-width entry))
            ""
            row))
         strings)
        "\n")
       "\n")))
        

  (define (max-degree spline)
    (let* ([n (vector-length spline)]
           [deg (- n 1)])
      (cond [(= deg -1) 0]
            [(zero? (vector-ref spline deg))
             (max-degree (make-shared-array spline list deg))]
            [else deg])))

  (define (x-xspline xspline)
    (let* ([n (vector-length xspline)]
           [sp (make-vector n)]
           [deg0-entry (make-array 0 2 2)])
      (array-set! deg0-entry (- (vector-ref xspline 0)) 0 0)
      (array-set! deg0-entry 1 1 0)     ; Represents ‘x’.
      (vector-set! sp 0 deg0-entry)
      (do ([i 1 (+ i 1)]) ([= i n])
        (vector-set! sp i (make-array (- (vector-ref xspline i)) 1 1)))
      sp))

  (define (y-yspline yspline)
    (let* ([n (vector-length yspline)]
           [sp (make-vector n)]
           [deg0-entry (make-array 0 2 2)])
      (array-set! deg0-entry (- (vector-ref yspline 0)) 0 0)
      (array-set! deg0-entry 1 0 1)     ; Represents ‘y’.
      (vector-set! sp 0 deg0-entry)
      (do ([i 1 (+ i 1)]) ([= i n])
        (vector-set! sp i (make-array (- (vector-ref yspline i)) 1 1)))
      sp))

  (define (poly:implicit-equation matrix)
    "Given the Bézout matrix made by poly:bezout-matrix, find its
determinant (the Bézout resultant), which is the implicit equation of
the spline. Returns #f if there is no implicit equation because the
curve is degenerated to a point."
    (bezout-resultant matrix #:sum multipoly+ #:difference multipoly-
                      #:product multipoly*))

  (define (poly:pretty-print-implicit-equation eq)
    (format #f "~a = 0" (multipoly:pretty-print eq)))

  ;;----------------------------------------------------------------------

  (define (poly:plug-into-implicit-equation eq x y)
    (let* ([eq (if (array? eq) (multipoly:by-degrees eq) eq)]
           [n (length eq)]
           [max-degree (- n 1)]
           [powers-of-x (powers-of-polynomial x max-degree)]
           [powers-of-y (powers-of-polynomial y max-degree)])
      (fold-left (lambda (prior coefs-of-degree)
                   (poly:add-scm-mono prior
                                      (plug-into-degree coefs-of-degree
                                                        powers-of-x
                                                        powers-of-y)))
                 #(0) eq)))

  (define (plug-into-degree coefs-of-degree powers-of-x powers-of-y)
    (let ([n (length coefs-of-degree)])
      (fold-left poly:add-scm-mono
                 #(0)
                 (map poly:mul-scm-mono
                      (map matrix* coefs-of-degree (take powers-of-x n))
                      (reverse (take powers-of-y n))))))

  (define (powers-of-polynomial p max-degree)
    (assert (<= 0 max-degree))
    (if (= max-degree 0)
        (list #(1))
        (reverse (fold-left (lambda (prior i)
                              (cons (poly:mul-scm-mono p (car prior))
                                    prior))
                            (list p #(1))
                            (iota (- max-degree 1))))))
  
  ;;----------------------------------------------------------------------

  ) ;; end of library.
