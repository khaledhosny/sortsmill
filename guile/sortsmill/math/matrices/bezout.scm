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

(library (sortsmill math matrices bezout)

  ;; See http://en.wikipedia.org/wiki/Bézout_matrix

  (export bezout-matrix
          bezout-resultant)

  (import (sortsmill math matrices base)
          (sortsmill math polyspline elev)
          (sortsmill i18n)
          (sortsmill core)
          (rnrs)
          (except (guile) error))

  (define/kwargs (bezout-matrix poly1 poly2 (sum +) (difference -) (product *))
    (let* ([n1 (row-matrix-size poly1)]
           [n2 (row-matrix-size poly2)])
      (assert (= n1 n2))
      (let* ([n (- n1 1)]
             [B (make-array *unspecified* n n)])
        (do ([i 1 (+ i 1)]) ([= n1 i])
          (do ([j 1 (+ j 1)]) ([< i j])
            ;; The following code avoids using the zero element of the
            ;; field.
            [let ([m (min i (- n1 j))]
                  [entry (difference (product (matrix-0ref poly1 0 j)
                                              (matrix-0ref poly2 0 (- i 1)))
                                     (product (matrix-0ref poly1 0 (- i 1))
                                              (matrix-0ref poly2 0 j)))])
              (do ([k 2 (+ k 1)]) ([< m k])
                (set! entry
                      (sum entry
                           (difference (product (matrix-0ref poly1 0 (+ j k -1))
                                                (matrix-0ref poly2 0 (- i k)))
                                       (product (matrix-0ref poly1 0 (- i k))
                                                (matrix-0ref poly2 0 (+ j k -1)))))))
              (matrix-1set! B i j entry)
              (when (not (= i j))
                ;; The matrix is symmetric.
                (matrix-1set! B j i entry)) ] ))
        B)))

  ;;----------------------------------------------------------------------
  ;;
  ;; Determinant routines. Not for general use. These assume exact
  ;; arithmetic.
  ;;
  ;; FIXME: TRY TO TAKE ADVANTAGE OF THE MATRIX’S SYMMETRY.

  (define (det2 B i1 i2 j1 j2 difference product)
    "Find the determinant of a 2x2 matrix."
    (difference (product (matrix-1ref B i1 j1) (matrix-1ref B i2 j2))
                (product (matrix-1ref B i1 j2) (matrix-1ref B i2 j1))))

  (define (det3 B sum difference product)
    "Find the determinant of a 3x3 matrix by Laplace expansion."
    (sum (difference (product (matrix-1ref B 1 1)
                              (det2 B 2 3 2 3 difference product))
                     (product (matrix-1ref B 1 2)
                              (det2 B 2 3 1 3 difference product)))
         (product (matrix-1ref B 1 3) (det2 B 2 3 1 2 difference product))))

  (define/kwargs (bezout-resultant matrix (sum +) (difference -) (product *))
    "Find the determinant of a Bézout matrix (the Bézout resultant),
assuming exact arithmetic. This procedure is not designed for general
use in finding a determinant. Returns #f if the Bézout ‘matrix’ is
0x0."
    (let ([n (car (matrix-dimensions matrix))])
      (case n
        [(0) #f]
        [(1) (matrix-1ref matrix 1 1)]
        [(2) (det2 matrix 1 2 1 2 difference product)]
        [(3) (det3 matrix sum difference product)]
        [else
         ;; We are unlikely to implicitize splines of degree greater
         ;; than 3, and thus there is no immediate need for more
         ;; general expansion of the determinant.
         (assertion-violation
          'bezout-resultant
          (_ "not implemented for matrices larger than 3x3")
          matrix)] )))
  
  ;;----------------------------------------------------------------------

  ) ;; end of library.
