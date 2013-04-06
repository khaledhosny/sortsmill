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

(library (sortsmill psmat)

  (export psmat:compose
          psmat:inverse
          psmat:rotate
          psmat:scale
          psmat:skew
          psmat:translate

          ;; Unlike its Python equivalent, psmat:identity is a
          ;; variable equal to the identity, rather than a procedure
          ;; returning the identity.
          psmat:identity)

  (import (sortsmill math matrices)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (define psmat:identity '(1 0 0 1 0 0))

  (define (compose-two-matrices mat1 mat2)
    (match mat1
      [(m10 m11 m12 m13 m14 m15)
       (match mat2
         [(m20 m21 m22 m23 m24 m25)
          (let ([a0 (+ (* m10 m20) (* m11 m22))]
                [a1 (+ (* m10 m21) (* m11 m23))]
                [a2 (+ (* m12 m20) (* m13 m22))]
                [a3 (+ (* m12 m21) (* m13 m23))]
                [a4 (+ (* m14 m20) (* m15 m22) m24)]
                [a5 (+ (* m14 m21) (* m15 m23) m25)])
            (list a0 a1 a2 a3 a4 a5))] )] ))

  (define psmat:compose
    (case-lambda
      [(mat1)        mat1]
      [(mat1 mat2)   (compose-two-matrices mat1 mat2)]
      [(mat1 . rest) (fold-left compose-two-matrices mat1 rest)] ))

  (define (psmat:inverse mat)
    (match mat
      [(m0 m1 m2 m3 m4 m5)
       (let* ([A (list->array 2 `((,m0 ,m1)
                                  (,m2 ,m3)))]
              [inv-A (matrix-inv A)]
              [n0 (matrix-0ref inv-A 0 0)]
              [n1 (matrix-0ref inv-A 0 1)]
              [n2 (matrix-0ref inv-A 1 0)]
              [n3 (matrix-0ref inv-A 1 1)]
              [n4 (- (+ (* m4 n0) (* m5 n2)))]
              [n5 (- (+ (* m4 n1) (* m5 n3)))])
         (list n0 n1 n2 n3 n4 n5))] ))

  (define (psmat:rotate theta)
    (let ([c (cos theta)]
          [s (sin theta)])
      (list c s (- s) c 0 0)))

  (define psmat:scale
    (case-lambda
      [(x)   (list x 0 0 x 0 0)]
      [(x y) (list x 0 0 y 0 0)] ))

  (define (psmat:skew theta)
    (list 1 0 (tan theta) 1 0 0))

  (define (psmat:translate x y)
    (list 1 0 0 1 x y))

  ) ;; end of library.
