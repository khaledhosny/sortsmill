;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Barry Schwartz
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

(library (sortsmill matrices)

  (export not-a-matrix

          zero-based
          one-based

          matrix-shape
          matrix-dimensions
          conformable-for*?
          conformable-for+?

          matrix-row
          matrix-column-transpose

          vector->matrix

          f64matrix-f64matrix*
          f64matrix-f64matrix+
          f64matrix-f64matrix-
          f64matrix*
          f64matrix+
          f64matrix-

          matrix*
          matrix+
          matrix-)          

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (srfi :4)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_matrices"))

  (define (not-a-matrix caller irritant . more-irritants)
    (if (null? more-irritants)
        (assertion-violation caller
                             (_ "expected a vector or matrix")
                             irritant)
        (apply assertion-violation caller
               (_ "expected vectors or matrices")
               irritant more-irritants)))

  (define (zero-based A)
    (match (array-shape A)
      [[(lo hi)]
       (make-shared-array A
                          (lambda (i) `[,(+ i lo)])
                          `[0 ,(- hi lo)] )]
      [[(lo1 hi1) (lo2 hi2)]
       (make-shared-array A
                          (lambda (i j) `[,(+ i lo1) ,(+ j lo2)])
                          `[0 ,(- hi1 lo1)] `[0 ,(- hi2 lo2)] )]
      [_ (not-a-matrix 'zero-based A)]))

  (define (one-based A)
    (match (array-shape A)
      [[(lo hi)]
       (make-shared-array A
                          (lambda (i) `[,(+ i lo -1)])
                          `[1 ,(- hi lo -1)] )]
      [[(lo1 hi1) (lo2 hi2)]
       (make-shared-array A
                          (lambda (i j) `[,(+ i lo1 -1) ,(+ j lo2 -1)])
                          `[1 ,(- hi1 lo1 -1)] `[1 ,(- hi2 lo2 -1)] )]
      [_ (not-a-matrix 'one-based A)]))

  (define (vector->matrix v)
    (if (= (array-rank v) 1)
        (match (array-shape v)
          [((lo hi))
           (make-shared-array v (lambda (i j) `[,j]) `[,lo ,lo] `[,lo ,hi])])
        v))

  (define (f64matrix* A B)
    (f64matrix-f64matrix* (vector->matrix A) (vector->matrix B)))

  (define (f64matrix+ A B)
    (f64matrix-f64matrix+ (vector->matrix A) (vector->matrix B)))

  (define (f64matrix- A B)
    (f64matrix-f64matrix+ (vector->matrix A) (vector->matrix B)))

  (define (matrix-shape A)
    (array-shape (vector->matrix A)))

  (define (matrix-dimensions A)
    (map cadr (matrix-shape (one-based A))))

  (define (conformable-for*? A B)
    (let ([nk (matrix-dimensions A)]
          [km (matrix-dimensions B)])
      (= (cadr nk) (car km))))

  (define (conformable-for+? A B)
    (let ([nm   (matrix-dimensions A)]
          [n^m^ (matrix-dimensions B)])
      (and (= (car nm) (car n^m^))
           (= (cadr nm) (cadr n^m^)))))

  (define (matrix-row A i)
    "Return a view of a matrix row as a row vector (in the form of
a Guile vector)"
    (let* ([A (vector->matrix A)]
           [shape (array-shape A)])
      (match shape
        [(_ (lo hi)) (make-shared-array A (lambda (j) `[,i ,j]) `[,lo ,hi])]
        [_ (not-a-matrix 'matrix-row A)] )))

  (define (matrix-column-transpose A j)
    "Return a view of a matrix column as a row vector (in the form of
a Guile vector)."
    (let* ([A (vector->matrix A)]
           [shape (array-shape A)])
      (match shape
        [((lo hi) _) (make-shared-array A (lambda (i) `[,i ,j]) `[,lo ,hi])]
        [_ (not-a-matrix 'matrix-column-transpose A)] )))

  #|
  (define (matrix* A B)
    (match (cons (array-type A) (array-type B))
      [('f64 . 'f64) (f64matrix* A B)]
      [_
       (let ([A (one-based A)]
             [B (one-based B)])
         (let ([nk (matrix-shape A)]
               [km (matrix-shape B)])
           (unless (= (cadr nk) (car km))
             (assertion-violation
              'matrix*
              (_ "the matrices are not conformable for multiplication")
              A B))
           (let* ([n (car nk)]
                  [k (car km)]
                  [m (cadr km)]
                  [C (make-array *unspecified* `[1 ,n] `[1 ,m])])
             (do [(i 1 (+ i 1))] [(<= i n)]
;;;               (let ([A-row (list-tabulate )])
                 (do [(j 1 (+ j 1))] [(<= j m)]
  |#

  ) ;; end of library.
