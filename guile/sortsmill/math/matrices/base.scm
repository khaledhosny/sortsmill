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

;;;
;;; FIXME: Perhaps lot of these routines could be made significantly
;;; less expensive by rewriting in C. It would take some of the worry
;;; out of using them, in any case. This may make for a good project
;;; on lazy days.
;;;

(library (sortsmill math matrices base)

  (export not-a-matrix
          rank-deficiency-exception

          zero-based
          one-based

          ;; Shape inquiry procedures, accepting both typed and
          ;; untyped arrays and vectors.
          matrix?           ; (matrix? A) → boolean
          matrix-shape      ; (matrix-shape A) → ((lr ur) (lc rc))
          matrix-dimensions ; (matrix-dimensions A) → (numrows numcols)
          matrix-row-count  ; (matrix-row-count A) → numrows
          matrix-column-count      ; (matrix-column-count A) → numcols
          row-matrix-size          ; (row-matrix-size A) → numcols
          column-matrix-size       ; (column-matrix-size A) → numrows
          square-matrix?           ; (square-matrix A) → boolean
          conformable-for-matrix*? ; (conformable-for-matrix*? A B) → boolean
          conformable-for-matrix+? ; (conformable-for-matrix+? A B) → boolean

          zero-matrix
          zero-f64matrix
          filled-matrix
          filled-f64matrix
          I-matrix
          I-f64matrix
          scalar-matrix
          scalar-f64matrix

          matrix-map
          matrix-copy

          f64vector->diagonal-f64matrix
          vector->diagonal-matrix

          vector->matrix
          row-matrix->vector

          matrix-1x1->scalar

          ;; (matrix-ref A i j) → value
          ;; (matrix-0ref A i j) → value   (zero-based indexing)
          ;; (matrix-1ref A i j) → value   (one-based indexing)
          ;;
          ;; These accept both typed and untyped arrays and vectors.
          matrix-ref
          matrix-0ref
          matrix-1ref

          ;; (matrix-set! A i j value) → *unspecified*
          ;; (matrix-0set! A i j value) → *unspecified*   (zero-based indexing)
          ;; (matrix-1set! A i j value) → *unspecified*   (one-based indexing)
          ;;
          ;; These accept both typed and untyped arrays and vectors.
          matrix-set!
          matrix-0set!
          matrix-1set!

          matrix-row
          matrix-0row
          matrix-1row
          matrix-column-transpose
          matrix-column
          matrix-diagonal
          matrix-transpose

          matrix-exact->inexact
          matrix-inexact->exact

          ;; matrix->f64matrix is the same as matrix-exact->inexact.
          matrix->f64matrix

          ;; f64matrix->matrix just changes the type tag, without
          ;; converting from inexact to exact numbers.
          f64matrix->matrix
          )

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (srfi :4)                  ; SRFI-4 uniform numeric vectors.
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension
     "init_guile_sortsmill_math_matrices_base"))

  ;;-----------------------------------------------------------------------

  (define (not-a-matrix caller irritant . more-irritants)
    (if (null? more-irritants)
        (assertion-violation caller
                             (_ "expected a vector or matrix")
                             irritant)
        (apply assertion-violation caller
               (_ "expected vectors or matrices")
               irritant more-irritants)))

  (define (matrix-empty caller . irritants)
    (assertion-violation caller
                         (_ "a vector or matrix is empty")
                         irritants))

  (define (rank-deficiency-exception caller . irritants)
    (apply error caller (_ "rank-deficient matrix") irritants))

  ;;-----------------------------------------------------------------------

  (define zero-matrix
    (case-lambda
      [(n)   (make-array 0 `(1 ,n) `(1 ,n))]
      [(n m) (make-array 0 `(1 ,n) `(1 ,m))] ))

  (define zero-f64matrix
    (case-lambda
      [(n)   (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,n))]
      [(n m) (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,m))] ))

  ;; FIXME: Is there a better name for this?
  (define filled-matrix
    (case-lambda
      [(v n)   (make-array v `(1 ,n) `(1 ,n))]
      [(v n m) (make-array v `(1 ,n) `(1 ,m))] ))

  ;; FIXME: Is there a better name for this?
  (define filled-f64matrix
    (case-lambda
      [(v n)   (make-typed-array 'f64 v `(1 ,n) `(1 ,n))]
      [(v n m) (make-typed-array 'f64 v `(1 ,n) `(1 ,m))] ))

  (define I-matrix
    (case-lambda
      [(n)   (I-matrix n n)]
      [(n m) (let* ([I (zero-matrix n m)]
                    [diag (matrix-diagonal I)])
               (array-fill! diag 1)
               I)] ))

  (define I-f64matrix
    (case-lambda
      [(n)   (I-f64matrix n n)]
      [(n m) (let* ([I (zero-f64matrix n m)]
                    [diag (matrix-diagonal I)])
               (array-fill! diag 1.0)
               I)] ))

  (define scalar-matrix
    (case-lambda
      [(v n)   (scalar-matrix v n n)]
      [(v n m) (let* ([A (zero-matrix n m)]
                      [diag (matrix-diagonal A)])
                 (array-fill! diag v)
                 A)] ))

  (define scalar-f64matrix
    (case-lambda
      [(v n)   (scalar-f64matrix v n n)]
      [(v n m) (let* ([A (zero-f64matrix n m)]
                      [diag (matrix-diagonal A)])
                 (array-fill! diag v)
                 A)] ))

  ;;-----------------------------------------------------------------------

  (define (zero-based A)
    (match (array-shape A)
      [[(lo hi)]
       (when (< hi lo)
         (matrix-empty 'zero-based A))
       (make-shared-array A
                          (lambda (i) `[,(+ i lo)])
                          `[0 ,(- hi lo)] )]
      [[(lo1 hi1) (lo2 hi2)]
       (when (or (< hi1 lo1) (< hi2 lo2))
         (matrix-empty 'zero-based A))
       (make-shared-array A
                          (lambda (i j) `[,(+ i lo1) ,(+ j lo2)])
                          `[0 ,(- hi1 lo1)] `[0 ,(- hi2 lo2)] )]
      [_ (not-a-matrix 'zero-based A)]))

  (define (one-based A)
    (match (array-shape A)
      [[(lo hi)]
       (when (< hi lo)
         (matrix-empty 'one-based A))
       (make-shared-array A
                          (lambda (i) `[,(+ i lo -1)])
                          `[1 ,(- hi lo -1)] )]
      [[(lo1 hi1) (lo2 hi2)]
       (when (or (< hi1 lo1) (< hi2 lo2))
         (matrix-empty 'one-based A))
       (make-shared-array A
                          (lambda (i j) `[,(+ i lo1 -1) ,(+ j lo2 -1)])
                          `[1 ,(- hi1 lo1 -1)] `[1 ,(- hi2 lo2 -1)] )]
      [_ (not-a-matrix 'one-based A)]))

  (define (matrix-1x1->scalar A)
    (if (array? A)
        (match (array-shape A)
          [((lo hi))
           (if (eqv? lo hi)
               (generalized-vector-ref A lo)
               A)]
          [((lo1 hi1) (lo2 hi2))
           (if (and (eqv? lo1 hi1) (eqv? lo2 hi2))
               (array-ref A lo1 lo2)
               A)]
          [_ A])
        A))

  (define (vector->matrix v)
    (match (array-rank v)
      [1
       (match (array-shape v)
         [((lo hi))
          (make-shared-array v (lambda (i j) `[,j]) `[,lo ,lo] `[,lo ,hi])])]
      [2 v]
      [_ (not-a-matrix 'vector->matrix v)] ))

  (define (row-matrix->vector V)
    (match (array-shape V)
      [[(lo1 hi1) (lo2 hi2)]
       (if (= lo1 hi1)
           (make-shared-array V (lambda (j) `[,lo1 ,j]) `[,lo2 ,hi2])
           (assertion-violation
            'row-matrix->vector (_ "not a row matrix") V))]
      [[(_ _)] V]
      [_ (not-a-matrix 'row-matrix->vector V)] ))

  (define (matrix-map proc A)
    (let* ([type (array-type A)]
           [shape (array-shape A)]
           [B (apply make-typed-array type *unspecified* shape)])
      (array-map! B proc A)
      B))

  (define (matrix-copy A)
    (matrix-map identity A))

  (define (f64vector->diagonal-f64matrix v)
    (let* ([v (row-matrix->vector v)]
           [n (f64vector-length v)]
           [A (zero-f64matrix n)])
      (array-map! (matrix-diagonal A) identity v)
      A))

  (define (vector->diagonal-matrix v)
    (if (f64vector? v)
        [f64vector->diagonal-f64matrix v]
        [let* ([v (row-matrix->vector v)]
               [n (generalized-vector-length v)]
               [A (zero-matrix n)])
          (array-map! (matrix-diagonal A) identity v)
          A] ))

  ;;-----------------------------------------------------------------------

  #|
  (define (matrix-row A i)
    "Return a view of a matrix row as a row vector (in the form of
a Guile vector)"
    (let* ([A (vector->matrix A)]
           [shape (array-shape A)])
      (match shape
        [(_ (lo hi)) (make-shared-array A (lambda (j) `[,i ,j]) `[,lo ,hi])]
        [_ (not-a-matrix 'matrix-row A)] )))
  |#

  (define (matrix-column-transpose A j)
    "Return a view of a matrix column as a row vector (in the form of
a Guile vector)."
    (let* ([A (vector->matrix A)]
           [shape (array-shape A)])
      (match shape
        [((lo hi) _) (make-shared-array A (lambda (i) `[,i ,j]) `[,lo ,hi])]
        [_ (not-a-matrix 'matrix-column-transpose A)] )))

  (define (matrix-column A j)
    "Return a view of a matrix column as a matrix (that is, a rank-2
array)."
    (matrix-transpose (matrix-column-transpose A j)))

  (define (matrix-diagonal A)
    (let ([n (apply min (matrix-dimensions A))])
      (make-shared-array A (lambda (i) `[,i ,i]) `[1 ,n])))

  (define (matrix-transpose A)
    (transpose-array (vector->matrix A) 1 0))

  ;;-----------------------------------------------------------------------

  (define (matrix-exact->inexact A)
    (let ([B (apply make-typed-array 'f64 *unspecified* (array-shape A))])
      (array-map! B exact->inexact A)
      B))

  (define (matrix-inexact->exact A)
    (let ([B (apply make-array *unspecified* (array-shape A))])
      (array-map! B inexact->exact A)
      B))

  (define matrix->f64matrix matrix-exact->inexact)

  (define (f64matrix->matrix A)
    (let ([B (apply make-array *unspecified* (array-shape A))])
      (array-map! B identity A)
      B))

  ;;-----------------------------------------------------------------------

  ) ;; end of library.
