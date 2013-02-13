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

          zero-matrix
          zero-f64matrix
          I-matrix
          I-f64matrix

          vector->matrix
          row-matrix->vector

          matrix-row
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

          f64matrix-f64matrix*
          f64matrix-f64matrix+
          f64matrix-f64matrix-
          f64matrix*
          f64matrix+
          f64matrix-

          matrix-scaled
          matrix*
          matrix+
          matrix-)          

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) iota reduce)
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

  (define zero-matrix
    (case-lambda
      [(n)   (make-array 0 `(1 ,n) `(1 ,n))]
      [(n m) (make-array 0 `(1 ,n) `(1 ,m))] ))

  (define zero-f64matrix
    (case-lambda
      [(n)   (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,n))]
      [(n m) (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,m))] ))

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

  (define (f64matrix* A B)
    (f64matrix-f64matrix* (vector->matrix A) (vector->matrix B)))

  (define (f64matrix+ A B)
    (f64matrix-f64matrix+ (vector->matrix A) (vector->matrix B)))

  (define (f64matrix- A B)
    (f64matrix-f64matrix- (vector->matrix A) (vector->matrix B)))

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

  (define (matrix-column A j)
    "Return a view of a matrix column as a matrix (that is, a rank-2
array)."
    (matrix-transpose (matrix-column-transpose A j)))

  (define (matrix-diagonal A)
    (let ([n (apply min (matrix-dimensions A))])
      (make-shared-array A (lambda (i) `[,i ,i]) `[1 ,n])))

  (define (matrix-transpose A)
    (transpose-array (vector->matrix A) 1 0))

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

  (define (row*col row column-transposed)
    (apply + (map * (generalized-vector->list row)
                  (generalized-vector->list column-transposed))))

  (define (scale-matrix a B)
    (if (and (real? a) (eq? (array-type B) 'f64))
        [let ([C (apply make-typed-array 'f64 *unspecified* (array-shape B))])
          (array-map! C (lambda (x) (* a x)) B)
          C]
        [let ([C (apply make-array *unspecified* (array-shape B))])
          (array-map! C (lambda (x) (* a x)) B)
          C] ))

  (define (result-type type-A type-B)
    (match (cons type-A type-B)
      [('f64 . _) 'f64]
      [(_ . 'f64) 'f64]
      [_            #t] ))

  (define (multiply-matrices A B)
    (let ([type-A (array-type A)]
          [type-B (array-type B)])
      (match (cons type-A type-B)
        [('f64 . 'f64) (f64matrix* A B)]
        [_
         (let ([A (one-based A)]
               [B (one-based B)]
               [nk (matrix-dimensions A)]
               [km (matrix-dimensions B)])
           (unless (= (cadr nk) (car km))
             (assertion-violation
              'matrix*
              (_ "the matrices are not conformable for multiplication")
              A B))
           (let* ([n (car nk)]
                  [k (car km)]
                  [m (cadr km)]
                  [type-C (result-type type-A type-B)]
                  [C (make-typed-array type-C *unspecified* `[1 ,n] `[1 ,m])]
                  [row-indices (iota n 1)]
                  [rows (vector-map (lambda (i) (matrix-row A i))
                                    (list->vector row-indices))]
                  [col-indices (iota m 1)]
                  [cols (vector-map
                         (lambda (j) (matrix-column-transpose B j))
                         (list->vector col-indices))])
             (for-each
              (lambda (i)
                (for-each
                 (lambda (j)
                   (array-set! C (row*col (vector-ref rows (1- i))
                                          (vector-ref cols (1- j)))
                               i j))
                 col-indices))
              row-indices)
             C))] )))

  (define matrix*
    (case-lambda
      [(A B)
       (cond [(array? A)
              (cond [(number? B) (scale-matrix B A)]
                    [(array? B) (multiply-matrices A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the second operand has illegal type") B)])]
             [(number? A)
              (cond [(array? B) (scale-matrix A B)]
                    [(number? B) (* A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the first operand has illegal type") A)])])]
      [(. rest) (reduce (lambda (B A) (matrix* A B)) 1 rest)] ))

  (define (matrix+ A B)
    (match (cons (array-type A) (array-type B))
      [('f64 . 'f64) (f64matrix+ A B)]
      [_
       (let ([A (vector->matrix (one-based A))]
             [B (vector->matrix (one-based B))]
             [nm (matrix-dimensions A)]
             [n^m^ (matrix-dimensions B)])
         (unless (and (= (car nm) (car n^m^))
                      (= (cadr nm) (cadr n^m^)))
           (assertion-violation
            'matrix+
            (_ "the matrices are not conformable for addition")
            A B))
         (let* ([n (car nm)]
                [m (cadr nm)]
                [C (make-array *unspecified* `[1 ,n] `[1 ,m])]
                [row-indices (iota n 1)]
                [col-indices (iota m 1)])
           (for-each
            (lambda (i)
              (for-each
               (lambda (j)
                 (array-set! C (+ (array-ref A i j)
                                  (array-ref B i j))
                             i j))
               col-indices))
            row-indices)
           C))] ))

  (define (matrix- A B)
    (match (cons (array-type A) (array-type B))
      [('f64 . 'f64) (f64matrix- A B)]
      [_
       (let ([A (vector->matrix (one-based A))]
             [B (vector->matrix (one-based B))]
             [nm (matrix-dimensions A)]
             [n^m^ (matrix-dimensions B)])
         (unless (and (= (car nm) (car n^m^))
                      (= (cadr nm) (cadr n^m^)))
           (assertion-violation
            'matrix+
            (_ "the matrices are not conformable for subtraction")
            A B))
         (let* ([n (car nm)]
                [m (cadr nm)]
                [C (make-array *unspecified* `[1 ,n] `[1 ,m])]
                [row-indices (iota n 1)]
                [col-indices (iota m 1)])
           (for-each
            (lambda (i)
              (for-each
               (lambda (j)
                 (array-set! C (- (array-ref A i j)
                                  (array-ref B i j))
                             i j))
               col-indices))
            row-indices)
           C))] ))

  ) ;; end of library.
