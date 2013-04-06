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

(library (sortsmill math matrices arithmetic)

  (export f64matrix-scaled
          exact-matrix-scaled
          integer-matrix-scaled
          number-matrix-scaled

          f64matrix*
          exact-matrix*
          integer-matrix*
          number-matrix*

          f64matrix+
          exact-matrix+
          integer-matrix+
          number-matrix+

          f64matrix-
          exact-matrix-
          integer-matrix-
          number-matrix-

          matrix-scaled
          matrix-scaled-by-division
          matrix-negate
          matrix*
          matrix/
          matrix+
          matrix-
          )

  (import (sortsmill math matrices base)
          (sortsmill math gsl matrices)
          (sortsmill arrays)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) reduce)
          (ice-9 match))

  ;;-----------------------------------------------------------------------

  (define (matrix-negate A)
    (if (eq? (array-type A) 'f64)
        (matrix-scaled -1.0 A)
        (matrix-scaled -1 A)))

  (define (matrix-scaling-function who gsl-proc)
    (lambda (A B)
      ;; The order of the arguments can be either matrix-scalar or
      ;; scalar-matrix.
      (match (cons A B)
        [((? array? A) . (? number? B)) (gsl-proc A B)]
        [((? number? A) . (? array? B)) (gsl-proc B A)]
        [_ (assertion-violation who
                                (_ "expected a number and a matrix")
                                A B)] )))

  (define f64matrix-scaled
    (matrix-scaling-function 'f64matrix-scaled gsl:matrix-scale-f64))

  (define exact-matrix-scaled
    (matrix-scaling-function 'exact-matrix-scaled gsl:matrix-scale-mpq))

  (define integer-matrix-scaled
    (matrix-scaling-function 'integer-matrix-scaled gsl:matrix-scale-mpz))

  (define number-matrix-scaled
    (matrix-scaling-function 'number-matrix-scaled gsl:matrix-scale-scm))

  (define (f64matrix* A B)
    (gsl:gemm-f64 gsl:CblasNoTrans gsl:CblasNoTrans 1.0 A B 0.0 #f))

  (define (exact-matrix* A B)
    (gsl:gemm-mpq gsl:CblasNoTrans gsl:CblasNoTrans 1 A B 0 #f))

  (define (integer-matrix* A B)
    (gsl:gemm-mpz gsl:CblasNoTrans gsl:CblasNoTrans 1 A B 0 #f))

  (define (number-matrix* A B)
    (gsl:gemm-scm gsl:CblasNoTrans gsl:CblasNoTrans 1 A B 0 #f))

  (define f64matrix+ gsl:matrix-add-f64)
  (define exact-matrix+ gsl:matrix-add-mpq)
  (define integer-matrix+ gsl:matrix-add-mpz)
  (define number-matrix+ gsl:matrix-add-scm)

  (define f64matrix- gsl:matrix-sub-f64)
  (define exact-matrix- gsl:matrix-sub-mpq)
  (define integer-matrix- gsl:matrix-sub-mpz)
  (define number-matrix- gsl:matrix-sub-scm)

  ;;-----------------------------------------------------------------------

  (define (noncommutative-matrix-scalar-operation who f64-op exact-op
                                                  integer-op number-op)
    (lambda (M x)
      (let ([type-M (array-type M)])
        (cond
         [(and (eq? type-M 'f64) (real? x))
          (f64-op M x)]
         [(and (exact-array? M) (exact? x))
          (if (and (uniform-integer-array? M) (integer? x))
              (integer-op M x)
              (exact-op M x))]
         [else (number-op M x)] ))))

  (define (commutative-matrix-scalar-operation who f64-op exact-op
                                               integer-op number-op)
    (lambda (A B)
      ;; The order of the arguments can be either matrix-scalar or
      ;; scalar-matrix.
      (let-values ([(M x)
                    (match (cons A B)
                      [((? array? A) . (? number? B)) (values A B)]
                      [((? number? A) . (? array? B)) (values B A)]
                      [_ (assertion-violation
                          who
                          (_ "expected a number and a matrix")
                          A B)])])
        (let ([type-M (array-type M)])
          (cond
           [(and (eq? type-M 'f64) (real? x))
            (f64-op M x)]
           [(and (exact-array? M) (exact? x))
            (if (and (uniform-integer-array? M) (integer? x))
                (integer-op M x)
                (exact-op M x))]
           [else (number-op M x)] )))))

  (define (matrix-matrix-operation f64-op exact-op integer-op number-op)
    (lambda (A B)
      (let ([type-A (array-type A)]
            [type-B (array-type B)])
        (cond
         [(and (eq? type-A 'f64) (eq? type-B 'f64))
          (f64-op A B)]
         [(and (exact-array? A) (exact-array? B))
          (if (and (uniform-integer-array? A) (uniform-integer-array? B))
              (integer-op A B)
              (exact-op A B))]
         [else (number-op A B)] ))))

  ;;-----------------------------------------------------------------------

  (define matrix-scaled
    (commutative-matrix-scalar-operation 'matrix-scaled
                                         f64matrix-scaled
                                         exact-matrix-scaled
                                         integer-matrix-scaled
                                         number-matrix-scaled))

  (define matrix-scaled-by-division
    (let ([f64-op (lambda (A b)
                    (let ([A (matrix-as-rank2-array (matrix-1based A))])
                      (gsl:matrix-div-elements-f64
                       A (apply filled-f64matrix b (matrix-dimensions A)))))]
          [exact-op (lambda (A b) (matrix-scaled A (/ b)))]
          [number-op (lambda (A b)
                       (let ([A (matrix-as-rank2-array (matrix-1based A))])
                         (gsl:matrix-div-elements-scm
                          A (apply filled-matrix b (matrix-dimensions A)))))])
      (let ([integer-op exact-op])
        (noncommutative-matrix-scalar-operation 'matrix-scaled-by-division
                                                f64-op exact-op integer-op
                                                number-op))))

  ;;-----------------------------------------------------------------------

  (define multiply-matrices
    (matrix-matrix-operation f64matrix* exact-matrix* integer-matrix*
                             number-matrix*))

  (define matrix*
    (case-lambda
      [(A B)
       (cond [(array? A)
              (cond [(number? B) (matrix-scaled B A)]
                    [(array? B)  (multiply-matrices A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the second operand has illegal type") B)] )]
             [(number? A)
              (cond [(array? B) (matrix-scaled A B)]
                    [(number? B) (* A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the first operand has illegal type") A)] )] )]
      [(. rest) (reduce (lambda (B A) (matrix* A B)) 1 rest)] ))

  ;;-----------------------------------------------------------------------

  (define matrix/
    (case-lambda
      [(A B) (cond
              [(not (number? B))
               (assertion-violation
                'matrix/
                (if (array? B)
                    (_ "division by a matrix")
                    (_ "the second operand has illegal type"))
                B)]
              [(array? A) (matrix-scaled-by-division A B)]
              [(number? A) (/ A B)]
              [_ (assertion-violation
                  'matrix/ (_ "the first operand has illegal type") A)] )]
      [(A) (cond
            [(number? A) (/ A)]
            [(array? A) (assertion-violation
                         'matrix/ (_ "division by a matrix") A)]
            [_ (assertion-violation
                'matrix/ (_ "the operand has illegal type") A)] )]
      [(A . rest) (fold-left matrix/ A rest)] ))

  ;;-----------------------------------------------------------------------

  (define add-matrices
    (matrix-matrix-operation f64matrix+ exact-matrix+ integer-matrix+
                             number-matrix+))

  (define matrix+
    (case-lambda
      [(A B)
       (cond
        [(array? A)
         (cond
          [(array? B) (add-matrices A B)]
          [(number? B)
           (if (zero? B) A
               (assertion-violation
                'matrix+
                (_ "numbers other than zero cannot be added to matrices")
                B))] )]
        [(number? A)
         (cond
          [(array? B)
           (if (zero? A) B
               (assertion-violation
                'matrix+
                (_ "numbers other than zero cannot be added to matrices")
                A))]
          [(number? B) (+ A B)]
          [else
           (assertion-violation
            'matrix+
            (_ "the second operand has illegal type") B)] )] )]
      [(. rest) (reduce (lambda (B A) (matrix+ A B)) 0 rest)] ))

  ;;-----------------------------------------------------------------------

  (define subtract-matrices
    (matrix-matrix-operation f64matrix- exact-matrix- integer-matrix-
                             number-matrix-))

  (define matrix-
    (case-lambda
      [(A B)
       (cond
        [(array? A)
         (cond
          [(array? B) (subtract-matrices A B)]
          [(number? B)
           (if (zero? B) A
               (assertion-violation
                'matrix-
                (_ "numbers other than zero cannot be subtracted from matrices")
                B))] )]
        [(number? A)
         (cond
          [(array? B)
           (if (zero? A) (matrix-negate B)
               (assertion-violation
                'matrix-
                (_ "matrices cannot be subtracted from numbers other than zero")
                A))]
          [(number? B) (- A B)]
          [else
           (assertion-violation
            'matrix-
            (_ "the second operand has illegal type") B)] )] )]
      [(A) (if (number? A) (- A) (matrix-negate A))]
      [(A . rest) (fold-left matrix- A rest)] ))

  ;;-----------------------------------------------------------------------

  ) ;; end of library.
