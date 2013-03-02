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

(library (sortsmill math matrices linalg)

  (export
   ;; SVD algorithms enum:
   ;;   'golub-reinsch
   ;;   'modified-golub-reinsch
   ;;   'jacobi
   matrix-svd-algorithms

   ;; Initially set to 'golub-reinsch
   current-matrix-svd-algorithm-fluid
   current-matrix-svd-algorithm
   set-current-matrix-svd-algorithm!
   with-matrix-svd-algorithm

   ;; (f64matrix-svd A algorithm)
   ;;
   ;; (f64matrix-svd A) is equivalent to
   ;;   (f64matrix-svd A 'golub-reinsch)
   f64matrix-svd

   ;; RCOND value for determining the effective rank in some
   ;; SVD-based calculations. If you have LAPACK installed then
   ;; see, for example, ‘man dgelss’, for a description of
   ;; RCOND.
   current-matrix-svd-rcond-fluid
   current-matrix-svd-rcond
   set-current-matrix-svd-rcond!
   with-matrix-svd-rcond

   matrix-svd-effective-rank
   matrix-svd-limit-rank

   ;; In the current implementation, the following linear
   ;; system solvers use the current SVD algorithm.
   f64matrix-solve:AX=B
   f64matrix-solve:XA=B

   ;; Inverse or pseudoinverse.
   f64matrix-pinv

   ;; These use different algorithms depending on what kinds of
   ;; matrix it sees. Currently it fully supports only matrices
   ;; A that are square and have fully rank. (In floating point
   ;; you might get infinities and NaNs rather than exceptions,
   ;; if the effective rank is less than full. At the moment,
   ;; this is considered a feature.)
   ;;
   ;; The present implementation uses the current SVD algorithm
   ;; (and returns an f64matrix) if all the matrix entries are
   ;; inexact and real; otherwise an LU decomposition is used.
   matrix-solve:AX=B
   matrix-solve:XA=B

   ;; Inverse of a full rank square matrix. (In floating point
   ;; you might get infinities and NaNs rather than exceptions,
   ;; if the effective rank is less than full. At the moment,
   ;; this is considered a feature.)
   matrix-inv
   )

  (import (sortsmill math matrices base)
          (sortsmill math matrices arithmetic)
          (sortsmill math gsl matrices)
          (sortsmill arrays)
          (sortsmill math math-constants)
          (sortsmill i18n)
          (sortsmill kwargs)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) take)
          (only (srfi :4) f64vector-length)
          (ice-9 match))


  ;;-----------------------------------------------------------------------
  ;;
  ;; Singular value decomposition.

  (define matrix-svd-algorithms
    (make-enumeration '(golub-reinsch modified-golub-reinsch jacobi)))

  (define current-matrix-svd-algorithm-fluid
    (make-fluid 'golub-reinsch))

  (define (current-matrix-svd-algorithm)
    (fluid-ref current-matrix-svd-algorithm-fluid))

  (define (set-current-matrix-svd-algorithm! algorithm)
    (assert (symbol? algorithm))
    (assert (enum-set-member? algorithm matrix-svd-algorithms))
    (fluid-set! current-matrix-svd-algorithm-fluid algorithm))

  (define-syntax with-matrix-svd-algorithm
    (lambda (x)
      (syntax-case x ()
        [(_ algorithm body body* ...)
         #'(begin
             (assert (symbol? algorithm))
             (assert (enum-set-member? algorithm matrix-svd-algorithms))
             (with-fluid* current-matrix-svd-algorithm-fluid algorithm
                          (lambda () body body* ...)))] )))

  (define (svd-rcond-value rcond)
    (cond [(not rcond)       (* 100 c:dbl-epsilon-exact)]
          [(negative? rcond) (* 100 c:dbl-epsilon-exact)]
          [else              (inexact->exact rcond)] ))

  (define current-matrix-svd-rcond-fluid
    (make-fluid (svd-rcond-value #f)))

  (define (current-matrix-svd-rcond)
    (fluid-ref current-matrix-svd-rcond-fluid))

  (define (set-current-matrix-svd-rcond! rcond)    
    (fluid-set! current-matrix-svd-rcond-fluid
                (svd-rcond-value rcond)))

  (define-syntax with-matrix-svd-rcond
    (lambda (x)
      (syntax-case x ()
        [(_ rcond body body* ...)
         #'(with-fluid*
               current-matrix-svd-rcond-fluid (svd-rcond-value rcond)
               (lambda () body body* ...))] )))

  (define f64matrix-svd
    (case-lambda
      [(A) (f64matrix-svd A (current-matrix-svd-algorithm))]
      [(A algorithm)
       (assert (symbol? algorithm))
       (assert (enum-set-member? algorithm matrix-svd-algorithms))
       (match algorithm
         ['golub-reinsch (gsl:svd-golub-reinsch-f64 A)]
         ['modified-golub-reinsch (gsl:svd-modified-golub-reinsch-f64 A)]
         ['jacobi (gsl:svd-jacobi-f64 A)] )] ))

  (define matrix-svd-effective-rank
    (case-lambda
      [(S) (matrix-svd-effective-rank S (current-matrix-svd-rcond))]
      [(S rcond)
       (let* ([S (zero-based (row-matrix->vector S))]
              [rcond^ (* rcond (generalized-vector-ref S 0))])
         (if (zero? rcond^) 0
             (let ([n (generalized-vector-length S)])
               (letrec
                   ([count
                     (lambda (i)
                       (cond [(= i n) n]
                             [(<= (generalized-vector-ref S i) rcond^) i]
                             [else (count (+ i 1))] ))])
                 (count 1)))))] ))

  (define (matrix-svd-limit-rank S rank)
    (let* ([S^ (row-matrix->vector S)]
           [n (generalized-vector-length S^)]
           [lst (generalized-vector->list S^)]
           [lst^ (append (take lst rank) (make-list (- n rank) 0))])
      (list->typed-array (array-type S) (array-shape S^) lst^)))

  (define/kwargs (f64matrix-solve:AX=B A B
                                       [full-rank? #t]
                                       [rcond (current-matrix-svd-rcond)])
    (call-with-values (lambda () (f64matrix-svd A))
      (lambda (U S V)
        (let ([effective-rank (matrix-svd-effective-rank S rcond)])
          (when full-rank?
            (unless (= effective-rank (f64vector-length S))
              (rank-deficiency-exception 'f64matrix-solve:AX=B A)))
          (values (gsl:svd-solve-f64 U S V B) effective-rank)))))

  (define/kwargs (f64matrix-solve:XA=B A B
                                       [full-rank? #t]
                                       [rcond (current-matrix-svd-rcond)])
    (matrix-transpose (f64matrix-solve:AX=B (matrix-transpose A)
                                            (matrix-transpose B)
                                            full-rank? rcond)))

  (define/kwargs (f64matrix-pinv A
                                 [full-rank? #t]
                                 [rcond (current-matrix-svd-rcond)])
    "Return the inverse or Moore-Penrose pseudoinverse of A, and the
effective rank of A."
    (call-with-values (lambda () (f64matrix-svd A))
      (lambda (U S V)
        (let ([effective-rank (matrix-svd-effective-rank S rcond)])
          (when full-rank?
            (unless (= effective-rank (f64vector-length S))
              (rank-deficiency-exception 'f64matrix-pinv A)))
          (let* ([S-pinv (matrix-map (lambda (x) (if (flzero? x) x (/ x)))
                                     (matrix-svd-limit-rank S effective-rank))]
                 [A-pinv (f64matrix*
                          (f64matrix* V (f64vector->diagonal-f64matrix S-pinv))
                          (matrix-transpose U))])
            (values A-pinv effective-rank))))))

  ;;-----------------------------------------------------------------------
  ;;
  ;; Solution of square linear systems.

  (define (exact-matrix-solve:AX=B A B)
    (call-with-values (lambda () (gsl:lu-decomposition-mpq-fast-pivot A))
      (lambda (LU permutation signum)
        (gsl:lu-solve-mpq LU permutation B))))

  (define (number-matrix-solve:AX=B A B)
    (call-with-values (lambda () (gsl:lu-decomposition-scm A))
      (lambda (LU permutation signum)
        (gsl:lu-solve-scm LU permutation B))))

  (define/kwargs (matrix-solve:AX=B A B)
    (assert (square-matrix? A))
    (let ([type-A (array-type A)]
          [type-B (array-type B)])
      (cond
       [(and (eq? type-A 'f64) (eq? type-B 'f64))
        (let-values ([(X effective-rank)
                      (f64matrix-solve:AX=B A B #:full-rank? #t)])
          X)]
       [(and (exact-array? A) (exact-array? B))
        (exact-matrix-solve:AX=B A B)]
       [(and (inexact-real-array? A) (inexact-real-array? B))
        (let-values ([(X effective-rank)
                      (f64matrix-solve:AX=B (matrix->f64matrix A)
                                            (matrix->f64matrix B)
                                            #:full-rank? #t)])
          X)]
       [else (number-matrix-solve:AX=B A B)] )))

  (define/kwargs (matrix-solve:XA=B A B)
    (matrix-transpose (matrix-solve:AX=B (matrix-transpose A)
                                         (matrix-transpose B))))

  ;;-----------------------------------------------------------------------
  ;;
  ;; Matrix inversion.

  (define (exact-matrix-invert A)
    (call-with-values (lambda () (gsl:lu-decomposition-mpq-fast-pivot A))
      (lambda (LU permutation signum)
        (gsl:lu-invert-mpq LU permutation))))

  (define (number-matrix-invert A)
    (call-with-values (lambda () (gsl:lu-decomposition-scm A))
      (lambda (LU permutation signum)
        (gsl:lu-invert-scm LU permutation))))

  (define/kwargs (matrix-inv A)
    (assert (square-matrix? A))
    (let ([type-A (array-type A)])
      (cond
       [(eq? type-A 'f64)
        (let-values ([(X effective-rank)
                      (f64matrix-pinv A #:full-rank? #t)])
          X)]
       [(exact-array? A) (exact-matrix-invert A)]
       [(inexact-real-array? A)
        (let-values ([(X effective-rank)
                      (f64matrix-pinv (matrix->f64matrix A)
                                      #:full-rank? #t)])
          X)]
       [else (number-matrix-invert A)] )))

  ;;-----------------------------------------------------------------------

  ) ;; end of library.
