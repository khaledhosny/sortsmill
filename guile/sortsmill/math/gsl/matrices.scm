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

(library (sortsmill math gsl matrices)

  (export gsl:CblasRowMajor
          gsl:CblasColMajor
          gsl:CblasNoTrans
          gsl:CblasTrans
          gsl:CblasConjTrans
          gsl:CblasUpper
          gsl:CblasLower
          gsl:CblasNonUnit
          gsl:CblasUnit
          gsl:CblasLeft
          gsl:CblasRight

          ;; Scaling a matrix by a scalar.
          ;;
          ;; (gsl:matrix-scale-WHATEVER A lambda) → λA
          ;;
          gsl:matrix-scale-f64
          gsl:matrix-scale-mpz
          gsl:matrix-scale-mpq
          gsl:matrix-scale-scm

          ;; General matrix multiplication.
          ;;
          ;; (gsl:gemm-WHATEVER TransA TransB alpha A B beta C) → αAB + βC
          ;;
          gsl:gemm-f64                  ; Real floating point.
          gsl:gemm-mpz                  ; Integers.
          gsl:gemm-mpq                  ; Exact rationals.
          gsl:gemm-scm                  ; General numbers.

          ;; Matrix addition.
          ;;
          ;; (gsl:matrix-add-WHATEVER A B) → A + B
          ;;
          gsl:matrix-add-f64
          gsl:matrix-add-mpz
          gsl:matrix-add-mpq
          gsl:matrix-add-scm

          ;; Matrix subtraction.
          ;;
          ;; (gsl:matrix-sub-WHATEVER A B) → A − B
          ;;
          gsl:matrix-sub-f64
          gsl:matrix-sub-mpz
          gsl:matrix-sub-mpq
          gsl:matrix-sub-scm

          ;; Elementwise (Hadamard) multiplication.
          ;;
          ;; (gsl:matrix-mul-elements-WHATEVER A B)
          ;;
          gsl:matrix-mul-elements-f64
          gsl:matrix-mul-elements-mpz
          gsl:matrix-mul-elements-mpq
          gsl:matrix-mul-elements-scm

          ;; Elementwise division. (Multiplication by the Hadamard
          ;; inverse.)
          ;;
          ;; (gsl:matrix-div-elements-WHATEVER A B)
          ;;
          gsl:matrix-div-elements-f64
          gsl:matrix-div-elements-mpq
          gsl:matrix-div-elements-scm

          ;; Predicates.
          ;;
          gsl:matrix-isnull-f64?
          gsl:matrix-isnull-mpz?
          gsl:matrix-isnull-mpq?
          gsl:matrix-isnull-scm?

          gsl:matrix-isneg-f64?
          gsl:matrix-isneg-mpz?
          gsl:matrix-isneg-mpq?
          gsl:matrix-isneg-scm?

          gsl:matrix-ispos-f64?
          gsl:matrix-ispos-mpz?
          gsl:matrix-ispos-mpq?
          gsl:matrix-ispos-scm?

          gsl:matrix-isnonneg-f64?
          gsl:matrix-isnonneg-mpz?
          gsl:matrix-isnonneg-mpq?
          gsl:matrix-isnonneg-scm?

          gsl:matrix-equal-f64?
          gsl:matrix-equal-mpz?
          gsl:matrix-equal-mpq?
          gsl:matrix-equal-scm?

          ;; ‘Thin’ singular value decomposition algorithms from
          ;; GSL. These procedures return U, S, V, respectively, as
          ;; multiple return values.
          ;;
          ;; Golub-Reinsch requires linear workspace.
          ;;
          ;; Modified Golub-Reinsch is faster than Golub-Reinsch if
          ;; there are many more rows than columns in the input
          ;; matrix, but requires quadratic workspace.
          ;;
          ;; The Jacobi method can compute singular values to higher
          ;; relative accuracy than Golub-Reinsch, but is slower. It
          ;; uses one-sided Jacobi orthogonalization.
          ;;
          ;;    (gsl:svd-WHATEVER-f64 A) → U, diag S, V′
          ;;
          gsl:svd-golub-reinsch-f64
          gsl:svd-modified-golub-reinsch-f64
          gsl:svd-jacobi-f64

          gsl:svd-solve-f64

          ;; LU decomposition.
          ;;
          ;; (gsl:lu-decomposition-WHATEVER A) → LU, permutation, signum
          ;;
          ;; L and U are stored in the same matrix, and L has an
          ;; implicit unit diagonal.
          gsl:lu-decomposition-f64
          gsl:lu-decomposition-mpq
          gsl:lu-decomposition-scm

          ;; Similar to gsl:lu-decomposition-mpq, but taking the first
          ;; non-zero pivot rather than the maximum. In general this
          ;; will give a different LU decomposition than the other
          ;; routines.
          gsl:lu-decomposition-mpq-fast-pivot

          gsl:lu-solve-f64
          gsl:lu-solve-mpq
          gsl:lu-solve-scm
          )

  (import (sortsmill arrays)
          (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_math_gsl_matrices"))

  (define (assert-rank-1-or-2-array who A)
    (unless (let ([r (array-rank A)]) (or (= r 1) (= r 2)))
      (assertion-violation who (_ "expected an array of rank 1 or 2") A)))

  (define (assert-f64-rank-1-or-2-array who A)
    (unless (typed-array? A 'f64)
      (assertion-violation who (_ "expected an array of type f64") A))
    (assert-rank-1-or-2-array who A))

  (define (assert-exact-rank-1-or-2-array who A)
    (unless (exact-array? A)
      (assertion-violation who (_ "expected an array of exact numbers") A))
    (assert-rank-1-or-2-array who A))

  (define (assert-CblasTrans-flag who trans)
    (unless (or (eqv? trans gsl:CblasNoTrans)
                (eqv? trans gsl:CblasTrans)
                (eqv? trans gsl:CblasConjTrans))
      (assertion-violation
       who
       (_ "expected gsl:CblasNoTrans, gsl:CblasTrans, or gsl:CblasConjTrans")
       trans)))

  (define (array-dimensions-simplified A)
    (map
     (match-lambda [(lo hi) (- hi lo -1)])
     (array-shape A)))

  ) ;; end of library.
