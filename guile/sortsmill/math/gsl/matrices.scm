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
          gsl:matrix-add-scm

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
          ;;    (gsl:svd-WHATEVER A) → U, diag S, V′
          ;;
          gsl:svd-f64-golub-reinsch
          gsl:svd-f64-modified-golub-reinsch
          gsl:svd-f64-jacobi
          )

  (import (sortsmill arrays)
          (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut))

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

  ) ;; end of library.
