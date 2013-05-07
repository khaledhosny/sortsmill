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

(library (sortsmill math matrices base)

  (export

   ;; Shape inquiry procedures, accepting both typed and
   ;; untyped arrays and vectors.
   matrix?                 ; (matrix? A) → boolean
   matrix-shape            ; (matrix-shape A) → ((lr ur) (lc rc))
   matrix-dimensions       ; (matrix-dimensions A) → (numrows numcols)
   matrix-row-count        ; (matrix-row-count A) → numrows
   matrix-column-count     ; (matrix-column-count A) → numcols
   row-matrix-size         ; (row-matrix-size A) → numcols
   column-matrix-size      ; (column-matrix-size A) → numrows
   square-matrix?          ; (square-matrix A) → boolean
   conformable-for-matrix*? ; (conformable-for-matrix*? A B) → boolean
   conformable-for-matrix+? ; (conformable-for-matrix+? A B) → boolean

   ;; Create uninitialized matrices.
   ;;
   ;; (uninitialized-matrix m n) → matrix
   ;; (uninitialized-matrix m) → square matrix
   ;; (uninitialized-u8matrix m n) → u8matrix
   ;; (uninitialized-u8matrix m) → square u8matrix
   ;;         ⋮
   ;; (uninitialized-c64matrix m n) → c64matrix
   ;; (uninitialized-c64matrix m) → square c64matrix
   ;; (typed-uninitialized-matrix type m n) → matrix
   ;; (typed-uninitialized-matrix type m) → square matrix
   uninitialized-matrix
   uninitialized-u8matrix
   uninitialized-s8matrix
   uninitialized-u16matrix
   uninitialized-s16matrix
   uninitialized-u32matrix
   uninitialized-s32matrix
   uninitialized-u64matrix
   uninitialized-s64matrix
   uninitialized-f32matrix
   uninitialized-f64matrix
   uninitialized-c32matrix
   uninitialized-c64matrix
   typed-uninitialized-matrix

   ;; Create matrices that have all entries equal to zero.
   ;;
   ;; (zero-matrix m n) → matrix
   ;; (zero-matrix m) → square matrix
   ;; (zero-u8matrix m n) → u8matrix
   ;; (zero-u8matrix m) → square u8matrix
   ;;         ⋮
   ;; (zero-c64matrix m n) → c64matrix
   ;; (zero-c64matrix m) → square c64matrix
   ;; (typed-zero-matrix type m n) → matrix
   ;; (typed-zero-matrix type m) → square matrix
   zero-matrix
   zero-u8matrix
   zero-s8matrix
   zero-u16matrix
   zero-s16matrix
   zero-u32matrix
   zero-s32matrix
   zero-u64matrix
   zero-s64matrix
   zero-f32matrix
   zero-f64matrix
   zero-c32matrix
   zero-c64matrix
   typed-zero-matrix

   ;; Create matrices that have all entries equal to one.
   ;;
   ;; (matrix-of-ones m n) → matrix
   ;; (matrix-of-ones m) → square matrix
   ;; (u8matrix-of-ones m n) → u8matrix
   ;; (u8matrix-of-ones m) → square u8matrix
   ;;         ⋮
   ;; (c64matrix-of-ones m n) → c64matrix
   ;; (c64matrix-of-ones m) → square c64matrix
   ;; (typed-matrix-of-ones type m n) → matrix
   ;; (typed-matrix-of-ones type m) → square matrix
   matrix-of-ones
   u8matrix-of-ones
   s8matrix-of-ones
   u16matrix-of-ones
   s16matrix-of-ones
   u32matrix-of-ones
   s32matrix-of-ones
   u64matrix-of-ones
   s64matrix-of-ones
   f32matrix-of-ones
   f64matrix-of-ones
   c32matrix-of-ones
   c64matrix-of-ones
   typed-matrix-of-ones

   ;; Create matrices that have all entries equal to some
   ;; scalar.
   ;;
   ;; (filled-matrix fill m n) → matrix
   ;; (filled-matrix fill m) → square matrix
   ;; (filled-u8matrix fill m n) → u8matrix
   ;; (filled-u8matrix fill m) → square u8matrix
   ;;         ⋮
   ;; (filled-c64matrix fill m n) → c64matrix
   ;; (filled-c64matrix fill m) → square c64matrix
   ;; (typed-filled-matrix type fill m n) → matrix
   ;; (typed-filled-matrix type fill m) → square matrix
   filled-matrix
   filled-u8matrix
   filled-s8matrix
   filled-u16matrix
   filled-s16matrix
   filled-u32matrix
   filled-s32matrix
   filled-u64matrix
   filled-s64matrix
   filled-f32matrix
   filled-f64matrix
   filled-c32matrix
   filled-c64matrix
   typed-filled-matrix

   ;; Create matrices that have all diagonal entries equal to
   ;; one and all other entries equal to zero.
   ;;
   ;; (I-matrix m n) → matrix
   ;; (I-matrix m) → square matrix
   ;; (I-u8matrix m n) → u8matrix
   ;; (I-u8matrix m) → square u8matrix
   ;;         ⋮
   ;; (I-c64matrix m n) → c64matrix
   ;; (I-c64matrix m) → square c64matrix
   ;; (typed-I-matrix type m n) → matrix
   ;; (typed-I-matrix type m) → square matrix
   I-matrix
   I-u8matrix
   I-s8matrix
   I-u16matrix
   I-s16matrix
   I-u32matrix
   I-s32matrix
   I-u64matrix
   I-s64matrix
   I-f32matrix
   I-f64matrix
   I-c32matrix
   I-c64matrix
   typed-I-matrix

   ;; Create matrices that have all diagonal entries equal to
   ;; some scalar and all other entries equal to zero.
   ;;
   ;; (scalar-matrix x m n) → matrix
   ;; (scalar-matrix x m) → square matrix
   ;; (scalar-u8matrix x m n) → u8matrix
   ;; (scalar-u8matrix x m) → square u8matrix
   ;;         ⋮
   ;; (scalar-c64matrix x m n) → c64matrix
   ;; (scalar-c64matrix x m) → square c64matrix
   ;; (typed-scalar-matrix type x m n) → matrix
   ;; (typed-scalar-matrix type x m) → square matrix
   scalar-matrix
   scalar-u8matrix
   scalar-s8matrix
   scalar-u16matrix
   scalar-s16matrix
   scalar-u32matrix
   scalar-s32matrix
   scalar-u64matrix
   scalar-s64matrix
   scalar-f32matrix
   scalar-f64matrix
   scalar-c32matrix
   scalar-c64matrix
   typed-scalar-matrix

   ;; Accepts both typed and untyped arrays and vectors; all must have
   ;; the same @code{matrix-dimensions}.
   typed-matrix-map  ; (typed-matrix-map type proc A₁ A₂ ...) → matrix

   ;; Create a matrix of a particular type, initialized to be equal to
   ;; a given matrix. These all accept both typed and untyped arrays
   ;; and vectors.
   ;;
   ;; (matrix->matrix A) → non-uniform matrix
   ;; (matrix->u8matrix A) → u8matrix
   ;;         ⋮
   ;; (matrix->c64matrix A) → c64matrix
   ;; (matrix->typed-matrix type A) → matrix
   matrix->matrix
   matrix->u8matrix
   matrix->s8matrix
   matrix->u16matrix
   matrix->s16matrix
   matrix->u32matrix
   matrix->s32matrix
   matrix->u64matrix
   matrix->s64matrix
   matrix->f32matrix
   matrix->f64matrix
   matrix->c32matrix
   matrix->c64matrix
   matrix->typed-matrix

   ;; (matrix-exact->inexact A) → non-uniform matrix
   ;; (matrix-inexact->exact A) → non-uniform matrix
   ;;
   ;; These accept both typed and untyped arrays and vectors.
   matrix-exact->inexact
   matrix-inexact->exact

   ;; (matrix-copy A) → matrix
   ;;
   ;; Accepts both typed and untyped arrays and vectors. Returns an
   ;; array with the same array rank and index bounds as the
   ;; original’s. (This is not true in general of our matrix routines;
   ;; we try to treat matrices as implicitly 2-dimensional and as
   ;; having arbitrary index bases.)
   matrix-copy

   ;; (row-matrix->diagonal-matrix row-matrix) → square matrix
   ;;
   ;; Accepts both typed and untyped arrays and vectors.
   row-matrix->diagonal-matrix

   ;; (matrix-ref A i j) → value
   ;; (matrix-0ref A i j) → value   (0-based indexing)
   ;; (matrix-1ref A i j) → value   (1-based indexing)
   ;;
   ;; These accept both typed and untyped arrays and vectors.
   matrix-ref
   matrix-0ref
   matrix-1ref

   ;; (matrix-set! A i j value) → *unspecified*
   ;; (matrix-0set! A i j value) → *unspecified*   (0-based indexing)
   ;; (matrix-1set! A i j value) → *unspecified*   (1-based indexing)
   ;;
   ;; These accept both typed and untyped arrays and vectors.
   matrix-set!
   matrix-0set!
   matrix-1set!

   ;; (matrix-1x1->scalar A) → scalar
   ;; (scalar->matrix x) → matrix
   ;; (scalar->u8matrix x) → u8matrix
   ;;         ⋮
   ;; (scalar->c64matrix x) → c64matrix
   ;; (scalar->typed-matrix type x) → matrix
   ;;
   ;; matrix-1x1->scalar accepts both typed and untyped arrays
   ;; and vectors.
   matrix-1x1->scalar
   scalar->matrix
   scalar->u8matrix
   scalar->s8matrix
   scalar->u16matrix
   scalar->s16matrix
   scalar->u32matrix
   scalar->s32matrix
   scalar->u64matrix
   scalar->s64matrix
   scalar->f32matrix
   scalar->f64matrix
   scalar->c32matrix
   scalar->c64matrix
   scalar->typed-matrix

   ;; Shared-array views of matrices and parts of
   ;; matrices. These accept both typed and untyped arrays and
   ;; vectors.
   matrix-0based       ; (matrix-0based A) → matrix
   matrix-1based       ; (matrix-1based A) → matrix
   matrix-row          ; (matrix-row A) → vector
   matrix-0row         ; (matrix-0row A) → vector   (0-based indexing)
   matrix-1row         ; (matrix-1row A) → vector   (1-based indexing)
   matrix-column-transpose      ; (matrix-column-transpose A) → vector
   matrix-0column-transpose ; (matrix-0column-transpose A) → vector   (0-based indexing)
   matrix-1column-transpose ; (matrix-0column-transpose A) → vector   (1-based indexing)
   matrix-column            ; (matrix-column A) → vector
   matrix-0column   ; (matrix-0column A) → vector   (0-based indexing)
   matrix-1column   ; (matrix-0column A) → vector   (1-based indexing)
   matrix-0block    ; (matrix-0block A i j m n) → matrix
   matrix-1block    ; (matrix-1block A i j m n) → matrix
   matrix-block     ; (matrix-block A i j m n) → matrix
   matrix-transpose ; (matrix-transpose A) → matrix  (preserves index bases of rank-2 arrays)
   matrix-diagonal  ; (matrix-transpose A) → vector
   matrix-as-rank2-array ; (matrix-as-rank2-array vector-or-array-matrix) → array-matrix
   matrix-as-min-rank-array ; (matrix-as-min-rank-array vector-or-array-matrix) → vector-or-array-matrix
   row-matrix->vector ; (row-matrix->vector vector-or-array-matrix) → vector

   ;; Predicates. These accept both typed and untyped arrays
   ;; and vectors.
   zero-matrix?        ; (zero-matrix? A) → boolean
   I-matrix?           ; (I-matrix? A) → boolean (allows non-square)
   matrix-of-ones?     ; (matrix-of-ones? A) → boolean
   matrix=?            ; (matrix=? A B) → boolean (numerical equality)
   matrix-eq?          ; (matrix-eq? A B) → boolean
   matrix-eqv?         ; (matrix-eqv? A B) → boolean
   matrix-equal?       ; (matrix-equal? A B) → boolean
   for-all-in-matrix   ; (for-all-in-matrix pred? A) → boolean
   exists-in-matrix    ; (exists-in-matrix pred? A) → boolean
   for-all-in-matrix-0ij   ; (for-all-in-matrix-0ij pred? A) → boolean
   exists-in-matrix-0ij    ; (exists-in-matrix-0ij pred? A) → boolean
   for-all-in-matrix-1ij   ; (for-all-in-matrix-1ij pred? A) → boolean
   exists-in-matrix-1ij    ; (exists-in-matrix-1ij pred? A) → boolean
   for-all-in-matrix-ij    ; (for-all-in-matrix-ij pred? A) → boolean
   exists-in-matrix-ij     ; (exists-in-matrix-ij pred? A) → boolean

   rank-deficiency-exception ; FIXME: Should we get rid of this?
   )

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (srfi :4))                 ; SRFI-4 uniform numeric vectors.

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension
     "init_guile_sortsmill_math_matrices_base"))

  (define (rank-deficiency-exception caller . irritants)
    (apply error caller (_ "rank-deficient matrix") irritants))

  ;;-----------------------------------------------------------------------

  (define-syntax define-initialized-matrix
    (syntax-rules ()
      [(_ name creator scalar)
       (define name
         (case-lambda
           [(m n) (creator scalar m n)]
           [(m)   (creator scalar m)]))]))

  (define-syntax define-typed-initialized-matrix
    (syntax-rules ()
      [(_ name creator scalar)
       (define name
         (case-lambda
           [(type m n) (creator type scalar m n)]
           [(type m)   (creator type scalar m)]))]))

  (define-initialized-matrix uninitialized-matrix filled-matrix *unspecified*)
  (define-initialized-matrix uninitialized-u8matrix filled-u8matrix *unspecified*)
  (define-initialized-matrix uninitialized-s8matrix filled-s8matrix *unspecified*)
  (define-initialized-matrix uninitialized-u16matrix filled-u16matrix *unspecified*)
  (define-initialized-matrix uninitialized-s16matrix filled-s16matrix *unspecified*)
  (define-initialized-matrix uninitialized-u32matrix filled-u32matrix *unspecified*)
  (define-initialized-matrix uninitialized-s32matrix filled-s32matrix *unspecified*)
  (define-initialized-matrix uninitialized-u64matrix filled-u64matrix *unspecified*)
  (define-initialized-matrix uninitialized-s64matrix filled-s64matrix *unspecified*)
  (define-initialized-matrix uninitialized-f32matrix filled-f32matrix *unspecified*)
  (define-initialized-matrix uninitialized-f64matrix filled-f64matrix *unspecified*)
  (define-initialized-matrix uninitialized-c32matrix filled-c32matrix *unspecified*)
  (define-initialized-matrix uninitialized-c64matrix filled-c64matrix *unspecified*)
  (define-typed-initialized-matrix typed-uninitialized-matrix typed-filled-matrix *unspecified*)

  (define-initialized-matrix matrix-of-ones filled-matrix 1)
  (define-initialized-matrix u8matrix-of-ones filled-u8matrix 1)
  (define-initialized-matrix s8matrix-of-ones filled-s8matrix 1)
  (define-initialized-matrix u16matrix-of-ones filled-u16matrix 1)
  (define-initialized-matrix s16matrix-of-ones filled-s16matrix 1)
  (define-initialized-matrix u32matrix-of-ones filled-u32matrix 1)
  (define-initialized-matrix s32matrix-of-ones filled-s32matrix 1)
  (define-initialized-matrix u64matrix-of-ones filled-u64matrix 1)
  (define-initialized-matrix s64matrix-of-ones filled-s64matrix 1)
  (define-initialized-matrix f32matrix-of-ones filled-f32matrix 1)
  (define-initialized-matrix f64matrix-of-ones filled-f64matrix 1)
  (define-initialized-matrix c32matrix-of-ones filled-c32matrix 1)
  (define-initialized-matrix c64matrix-of-ones filled-c64matrix 1)
  (define-typed-initialized-matrix typed-matrix-of-ones typed-filled-matrix 1)

  (define (matrix-of-ones? A)
    (for-all-in-matrix (lambda (x) (= x 1)) A))

  ;;-----------------------------------------------------------------------

  (define (typed-matrix-map type proc A . more-matrices)
    (if (null? more-matrices)
        [private:matrix-mapped-to-typed-matrix type A proc]
        [let* ([dimensions (matrix-dimensions A)]
               [B (apply make-typed-array type *unspecified* dimensions)]
               [prep-matrix (compose matrix-as-rank2-array matrix-0based)])
          (unless (for-all (lambda (A^)
                             (equal? (matrix-dimensions A^) dimensions))
                           more-matrices)
            (apply assertion-violation 'typed-matrix-map
                   "matrix dimension mismatch" A more-matrices))
          (apply array-map! B proc (prep-matrix A)
                 (map prep-matrix more-matrices))
          (if (= (car dimensions) 1)
              [row-matrix->vector B]
              B)] ))

  ;;-----------------------------------------------------------------------

  ) ;; end of library.
