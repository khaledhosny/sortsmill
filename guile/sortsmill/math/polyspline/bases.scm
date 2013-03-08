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

(library (sortsmill math polyspline bases)

  (export poly:basis-transformation-set!
          poly:basis-transformation
          poly:change-basis
          poly:spower-halves)

  (import (sortsmill math matrices)
          (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (system foreign)
          (only (srfi :26) cut))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_bases"))

  ;;------------------------------------------------------------------------

  (define poly:basis-transformations (make-hashtable equal-hash equal?))

  (define (poly:basis-transformation-set! from-coefficients
                                          to-coefficients
                                          matrix-proc)
    (hashtable-set! poly:basis-transformations
                    `(,from-coefficients . ,to-coefficients) matrix-proc))

  (define (poly:basis-transformation-ref from-coefficients
                                         to-coefficients)
    (hashtable-ref poly:basis-transformations
                   `(,from-coefficients . ,to-coefficients) #f))

  (define (poly:basis-transformation from-coefficients
                                     to-coefficients
                                     degree)
    (let ([proc (poly:basis-transformation-ref from-coefficients
                                               to-coefficients)])
      (if proc
          (proc degree)
          (error 'poly:basis-transformation
                 (_ "transformation matrix not found")
                 `(,from-coefficients ,to-coefficients ,degree)))))

  (define (poly:change-basis from to coefs)
    (let ([degree (- (matrix-column-count coefs) 1)])
      (matrix* coefs (poly:basis-transformation from to degree))))

  (for-each
   (cut apply poly:basis-transformation-set! <...>)
   `(
     (mono mono ,coefficients_mono_to_mono)
     (mono bern ,coefficients_mono_to_bern)
     (mono sbern ,coefficients_mono_to_sbern)
     (mono spower ,coefficients_mono_to_spower)

     (bern mono ,coefficients_bern_to_mono)
     (bern bern ,coefficients_bern_to_bern)
     (bern sbern ,coefficients_bern_to_sbern)
     (bern spower ,coefficients_bern_to_spower)

     (sbern mono ,coefficients_sbern_to_mono)
     (sbern bern ,coefficients_sbern_to_bern)
     (sbern sbern ,coefficients_sbern_to_sbern)
     (sbern spower ,coefficients_sbern_to_spower)

     (spower mono ,coefficients_spower_to_mono)
     (spower bern ,coefficients_spower_to_bern)
     (spower sbern ,coefficients_spower_to_sbern)
     (spower spower ,coefficients_spower_to_spower) ))

  ;;------------------------------------------------------------------------

  (define (poly:spower-halves coefs)
    "Shared array views of the symmetric halves of s-power splines. If
the spline degree is even, then both halves will contain the middle
coefficient as their last (highest degree) coefficient."
    (let* ([coefs (vector->matrix (one-based coefs))]
           [dims (matrix-dimensions coefs)]
           [row-count (car dims)]
           [column-count (cadr dims)]
           [degree (- column-count 1)]
           [quot (div degree 2)])
      (let ([left-half (make-shared-array coefs list
                                          `(1 ,row-count)
                                          `(1 ,(+ quot 1)))]
            [right-half (make-shared-array coefs
                                           (lambda (i j)
                                             `(,i  ,(- column-count j -1)))
                                           `(1 ,row-count)
                                           `(1 ,(+ quot 1)))])
            (values left-half right-half))))

  ;;------------------------------------------------------------------------

  ) ;; end of library.
