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

  (export polyspline-basis-transformation-set!
          polyspline-basis-transformation)

  (import (sortsmill dynlink)
          (sortsmill math matrices base)
          (sortsmill math matrices mpqmat)
          (rnrs)
          (except (guile) error)
          (system foreign)
          (only (srfi :26) cut))

  (sortsmill-dynlink-declarations "#include <sortsmill/math/polyspline/bases.h>")

  ;;------------------------------------------------------------------------

  (define polyspline-basis-transformations (make-hashtable equal-hash equal?))

  (define (polyspline-basis-transformation-set! from-coefficients
                                                to-coefficients
                                                matrix-proc)
    (hashtable-set! polyspline-basis-transformations
                    `(,from-coefficients . ,to-coefficients) matrix-proc))

  (define (polyspline-basis-transformation-ref from-coefficients
                                               to-coefficients)
    (hashtable-ref polyspline-basis-transformations
                   `(,from-coefficients . ,to-coefficients) #f))

  (define (polyspline-basis-transformation from-coefficients
                                           to-coefficients
                                           degree)
    (let ([proc (polyspline-basis-transformation-ref from-coefficients
                                                     to-coefficients)])
      (proc degree)))

  (define (basis->same-basis degree)
    (I-matrix (+ degree 1)))

  (define (basis->basis:c-func c-func)
    (let ([proc (pointer->procedure
                 '* (sortsmill-dynlink-func c-func) `[,unsigned-int])])
      (lambda (degree)
        (mpqmat->matrix (pointer->mpqmat (proc degree))))))

  (for-each
   (cut apply polyspline-basis-transformation-set! <...>)
   `(
     (mono mono ,basis->same-basis)
     (mono bern ,(basis->basis:c-func "coefficients_mono_to_bern"))
     (mono sbern ,(basis->basis:c-func "coefficients_mono_to_sbern"))
     (mono spower #f)

     (bern mono ,(basis->basis:c-func "coefficients_bern_to_mono"))
     (bern bern ,basis->same-basis)
     (bern sbern ,(basis->basis:c-func "coefficients_bern_to_sbern"))
     (bern spower #f)

     (sbern mono ,(basis->basis:c-func "coefficients_sbern_to_mono"))
     (sbern bern ,(basis->basis:c-func "coefficients_sbern_to_bern"))
     (sbern sbern ,basis->same-basis)
     (sbern spower #f)

     (spower mono #f)
     (spower bern #f)
     (spower sbern #f)
     (spower spower ,basis->same-basis)
     ))

  ;;------------------------------------------------------------------------

  ) ;; end of library.
