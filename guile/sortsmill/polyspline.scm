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

(library (sortsmill polyspline)

  (export f64vector-binomial-coefficients
          f64vector-binomial-coefficients-altsigns
          vector-binomial-coefficients
          vector-binomial-coefficients-altsigns

          f64vector-sbern-basis-in-mono
          f64vector-mono-basis-in-sbern
          f64vector-sbern-basis-in-spower
          f64vector-spower-basis-in-sbern
          vector-sbern-basis-in-mono
          vector-mono-basis-in-sbern
          vector-sbern-basis-in-spower
          vector-spower-basis-in-sbern

          f64matrix-sbern-basis-in-mono
          f64matrix-mono-basis-in-sbern
          f64matrix-sbern-basis-in-spower
          f64matrix-spower-basis-in-sbern
          matrix-sbern-basis-in-mono
          matrix-mono-basis-in-sbern
          matrix-sbern-basis-in-spower
          matrix-spower-basis-in-sbern

          f64vector-sbern->bern
          f64vector-bern->sbern
          f64vector-sbern->mono
          f64vector-mono->sbern
          f64vector-bern->mono
          f64vector-mono->bern

          f64vector-eval-sbern
          f64vector-eval-bern
          f64vector-evaldc-sbern
          f64vector-evaldc-bern
          f64vector-eval-mono

          f64vector-subdiv-sbern
          f64vector-subdiv-bern

          f64vector-mul-sbern
          f64vector-mul-bern
          f64vector-mul-mono
          )

  (import (sortsmill dynlink)
          (rnrs)
          (only (guile) eval-when make-shared-array))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_polyspline"))

  (define (reshape-n*n n)
    (lambda (i j) (list (+ (* i n) j))))

  (define-syntax define-n*n-transformation-matrix
    (syntax-rules ()
      [(_ n*n vec)
       (define (n*n degree)
         (let ([n (+ degree 1)])
           (make-shared-array (vec degree) (reshape-n*n n) n n)))]))

  (define-n*n-transformation-matrix
    f64matrix-sbern-basis-in-mono f64vector-sbern-basis-in-mono)

  (define-n*n-transformation-matrix
    f64matrix-mono-basis-in-sbern f64vector-mono-basis-in-sbern)

  (define-n*n-transformation-matrix
    f64matrix-sbern-basis-in-spower f64vector-sbern-basis-in-spower)

  (define-n*n-transformation-matrix
    f64matrix-spower-basis-in-sbern f64vector-spower-basis-in-sbern)

  (define-n*n-transformation-matrix
    matrix-sbern-basis-in-mono vector-sbern-basis-in-mono)

  (define-n*n-transformation-matrix
    matrix-mono-basis-in-sbern vector-mono-basis-in-sbern)

  (define-n*n-transformation-matrix
    matrix-sbern-basis-in-spower vector-sbern-basis-in-spower)

  (define-n*n-transformation-matrix
    matrix-spower-basis-in-sbern vector-spower-basis-in-sbern)

  ) ;; end of library.
