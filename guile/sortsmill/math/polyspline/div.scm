;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2013 Khaled Hosny and Barry Schwartz
;; This file is part of the Sorts Mill Tools.
;; 
;; Sorts Mill Tools is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Sorts Mill Tools is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sortsmill math polyspline div)

  (export poly:div-f64-mono
          poly:div-scm-mono

          poly:gcd-f64-mono
          poly:gcd-scm-mono

          poly:sqfr-scm-mono)

  (import (sortsmill math polyspline add)
          (sortsmill math polyspline deriv)
          (sortsmill math polyspline reduce)
          (sortsmill math matrices)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) drop-while)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_div"))

  (define (poly:sqfr-scm-mono poly)
    "Yun’s algorithm for the square-free decomposition of a univariate
polynomial. See http://en.wikipedia.org/wiki/Square-free_polynomial

This version first reduces the polynomial to minimum (actual) degree,
and then normalizes the polynomial to monic form. There are two return
values: the square-free decomposition of the monic polynomial, and the
original lead coefficient of the minimum degree polynomial. If the
polynomial is identically zero, then the return values are @code{#(0)}
and @code{0}, respectively."
    (let* ([f (matrix-0based (poly:reduce-to-min-degree-scm-mono
                              (matrix-inexact->exact poly)))])
      (match f
        [#(0) (values (list #(0)) 0)]
        [_ (let* ([lead-coef (matrix-0ref f 0 (- (row-matrix-size f) 1))]
                  [f  (typed-matrix-map #t (lambda (x) (/ x lead-coef)) f)]
                  [f^ (poly:deriv-scm-mono f)]
                  [a0 (poly:gcd-scm-mono f f^)]
                  [b1 (poly:div-scm-mono f a0)]
                  [c1 (poly:div-scm-mono f^ a0)])
             (match (matrix-0based b1)
               [#(1) (values (list #(1)) lead-coef)]
               [_ (values (reverse (yun-recursion '() b1 c1))
                          lead-coef)] ))] )))

  (define (yun-recursion prior b c)
    (let* ([b^ (poly:deriv-scm-mono b)]
           [c-b^ (poly:sub-scm-mono c b^)]
           [a (poly:gcd-scm-mono b c-b^)]
           [next (cons a prior)]
           [b-next (poly:div-scm-mono b a)])
      (match (matrix-0based b-next)
        [#(1) next]
        [_ (yun-recursion next b-next (poly:div-scm-mono c-b^ a))] )))

  ) ;; end of library.
