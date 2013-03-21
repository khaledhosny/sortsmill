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

(library (sortsmill math polyspline div)

  (export poly:div-f64-mono
          poly:div-scm-mono

          poly:gcd-f64-mono
          poly:gcd-scm-mono

          poly:sqfr-scm-mono)

  (import (sortsmill math polyspline add)
          (sortsmill math polyspline deriv)
          (sortsmill math matrices)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_div"))

  (define (poly:sqfr-scm-mono poly)
    "Yun’s algorithm for the square-free decomposition of a univariate
polynomial. See http://en.wikipedia.org/wiki/Square-free_polynomial"
    (let* ([poly (matrix-inexact->exact poly)]
           [poly^ (poly:deriv-scm-mono poly)]
           [a0 (poly:gcd-scm-mono poly poly^)])
      (match (one-based a0)
        [#@1(0) (list #@1(0))]
        [_      (let* ([b (poly:div-scm-mono poly a0)]
                       [c (poly:div-scm-mono poly^ a0)]
                       [d (poly:sub-scm-mono c (poly:deriv-scm-mono b))])
                  (yun-recursion '() b c d))] )))

  (define (yun-recursion prior b c d)
    (let* ([aa (poly:gcd-scm-mono b d)]
           [bb (poly:div-scm-mono b aa)])
      (match (one-based bb)
        [#@1(1) (reverse (cons aa prior))]
        [_      (let* ([cc (poly:div-scm-mono d aa)]
                       [dd (poly:sub-scm-mono cc (poly:deriv-scm-mono bb))])
                  (yun-recursion (cons aa prior) bb cc dd)) ] )))

  ) ;; end of library.
