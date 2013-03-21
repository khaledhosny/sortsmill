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

(library (sortsmill math polyspline roots)

  (export poly:sign-variations-f64
          poly:sign-variations-scm

          poly:budan-0_1-scm-mono

          poly:isolate-roots-scm-mono)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_roots"))

  #|
  (define (poly:isolate-roots-scm-mono poly)
    "The Vincent-Collins-Akritas method for isolating roots of a
square-free polynomial (with coefficients given in the ordinary
monomial basis) in the open interval (0,1). See
http://en.wikipedia.org/wiki/Vincent%27s_theorem"
    (vca-recursion poly 0 1))

  (define (vca-resursion poly a b)
    (case (poly:budan-0_1-scm-mono poly01)
      [(0) '()]
      [(1) `((,a . ,b))]
      [else 
       ] ))
  |#

  ) ;; end of library.
