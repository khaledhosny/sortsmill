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

          ;; Budan’s 0_1 roots test.
          poly:budan-0_1-scm-mono

          ;; Isolate roots of a square-free polynomial in the interval
          ;; [0,1]. You want to do this with exact arithmetic. The
          ;; intervals are returned as an ordered list. Intervals of
          ;; length zero are closed; intervals of non-zero length are
          ;; open.
          poly:isolate-roots-scm-mono

          poly:find-bracketed-root-scm-mono-exact)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_roots"))

  ) ;; end of library.
