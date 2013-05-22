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

(library (sortsmill math polyspline reduce)

  (export poly:min-degree-f64-mono
          poly:min-degree-scm-mono
          poly:min-degree-f64-spower
          poly:min-degree-scm-spower

          poly:reduce-degree-f64-mono
          poly:reduce-degree-scm-mono
          poly:reduce-degree-f64-spower
          poly:reduce-degree-scm-spower

          poly:reduce-to-min-degree-f64-mono
          poly:reduce-to-min-degree-scm-mono
          poly:reduce-to-min-degree-f64-spower
          poly:reduce-to-min-degree-scm-spower)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_reduce"))

  ) ;; end of library.
