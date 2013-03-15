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

(library (sortsmill math polyspline add)

  (export poly:add-f64-splines
          poly:add-scm-splines

          poly:sub-f64-splines
          poly:sub-scm-splines

          poly:add-f64-mono
          poly:add-scm-mono

          poly:add-f64-bern
          poly:add-scm-bern

          poly:add-f64-sbern
          poly:add-scm-sbern

          poly:add-f64-spower
          poly:add-scm-spower

          poly:sub-f64-mono
          poly:sub-scm-mono

          poly:sub-f64-bern
          poly:sub-scm-bern

          poly:sub-f64-sbern
          poly:sub-scm-sbern

          poly:sub-f64-spower
          poly:sub-scm-spower)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_add"))

  ) ;; end of library.
