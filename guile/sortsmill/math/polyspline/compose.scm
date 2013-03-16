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

(library (sortsmill math polyspline compose)

  (export poly:compose-f64-mono
          poly:compose-scm-mono

          poly:compose-f64-bern-de-casteljau
          poly:compose-scm-bern-de-casteljau

          poly:compose-f64-bern-horner
          poly:compose-scm-bern-horner

          poly:compose-f64-sbern-de-casteljau
          poly:compose-scm-sbern-de-casteljau

          poly:compose-f64-sbern-horner
          poly:compose-scm-sbern-horner

          poly:compose-f64-spower
          poly:compose-scm-spower)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_compose"))

  ) ;; end of library.
