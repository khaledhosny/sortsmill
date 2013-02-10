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
          vector-binomial-coefficients

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
          (only (guile) eval-when))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_polyspline"))

  ) ;; end of library.
