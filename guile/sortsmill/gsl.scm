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

(library (sortsmill gsl)

  (export zero-based
          one-based
          f64matrix*
          f64matrix+
          f64matrix-)

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_gsl"))

  (define (zero-based mat)
    (match (array-shape mat)
      [[(0 _)] mat]
      [[(0 _) (0 _)] mat]
      [[(1 hi)]
       (make-shared-array mat (lambda (i) `[,(+ i 1)]) `[0 ,(- hi 1)] )]
      [[(1 hi1) (1 hi2)]
       (make-shared-array mat
                          (lambda (i j) `[,(+ i 1) ,(+ j 1)])
                          `[0 ,(- hi1 1)] `[0 ,(- hi2 1)] )]
      [else (assertion-violation
             'zero-based
             (_ "expected a uniformly zero-based or one-based vector or matrix")
             mat)] ))

  (define (one-based mat)
    (match (array-shape mat)
      [[(1 _)] mat]
      [[(1 _) (1 _)] mat]
      [[(0 hi)]
       (make-shared-array mat (lambda (i) `[,(- i 1)]) `[1 ,(+ hi 1)] )]
      [[(0 hi1) (0 hi2)]
       (make-shared-array mat
                          (lambda (i j) `[,(- i 1) ,(- j 1)])
                          `[1 ,(+ hi1 1)] `[1 ,(+ hi2 1)] )]
      [else (assertion-violation
             'one-based
             (_ "expected a uniformly zero-based or one-based vector or matrix")
             mat)] ))

  ) ;; end of library.
