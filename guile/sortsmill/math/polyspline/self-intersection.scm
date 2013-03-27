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

#|

(library (sortsmill math polyspline self-intersection)

  (export )

  (import (rnrs)
          (except (guile) error))

;;; Self-intersections can be found as the common solutions of
;;;
;;;    x(s) − x(t) = 0
;;;    y(s) − y(t) = 0
;;;
;;; ignoring the trivial solution s = t.
;;;
;;; Suppose
;;;
;;;    x(t) = x₀ + x₁t + x₂t² + x₃t³
;;;    y(t) = y₀ + y₁t + y₂t² + y₃t³
;;;
;;; Then
;;;
;;;    x(s) − x(t) = x₁(s − t) + x₂(s² − t²) + x₃(s³ − t³)
;;;                = x₁(s − t) + x₂(s − t)(s + t) + x₃(s − t)(s² + st + t²)
;;;                = (s − t)(x₁ + x₂(s + t) + x₃(s² + st + t²))
;;;
;;;    y(s) − y(t) = (s − t)(y₁ + y₂(s + t) + y₃(s² + st + t²))
;;;
;;; Thus solutions other than s = t have to satisfy
;;;
;;;    x₁ + x₂(s + t) + x₃(s² + st + t²) = 0
;;;    y₁ + y₂(s + t) + y₃(s² + st + t²) = 0

  ) ;; end of library.

|#
