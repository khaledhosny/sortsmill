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

(library (sortsmill math polyspline ellipses)

  (export elliptic-arc? pointer->elliptic-arc elliptic-arc->pointer
          make-unit-circle-at-origin
          make-elliptic-arc
          make-ellipse
          elliptic-arc-bezier-path

          elliptic-arc-piecewise-bezier)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (system foreign)
          (ice-9 format))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_ellipses"))

  (define-wrapped-pointer-type elliptic-arc
    elliptic-arc? pointer->elliptic-arc elliptic-arc->pointer
    [lambda (obj port)
      (format port "#<elliptic-arc 0x~x>"
              (pointer-address (elliptic-arc->pointer obj)))] )

  ) ;; end of library.
