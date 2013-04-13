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

(library (sortsmill fonts peg-spacing)

  (export
   peg-spacing-tolerance
   peg-spacing-rounding-function

   ;; (spacing-peg-name? anchor-name) → boolean
   spacing-peg-name?

   ;; (spacing-peg-side anchor-name) → 'left, 'right, or #f
   spacing-peg-side

   ;; (spacing-peg-modifier anchor-name) → 'kerning-only, 'special, or #f
   spacing-peg-modifier

   ;; (spacing-peg-identifier anchor-name) → string or #f
   spacing-peg-identifier

   ;; (spacing-pegs anchor-points-list) → anchor-points-list
   ;; (left-spacing-pegs anchor-points-list) → anchor-points-list
   ;; (right-spacing-pegs anchor-points-list) → anchor-points-list
   ;;                   ⋮
   spacing-pegs
   left-spacing-pegs
   right-spacing-pegs
   ordinary-spacing-pegs
   kerning-only-spacing-pegs
   special-spacing-pegs
   nonspecial-spacing-pegs
   left-ordinary-spacing-pegs
   right-ordinary-spacing-pegs

   ;; (within-peg-spacing-tolerance? real1 real2) → boolean
   within-peg-spacing-tolerance?

   peg-spacing-left-spacing
   peg-spacing-right-spacing
   )

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_peg_spacing"))

  (define peg-spacing-tolerance (make-fluid #f))
  (define peg-spacing-rounding-function (make-fluid identity))

  ) ;; end of library.
