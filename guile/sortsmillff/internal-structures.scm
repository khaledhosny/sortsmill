;; -*- coding: utf-8 -*-

;; Copyright (C) 2012 Barry Schwartz
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

(define-module (sortsmillff internal-structures)
  #:use-module (sortsmillff view)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:re-export (font-view
               font-view?
               wrap-font-view
               unwrap-font-view
               glyph-view
               glyph-view?
               wrap-glyph-view
               unwrap-glyph-view)
  #:export (font-view->ff:FontViewBase
            ff:FontViewBase->font-view
            glyph-view->ff:SplineChar
            ff:SplineChar->glyph-view

            ff:define-pointer-type

            ff:FontViewBase
            ff:FontViewBase?
            wrap-ff:FontViewBase
            unwrap-ff:FontViewBase

            ff:SplineChar
            ff:SplineChar?
            wrap-ff:SplineChar
            unwrap-ff:SplineChar

            ff:SplineFont
            ff:SplineFont?
            wrap-ff:SplineFont
            unwrap-ff:SplineFont

            ff:EncMap
            ff:EncMap?
            wrap-ff:EncMap
            unwrap-ff:EncMap

            ff:BDFFont
            ff:BDFFont?
            wrap-ff:BDFFont
            unwrap-ff:BDFFont
            ))

;;-------------------------------------------------------------------------
;;
;; Conversions between different wrappers for the same pointer types.
;;
;; The ‘font-view’ and ‘glyph-view’ wrappers are for use by users.
;;
;; The ‘ff:FontViewBase’ and ‘ff:SplineChar’ wrappers are for the
;; implementation.

(define (font-view->ff:FontViewBase fv)
  (wrap-ff:FontViewBase (unwrap-font-view fv)))

(define (ff:FontViewBase->font-view ff:fvb)
  (wrap-font-view (unwrap-ff:FontViewBase ff:fvb)))

(define (glyph-view->ff:SplineChar fv)
  (wrap-ff:SplineChar (unwrap-glyph-view fv)))

(define (ff:SplineChar->glyph-view ff:sc)
  (wrap-glyph-view (unwrap-ff:SplineChar ff:sc)))

;;-------------------------------------------------------------------------

(define-syntax-rule (ff:define-pointer-type type-name pred wrap unwrap)
  (define-wrapped-pointer-type type-name
    pred wrap unwrap
    (lambda (ptr port)
      (format port "#<~a ~x>"
              (record-type-name type-name)
              (pointer-address (unwrap ptr))))))

(ff:define-pointer-type ff:FontViewBase
                        ff:FontViewBase?
                        wrap-ff:FontViewBase
                        unwrap-ff:FontViewBase)

(ff:define-pointer-type ff:SplineChar
                        ff:SplineChar?
                        wrap-ff:SplineChar
                        unwrap-ff:SplineChar)

(ff:define-pointer-type ff:SplineFont
                        ff:SplineFont?
                        wrap-ff:SplineFont
                        unwrap-ff:SplineFont)

(ff:define-pointer-type ff:EncMap
                        ff:EncMap?
                        wrap-ff:EncMap
                        unwrap-ff:EncMap)

(ff:define-pointer-type ff:BDFFont
                        ff:BDFFont?
                        wrap-ff:BDFFont
                        unwrap-ff:BDFFont)

;;-------------------------------------------------------------------------

(load-extension "libguile-sortsmillff_fontforge"
                "init_guile_sortsmillff_internal_structures")

;;-------------------------------------------------------------------------
