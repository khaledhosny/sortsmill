;; -*- mode: bee; coding: utf-8 -*-

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

(define-module (sortsmillff views))

(use-modules
   (sortsmillff fontforge-api)
   (system foreign)
   (ice-9 format)
   )

(export
   font-view
   font-view?
   pointer->font-view
   font-view->pointer
   glyph-view
   glyph-view?
   pointer->glyph-view
   glyph-view->pointer
   view->pointer
   font-view->FontViewBase
   FontViewBase->font-view
   glyph-view->CharViewBase
   CharViewBase->glyph-view
   )

(define-wrapped-pointer-type font-view
   font-view?
   pointer->font-view font-view->pointer
   (lambda (fv port)
      (let* ((fvb (font-view->FontViewBase fv))
             (sf (FontViewBase:sf-dref fvb)))
         (format port "#<font-view ~s 0x~x>"
            (pointer->string (SplineFont:font-name-ref sf))
            (pointer-address (font-view->pointer fv))))))

(define-wrapped-pointer-type glyph-view
   glyph-view?
   pointer->glyph-view glyph-view->pointer
   (lambda (gv port)
      (let* ((cvb (glyph-view->CharViewBase gv))
             (sc (CharViewBase:sc-dref cvb))
             (sf (SplineChar:parent-dref sc)))
         (format port "#<glyph-view ~s:~s 0x~x>"
            (pointer->string (SplineFont:font-name-ref sf))
            (pointer->string (SplineChar:name-ref sc))
            (pointer-address (glyph-view->pointer gv))))))

(define (view->pointer v)
   (cond ((font-view? v) (font-view->pointer v))
         ((glyph-view? v) (glyph-view->pointer v))
         (else (scm-error 'wrong-type-arg "view->pointer"
                  "Not a font-view or glyph-view: ~S"
                  (list v) (list v)))))

(define (font-view->FontViewBase fv)
   (pointer->FontViewBase (font-view->pointer fv)))

(define (FontViewBase->font-view fvb)
   (pointer->font-view (FontViewBase->pointer fvb)))

(define (glyph-view->CharViewBase gv)
   (pointer->CharViewBase (glyph-view->pointer gv)))

(define (CharViewBase->glyph-view sc)
   (pointer->glyph-view (CharViewBase->pointer sc)))

;;(load-extension "libguile-sortsmillff_fontforge"
;;   "init_guile_sortsmillff_views")
