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

(library (sortsmill fonts views)

  (export font-view-flag
          glyph-view-flag

          font-view?
          pointer->font-view
          font-view->pointer

          glyph-view?
          pointer->glyph-view
          glyph-view->pointer

          view?
          pointer->view
          view->pointer

          glyph-view->CharViewBase
          CharViewBase->glyph-view

          font-view->FontViewBase
          FontViewBase->font-view

          glyph-view->ViewBase
          ViewBase->glyph-view
          font-view->ViewBase
          ViewBase->font-view

          ViewBase->CharViewBase
          CharViewBase->ViewBase
          ViewBase->FontViewBase
          FontViewBase->ViewBase

          ;; Convenience functions.
          glyph-view->SplineChar
          font-view->SplineFont)

  (import (sortsmill fontforge-api)
          (sortsmill i18n)
          (rnrs)
          (only (guile) compose)
          (system foreign)
          (ice-9 format))

  (define font-view-flag 1)
  (define glyph-view-flag 2)

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

  (define (view? v)
    (or (glyph-view? v) (font-view? v)))

  (define (pointer->view p)
    (let ((tag (ViewBase:tag-ref (pointer->ViewBase p))))
      (cond
       ((= tag glyph-view-flag) (pointer->glyph-view p))
       ((= tag font-view-flag) (pointer->font-view p))
       (else (assertion-violation
              'pointer->view
              (format #f (_ "illegal ViewBase tag ‘~d’ at address") tag)
              p)))))

  (define (view->pointer v)
    (cond ((glyph-view? v) (glyph-view->pointer v))
          ((font-view? v) (font-view->pointer v))
          (else (assertion-violation
                 'view->pointer
                 (_ "expected a font-view or glyph-view") v))))

  (define font-view->FontViewBase
    (compose pointer->FontViewBase font-view->pointer))

  (define FontViewBase->font-view
    (compose pointer->font-view FontViewBase->pointer))

  (define glyph-view->CharViewBase
    (compose pointer->CharViewBase glyph-view->pointer))

  (define CharViewBase->glyph-view
    (compose pointer->glyph-view CharViewBase->pointer))

  (define glyph-view->ViewBase
    (compose pointer->ViewBase glyph-view->pointer))

  (define ViewBase->glyph-view
    (compose pointer->glyph-view ViewBase->pointer))

  (define font-view->ViewBase
    (compose pointer->ViewBase font-view->pointer))

  (define ViewBase->font-view
    (compose pointer->font-view ViewBase->pointer))

  (define ViewBase->CharViewBase
    (compose pointer->CharViewBase ViewBase->pointer))

  (define CharViewBase->ViewBase
    (compose pointer->ViewBase CharViewBase->pointer))

  (define ViewBase->FontViewBase
    (compose pointer->FontViewBase ViewBase->pointer))

  (define FontViewBase->ViewBase
    (compose pointer->ViewBase FontViewBase->pointer))

  (define (glyph-view->SplineChar gv)
    (let* ([cvb (glyph-view->CharViewBase gv)]
           [sc (CharViewBase:sc-dref cvb)])
      sc))

  (define (font-view->SplineFont gv)
    (let* ([fvb (font-view->FontViewBase gv)]
           [sf (FontViewBase:sf-dref fvb)])
      sf))

  ) ;; end of library.
