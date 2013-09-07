;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
;; This file is part of the Sorts Mill Tools.
;; 
;; Sorts Mill Tools is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Sorts Mill Tools is distributed in the hope that it will be useful,
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
          font-view->SplineFont
          view->SplineFont
          view->FontViewBase

          ;; Legal flags: 'fsType-permitted, 'all-glyphs-in-ttc,
          ;; 'lint, 'hide-window
          make-font ;; (make-font [optional kwargs]) → font-view
          open-font ;; (open-font file-name [flag1 flag2 ...]) → font-view
          open-font-hidden ;; (open-font-hidden file-name [flag1 flag2 ...]) → font-view
          )

  (import (sortsmill fontforge-api)
          (sortsmill i18n)
          (sortsmill dynlink)
          (sortsmill core)
          (rnrs)
          (only (guile) compose eval-when)
          (system foreign)
          (ice-9 format))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_views"))

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
          [else raise-not-a-view 'view->pointer v]))

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

  (define (font-view->SplineFont fv)
    (let* ([fvb (font-view->FontViewBase fv)]
           [sf (FontViewBase:sf-dref fvb)])
      sf))

  (define (view->SplineFont v)
    (cond [(font-view? v) (font-view->SplineFont v)]
          [(glyph-view? v)
           (SplineChar:parent-dref (glyph-view->SplineChar v))]
          [else raise-not-a-view 'view->SplineFont v]))

  (define (view->FontViewBase v)
    (cond [(font-view? v) (font-view->FontViewBase v)]
          [(glyph-view? v)
           (SplineFont:fv-dref
            (SplineChar:parent-dref (glyph-view->SplineChar v)))]
          [else raise-not-a-view 'view->FontViewBase v]))

  (define (raise-not-a-view who obj)
    (assertion-violation who (_ "expected a font-view or glyph-view") obj))

  (define/kwargs (make-font [encoding #f]
                            [foreground-degree #f]
                            [background-degree #f]
                            [guide-layer-degree #f]
                            [hide #f])
    (private:make-font encoding foreground-degree background-degree
                       guide-layer-degree hide))

  ) ;; end of library.
