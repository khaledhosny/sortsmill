;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

(library (sortsmill fonts general)

  (export view:ascent-ref
          view:ascent-set!
          view:descent-ref
          view:descent-set!

          ;; (view:units-per-em view) → ascent+descent
          view:units-per-em

          ;; Procedures reëxported from (sortsmill fonts
          ;; fontinfo-dict).
          ;;
          ;; These happen to correspond to objects in the
          ;; Element→Font Info→General dialog, at the time of this
          ;; writing, and this module was named after that
          ;; dialog. Otherwise there is no real reason for including
          ;; them here.
          view:ItalicAngle-ref
          view:ItalicAngle-set!
          view:UnderlinePosition-ref
          view:UnderlinePosition-set!
          view:UnderlineThickness-ref
          view:UnderlineThickness-set!

          ;; Some values that appear in UFO files whose meaning is
          ;; unclear. (These may go away; they are here because the
          ;; author of FontForge did not know what else to do with
          ;; some UFO fields except store them and write them back
          ;; out. We do not recommend the use of UFO, anyway.)
          view:ufo-ascent-ref
          view:ufo-ascent-set!
          view:ufo-descent-ref
          view:ufo-descent-set!
          )

  (import (sortsmill fonts views)
          (sortsmill fonts fontinfo-dict) ; For reëxportation.
          (sortsmill fontforge-api)
          (rnrs)
          (except (guile) error))

  (define (view:ascent-ref view)
    (SplineFont:ascent-ref (view->SplineFont view)))

  (define (view:ascent-set! view value)
    (SplineFont:ascent-set! (view->SplineFont view) value))

  (define (view:descent-ref view)
    (SplineFont:descent-ref (view->SplineFont view)))

  (define (view:descent-set! view value)
    (SplineFont:descent-set! (view->SplineFont view) value))

  (define (view:units-per-em view)
    (let ([sf (view->SplineFont view)])
      (+ (SplineFont:ascent-ref sf) (SplineFont:descent-ref sf))))

  (define (view:ufo-ascent-ref view)
    (SplineFont:ufo-ascent-ref (view->SplineFont view)))

  (define (view:ufo-ascent-set! view value)
    (SplineFont:ufo-ascent-set! (view->SplineFont view) value))

  (define (view:ufo-descent-ref view)
    (SplineFont:ufo-descent-ref (view->SplineFont view)))

  (define (view:ufo-descent-set! view value)
    (SplineFont:ufo-descent-set! (view->SplineFont view) value))

  ) ;; end of library.
