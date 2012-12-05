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

(define-module (sortsmillff view)
  #:use-module (system foreign)
  #:export (font-view
            font-view?
            wrap-font-view
            unwrap-font-view
            glyph-view
            glyph-view?
            wrap-glyph-view
            unwrap-glyph-view
            ))

(define-wrapped-pointer-type font-view
  font-view?
  wrap-font-view unwrap-font-view
  (lambda (fv port)
    (simple-format port "#<font-view ~S>"
                   "FONT NAME GOES HERE")))

(define-wrapped-pointer-type glyph-view
  glyph-view?
  wrap-glyph-view unwrap-glyph-view
  (lambda (fv port)
    (simple-format port "#<glyph-view ~S:~S>"
                   "FONT NAME GOES HERE"
                   "GLYPH NAME GOES HERE")))
