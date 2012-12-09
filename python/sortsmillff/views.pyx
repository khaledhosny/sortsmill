# -*- coding: utf-8; python-indent: 2 -*-

# Copyright (C) 2012 Barry Schwartz
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

import internal_types

#(use-modules
#   (sortsmillff internal-types)
#   (system foreign)
#   (ice-9 format)
#   )
#
#(export
#   font-view
#   font-view?
#   wrap-font-view
#   unwrap-font-view
#   glyph-view
#   glyph-view?
#   wrap-glyph-view
#   unwrap-glyph-view
#   font-view->ff:FontViewBase
#   ff:FontViewBase->font-view
#   glyph-view->ff:SplineChar
#   ff:SplineChar->glyph-view
#   )
#
#(define-wrapped-pointer-type font-view
#   font-view?
#   wrap-font-view unwrap-font-view
#   (lambda (fv port)
#      (let* ((fvb (font-view->ff:FontViewBase fv))
#             (sf (pointer->ff:SplineFont (ff:FontViewBase:sf-ref fvb))))
#         (format port "#<font-view ~s ~x>"
#            (pointer->string (ff:SplineFont:font-name-ref sf))
#            (pointer-address (unwrap-font-view fv))))))
#
#(define-wrapped-pointer-type glyph-view
#   glyph-view?
#   wrap-glyph-view unwrap-glyph-view
#   (lambda (gv port)
#      (let* ((sc (glyph-view->ff:SplineChar gv))
#             (sf (pointer->ff:SplineFont (ff:SplineChar:parent-ref sc))))
#         (format port "#<glyph-view ~s:~s ~x>"
#            (pointer->string (ff:SplineFont:font-name-ref sf))
#            (pointer->string (ff:SplineChar:name-ref sc))
#            (pointer-address (unwrap-glyph-view gv))))))
#
#(define (font-view->ff:FontViewBase fv)
#   (pointer->ff:FontViewBase (unwrap-font-view fv)))
#
#(define (ff:FontViewBase->font-view fvb)
#   (wrap-font-view (ff:FontViewBase->pointer fvb)))
#
#(define (glyph-view->ff:SplineChar gv)
#   (pointer->ff:SplineChar (unwrap-glyph-view gv)))
#
#(define (ff:SplineChar->glyph-view sc)
#   (wrap-glyph-view (ff:SplineChar->pointer sc)))
