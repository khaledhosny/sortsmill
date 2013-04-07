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

(library (sortsmill fonts anchors)

  (export

   ;; (glyph-view-anchor-points gv) → list of alists
   ;;
   ;; alist keys:
   ;;   'type → 'mark 'base 'ligature 'basemark 'entry 'exit 'unrecognized
   ;;   'name → string
   ;;   'coords → (list x y)
   ;;   'selected? → #f or #t
   ;;   'ligature-index → integer  (present if type is 'ligature)
   glyph-view-anchor-points
   )

  (import (sortsmill fonts views)
          (sortsmill fontforge-api)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) unfold)
          (system foreign))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_anchors"))

  (define (glyph-view-anchor-points gv)
    (let ([ap-list (glyph-view->AnchorPoint-list gv)])
      (map AnchorPoint->alist ap-list)))

  (define (glyph-view->AnchorPoint-list gv)
    (let ([sc (glyph-view->SplineChar gv)])
      (unfold null-pointer?
              pointer->AnchorPoint
              (compose AnchorPoint:next-ref pointer->AnchorPoint)
              (SplineChar:anchor-points-ref sc))))

  (define (AnchorPoint->alist ap)
    (let ([my-coords (AnchorPoint:coords-ref ap)]
          [ac (AnchorPoint:anchor-class-dref ap)])
      (let ([type (AnchorPoint->type-symbol ap)]
            [name (pointer->string (AnchorClass:name-ref ac) -1 "UTF-8")]
            [x    (BasePoint:x-ref my-coords)]
            [y    (BasePoint:y-ref my-coords)]
            [selected? (AnchorPoint:selected-ref ap)]
            [lig-index (AnchorPoint:lig-index-ref ap)])
        `([type      . ,type]
          [name      . ,name]
          [coords    . ,(list x y)]
          [selected? . ,selected?]
          ,@[if (eq? type 'ligature)
                `([ligature-index . ,lig-index])
                '()])
        )))

  (define (AnchorPoint-type->type-symbol type)
    (cond [(= type anchor-type:mark) 'mark]
          [(= type anchor-type:base) 'base]
          [(= type anchor-type:ligature) 'ligature]
          [(= type anchor-type:basemark) 'basemark]
          [(= type anchor-type:entry) 'entry]
          [(= type anchor-type:exit) 'exit]
          [else 'unrecognized]))

  (define (AnchorPoint->type-symbol ap)
    (AnchorPoint-type->type-symbol (AnchorPoint:type-ref ap)))
  
  ) ;; end of library.
