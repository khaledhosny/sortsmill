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

(library (sortsmill fonts t1font-dict)

  (export
   ;; (view:t1font-dict-set! font-or-glyph-view key value) → *unspecified*
   view:t1font-dict-set!

   ;; (view:t1font-dict-ref font-or-glyph-view key) → string
   view:t1font-dict-ref

   ;; (view:t1font-dict-set-from-alist! font-or-glyph-view alist) → *unspecified*
   view:t1font-dict-set-from-alist!

   ;; (view:t1font-dict->alist font-or-glyph-view) → alist
   view:t1font-dict->alist

   ;; (view:t1font-dict-keys font-or-glyph-view) → list
   view:t1font-dict-keys

   ;; Convenience functions.
   ;; --------------------------------------------------------------------------------
   ;;
   ;; (view:FIELDNAME-ref font-or-glyph-view) → string or #f
   view:FontName-ref
   ;;
   ;; (view:FIELDNAME-ref font-or-glyph-view) → number, string, or #f
   view:StrokeWidth-ref
   ;;
   ;; (view:FIELDNAME-set! font-or-glyph-view value) → *unspecified*
   view:FontName-set!
   view:StrokeWidth-set!
   )

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_t1font_dict"))

  ) ;; end of library.
