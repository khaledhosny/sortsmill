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

(library (sortsmill fonts fontinfo-dict)

  (export
   ;; (view:fontinfo-dict-set! font-or-glyph-view key value) → *unspecified*
   view:fontinfo-dict-set!

   ;; (view:fontinfo-dict-ref font-or-glyph-view key) → string
   view:fontinfo-dict-ref

   ;; (view:fontinfo-dict-set-from-alist! font-or-glyph-view alist) → *unspecified*
   view:fontinfo-dict-set-from-alist!

   ;; (view:fontinfo-dict->alist font-or-glyph-view) → alist
   view:fontinfo-dict->alist

   ;; (view:fontinfo-dict-keys font-or-glyph-view) → list
   view:fontinfo-dict-keys

   ;; Convenience functions.
   ;; --------------------------------------------------------------------------------
   ;;
   ;; (view:FIELDNAME-ref font-or-glyph-view) → string or #f
   view:version-ref
   view:Notice-ref
   view:FullName-ref
   view:FamilyName-ref
   view:Weight-ref
   ;;
   ;; (view:FIELDNAME-ref font-or-glyph-view) → number, string, or #f
   view:ItalicAngle-ref
   view:UnderlinePosition-ref
   view:UnderlineThickness-ref
   ;;
   ;; (view:FIELDNAME-ref font-or-glyph-view) → boolean, string, or #f
   ;; WARNING: These do not distinguish a missing entry from an entry
   ;; set to "false".
   view:IsFixedPitch-ref
   ;;
   ;; (view:FIELDNAME-set! font-or-glyph-view value) → *unspecified*
   view:version-set!
   view:Notice-set!
   view:FullName-set!
   view:FamilyName-set!
   view:Weight-set!
   view:ItalicAngle-set!
   view:UnderlinePosition-set!
   view:UnderlineThickness-set!
   )

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_fontinfo_dict"))

  ) ;; end of library.
