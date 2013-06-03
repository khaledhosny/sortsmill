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

(library (sortsmill fonts private-dict)

  (export
   ;; (view:private-dict-set! font-or-glyph-view key value) → *unspecified*
   view:private-dict-set!

   ;; (view:private-dict-ref font-or-glyph-view key) → string
   view:private-dict-ref

   ;; (view:private-dict-ref font-or-glyph-view key) → *unspecified*
   view:private-dict-remove!

   ;; (view:private-dict-clear! font-or-glyph-view) → *unspecified*
   view:private-dict-clear!

   ;; (view:private-dict-set-from-alist! font-or-glyph-view alist) → *unspecified*
   view:private-dict-set-from-alist!

   ;; (view:alist->private-dict! font-or-glyph-view alist) → *unspecified*
   view:alist->private-dict!

   ;; (view:private-dict->alist font-or-glyph-view) → alist
   view:private-dict->alist

   ;; (view:private-dict-keys font-or-glyph-view) → list
   view:private-dict-keys

   ;; Convenience functions.
   ;; --------------------------------------------------------------------------------
   ;;
   ;; (view:FIELDNAME-ref font-or-glyph-view) → number-list, string, or #f
   view:BlueValues-ref
   view:OtherBlues-ref
   view:FamilyBlues-ref
   view:FamilyOtherBlues-ref
   view:StdHW-ref
   view:StdVW-ref
   view:StemSnapH-ref
   view:StemSnapV-ref
   view:BlueFuzz-ref
   view:BlueScale-ref
   view:BlueShift-ref
   view:ExpansionFactor-ref
   view:LanguageGroup-ref
   view:ForceBold-ref ; Does not distinguish a missing entry from an entry set to "false".
   view:RndStemUp-ref ; Does not distinguish a missing entry from an entry set to "false".
   ;;
   ;; (view:FIELDNAME-set! font-or-glyph-view value) → *unspecified*
   view:BlueValues-set!
   view:OtherBlues-set!
   view:FamilyBlues-set!
   view:FamilyOtherBlues-set!
   view:StdHW-set!
   view:StdVW-set!
   view:StemSnapH-set!
   view:StemSnapV-set!
   view:BlueFuzz-set!
   view:BlueScale-set!
   view:BlueShift-set!
   view:ExpansionFactor-set!
   view:LanguageGroup-set!
   view:ForceBold-set!
   view:RndStemUp-set!
   ;;
   ;; (view:FIELDNAME-remove! font-or-glyph-view) → *unspecified*
   view:BlueValues-remove!
   view:OtherBlues-remove!
   view:FamilyBlues-remove!
   view:FamilyOtherBlues-remove!
   view:StdHW-remove!
   view:StdVW-remove!
   view:StemSnapH-remove!
   view:StemSnapV-remove!
   view:BlueFuzz-remove!
   view:BlueScale-remove!
   view:BlueShift-remove!
   view:ExpansionFactor-remove!
   view:LanguageGroup-remove!
   view:ForceBold-remove!
   view:RndStemUp-remove!
   )

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_private_dict"))

  ) ;; end of library.
