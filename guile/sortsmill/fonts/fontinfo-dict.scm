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

   ;; (view:fontinfo-dict-ref font-or-glyph-view key) → *unspecified*
   view:fontinfo-dict-remove!

   ;; (view:fontinfo-dict-clear! font-or-glyph-view) → *unspecified*
   view:fontinfo-dict-clear!

   ;; (view:fontinfo-dict-set-from-alist! font-or-glyph-view alist) → *unspecified*
   view:fontinfo-dict-set-from-alist!

   ;; (view:alist->fontinfo-dict! font-or-glyph-view alist) → *unspecified*
   view:alist->fontinfo-dict!

   ;; (view:fontinfo-dict->alist font-or-glyph-view) → alist
   view:fontinfo-dict->alist

   ;; (view:fontinfo-dict-keys font-or-glyph-view) → list
   view:fontinfo-dict-keys

   ;; (fontinfo-dict-value->string various-types) → string
   fontinfo-dict-value->string
   )

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 format))

;;  (eval-when (compile load eval)
;;    (sortsmill-dynlink-load-extension "init_guile_fonts_fontinfo_dict"))

  ) ;; end of library.
