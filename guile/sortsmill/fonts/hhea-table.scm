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

(library (sortsmill fonts hhea-table)

  (export
   ;; (view:hhea-table-set! font-or-glyph-view key value) → *unspecified*
   ;; (view:hhea-table-set! font-or-glyph-view key value value-is-offset?) → *unspecified*
   view:hhea-table-set!

   ;; (view:hhea-table-ref font-or-glyph-view key) → string
   ;; (view:hhea-table-ref font-or-glyph-view key value-is-offset?) → string
   view:hhea-table-ref

   ;; (view:hhea-table-set-from-alist! font-or-glyph-view alist) → *unspecified*
   view:hhea-table-set-from-alist!

   ;; (view:hhea-table->alist font-or-glyph-view) → alist
   view:hhea-table->alist

   ;; (view:hhea-table-keys font-or-glyph-view) → list
   view:hhea-table-keys
   )

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_hhea_table"))

  ) ;; end of library.
