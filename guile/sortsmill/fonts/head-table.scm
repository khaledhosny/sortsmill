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

(library (sortsmill fonts head-table)

  (export
   ;; (view:head-table-set! font-or-glyph-view key value) → *unspecified*
   view:head-table-set!

   ;; (view:head-table-ref font-or-glyph-view key) → string
   view:head-table-ref

   ;; (view:head-table-set-from-alist! font-or-glyph-view alist) → *unspecified*
   view:head-table-set-from-alist!

   ;; (view:head-table->alist font-or-glyph-view) → alist
   view:head-table->alist

   ;; (view:head-table-keys font-or-glyph-view) → list
   view:head-table-keys

   ;; (view:optimized-for-cleartype-set! font-or-glyph-view value) → *unspecified*
   ;; (view:optimized-for-cleartype? font-or-glyph-view) → boolean
   view:optimized-for-cleartype-set!
   view:optimized-for-cleartype?
   )

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_fonts_head_table"))

  ) ;; end of library.
