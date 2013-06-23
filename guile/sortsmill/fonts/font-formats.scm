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

(library (sortsmill fonts font-formats)

  (export
   integer->font-format ;; (integer->font-format integer) → symbol or #f
   font-format->integer ;; (font-format->integer symbol) → integer or #f
   )

  ;; font-format symbols:
  ;;
  ;;    'pfa
  ;;    'pfb
  ;;    'pfb-macbin
  ;;    'pfb-multiple
  ;;    'mma
  ;;    'mmb
  ;;    'type3
  ;;    'type0
  ;;    'cid
  ;;    'cff
  ;;    'cff-cid
  ;;    'type42
  ;;    'type11  (CID-indexed Type 42)
  ;;    'ttf
  ;;    'ttf-symbol
  ;;    'ttf-macbin
  ;;    'ttc
  ;;    'ttf-dfont
  ;;    'otf
  ;;    'otf-dfont
  ;;    'otf-cid
  ;;    'otf-cid-dfont
  ;;    'svg
  ;;    'ufo
  ;;    'woff
  ;;    'no-font

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_sortsmill_guile_fonts_font_formats"))

  ) ;; end of library.
