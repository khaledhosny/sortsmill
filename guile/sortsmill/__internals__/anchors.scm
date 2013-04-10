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

(library (sortsmill __internals__ anchors)

  (export AnchorPointType->type-symbol
          type-symbol->AnchorPointType
          anchor-point-type-symbols
          anchor-point-type-index
          free-AnchorPoint-linked-list)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_internals_anchors"))

  (define anchor-point-type-symbols
    (make-enumeration '[mark base ligature base-mark entry exit] ))

  (define anchor-point-type-index
    (enum-set-indexer anchor-point-type-symbols))

  ) ;; end of library.
