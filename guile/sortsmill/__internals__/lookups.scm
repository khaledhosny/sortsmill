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

(library (sortsmill __internals__ lookups)

  (export OTLookupType->type-symbol
          type-symbol->OTLookupType
          lookup-type-symbols
          lookup-type-index)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_internals_lookups"))

  (define lookup-type-symbols
    (make-enumeration '[gsub-single
                        gsub-multiple
                        gsub-alternate
                        gsub-ligature
                        gsub-context
                        gsub-chaining-contextual
                        gsub-extension
                        gsub-reverse-chaining-contextual
                        gpos-single
                        gpos-pair
                        gpos-cursive
                        gpos-mark-to-base
                        gpos-mark-to-ligature
                        gpos-mark-to-mark
                        gpos-context
                        gpos-chaining-contextual
                        gpos-extension] ))

  (define lookup-type-index
    (enum-set-indexer lookup-type-symbols))

  ) ;; end of library.
