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

(library (sortsmill containers rbmap)

  (export make-rbmapi
          rbmapi?
          rbmapi-set!
          rbmapi-delete!
          rbmapi-ref
          rbmapi-fold-left
          rbmapi-fold-right
          alist->rbmapi
          rbmapi->alist ;; rbmapi->alist preserves the order.
          rbmapi-map->list ;; rbmapi-map->list preserves the order.
          rbmapi-for-each
          rbmapi-count
          rbmapi-size)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (system foreign)
          (ice-9 format))

  (define-wrapped-pointer-type rbmapi
    rbmapi? pointer->rbmapi rbmapi->pointer
    [lambda (obj port)
      (format port "#<rbmapi 0x~x>"
              (pointer-address (rbmapi->pointer obj)))] )

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_sortsmill_guile_rbmap"))

  ) ;; end of library.
