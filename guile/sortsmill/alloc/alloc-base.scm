;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Barry Schwartz
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

(library (sortsmill alloc alloc-base)

  (export c:zalloc ;; Allocate and fill with zeroes.
          c:free
          c:gc-zalloc ;; Garbage-collected.
          c:gc-free)

  (import (guile))

  (eval-when (compile load eval)
    (load-extension "libguile-sortsmill_aux" "init_libguile_sortsmill_aux"))

  ) ;; end of library.
