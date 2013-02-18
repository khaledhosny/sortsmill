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

(library (sortsmill alloc)

  (export c:zalloc ;; Allocate and fill with zeroes.
          c:free
          c:gc-zalloc ;; Garbage-collected.
          c:gc-free
          c:alloc-die
          c:alloc-die-on-null)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (system foreign))

  (define c:zalloc
    (pointer->procedure
     '*
     (sortsmill-dynlink-func "x_zalloc" "#include <sortsmill/x_alloc.h>")
     `(,size_t)))

  (define c:free
    (pointer->procedure
     void
     (sortsmill-dynlink-func "free" "#include <stdlib.h>")
     '(*)))

  (define c:gc-zalloc
    (pointer->procedure
     '*
     (sortsmill-dynlink-func "x_gc_malloc" "#include <sortsmill/xgc.h>")
     `(,size_t)))

  (define c:gc-free
    (pointer->procedure
     void
     (sortsmill-dynlink-func "GC_free" "#include <sortsmill/xgc.h>")
     '(*)))

  (define c:alloc-die
    (pointer->procedure
     void
     (sortsmill-dynlink-func "x_alloc_die" "#include <sortsmill/x_alloc.h>")
     '()))
    
  (define (c:alloc-die-on-null p)
    (when (null-pointer? p) (c:alloc-die))
    p)

  ) ;; end of library.
