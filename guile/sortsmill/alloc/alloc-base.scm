;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
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

(library (sortsmill alloc alloc-base)

  (export c:zalloc ;; Allocate and fill with zeroes.
          c:free
          c:gc-zalloc ;; Garbage-collected.
          c:gc-free)

  (import (rnrs)
          (except (guile) error)
          (system foreign))

  (eval-when (compile load eval)

    (define libsortsmill-core (dynamic-link "libsortsmill_core"))

    (define c:zalloc
      (pointer->procedure '*
                          (dynamic-func "x_zalloc" libsortsmill-core)
                          `(,size_t)))

    (define c:free
      (pointer->procedure void
                          (dynamic-func "x_free" libsortsmill-core)
                          '(*)))

    (define c:gc-zalloc
      (pointer->procedure '*
                          (dynamic-func "x_gc_malloc" libsortsmill-core)
                          `(,size_t)))

    (define c:gc-free
      (pointer->procedure void
                          (dynamic-func "x_gc_free" libsortsmill-core)
                          '(*)))

    ) ;; end eval-when

;;;  A version using libguile-2.0.
;;;  
;;;  ;; NOTE: The definitions of c:gc-zalloc and c:gc-free assume Guile
;;;  ;; 2.x, where the GC routines are wrappers around Boehm GC, and so a
;;;  ;; bunch of parameters are for backwards compatibility and ignored.
;;;
;;;  (eval-when (compile load eval)
;;;
;;;    (define libguile
;;;      (dynamic-link (string-append "libguile-" (effective-version))))
;;;
;;;    (define this-program (dynamic-link))
;;;
;;;    (define scm-calloc
;;;      (pointer->procedure '*
;;;                          (dynamic-func "scm_calloc" libguile)
;;;                          `(,size_t)))
;;;
;;;    (define free
;;;      (pointer->procedure void
;;;                          (dynamic-func "free" this-program)
;;;                          '(*)))
;;;
;;;    (define scm-gc-malloc
;;;      (pointer->procedure '*
;;;                          (dynamic-func "scm_gc_calloc" libguile)
;;;                          `(,size_t *)))
;;;
;;;    (define scm-gc-free
;;;      (pointer->procedure void
;;;                          (dynamic-func "scm_gc_free" libguile)
;;;                          `(* ,size_t *)))
;;;
;;;    ) ;; end eval-when
;;;
;;;  (define (c:zalloc size)
;;;    (scm-calloc (max 1 size)))
;;;
;;;  (define (c:free p)
;;;    (free p))
;;;
;;;  ;; The ‘what’ parameter is not used with Boehm GC, but the Guile
;;;  ;; library routines still expect it.
;;;  (define what (string->pointer "c:gc-zalloc"))
;;;
;;;  (define (c:gc-zalloc size)
;;;    (scm-gc-malloc (max 1 size) what))
;;;
;;;  (define (c:gc-free p)
;;;    (scm-gc-free p 0 what))

  ) ;; end of library.
