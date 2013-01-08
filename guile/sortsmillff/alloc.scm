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

(define-module (sortsmillff alloc))

(export (rename (scm_calloc c:zalloc) ; Allocate and fill with zeroes.
                (free       c:free)
                (GC_malloc  c:gc-zalloc) ; Garbage-collected.
                (GC_free    c:gc-free)))

(import (rnrs base)
        (system foreign))

(define scm_calloc
  (pointer->procedure '*
                      (dynamic-func "scm_calloc" (dynamic-link))
                      (list size_t)))

(define free
  (pointer->procedure void
                      (dynamic-func "free" (dynamic-link))
                      (list '*)))

(define _GC_malloc_error
  (pointer->procedure void
                      (dynamic-func "scm_memory_error" (dynamic-link))
                      (list '*)))

(define _GC_malloc_error_msg
  (string->pointer "GC_malloc"))

(define (GC-malloc-error)
  (_GC_malloc_error _GC_malloc_error_msg))

(define _GC_malloc
  (pointer->procedure '*
                      (dynamic-func "GC_malloc" (dynamic-link))
                      (list size_t)))

(define (GC_malloc size)
  (let ((ptr (_GC_malloc size)))
    (when (null-pointer? ptr)
      (GC-malloc-error))
    ptr))

(define GC_free
  (pointer->procedure void
                      (dynamic-func "GC_free" (dynamic-link))
                      (list '*)))
