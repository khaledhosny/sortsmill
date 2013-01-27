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

(define-module (sortsmill alloc))

(export c:zalloc ;; Allocate and fill with zeroes.
        c:free
        c:gc-zalloc ;; Garbage-collected.
        c:gc-free)

(import (rnrs base)
        (system foreign))

(define c:zalloc
  (pointer->procedure '*
                      (dynamic-func "scm_calloc" (dynamic-link))
                      (list size_t)))

(define c:free
  (pointer->procedure void
                      (dynamic-func "free" (dynamic-link))
                      (list '*)))

(define GC_malloc_error_
  (pointer->procedure void
                      (dynamic-func "scm_memory_error" (dynamic-link))
                      (list '*)))

(define GC_malloc_error_msg_
  (string->pointer "GC_malloc"))

(define (GC-malloc-error)
  (GC_malloc_error_ GC_malloc_error_msg_))

(define GC_malloc
  (pointer->procedure '*
                      (dynamic-func "GC_malloc" (dynamic-link))
                      (list size_t)))

(define (c:gc-zalloc size)
  (let ((ptr (GC_malloc size)))
    (when (null-pointer? ptr)
      (GC-malloc-error))
    ptr))

(define c:gc-free
  (pointer->procedure void
                      (dynamic-func "GC_free" (dynamic-link))
                      (list '*)))
