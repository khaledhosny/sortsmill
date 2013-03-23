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

  (import (rnrs)
          (except (guile) error)
          (system foreign))

  (define what (string->pointer "sortsmill alloc"))

  (define (alloc-dynlink-func func-name)
    "Dynamic link to symbols that hopefully are nearly, almost
positively, entirely, with luck guaranteed to be available."
    (dynamic-pointer func-name (dynamic-link)))

  (define c:zalloc
    (pointer->procedure '* (alloc-dynlink-func "scm_calloc") `(,size_t)))

  (define c:free
    (pointer->procedure void (alloc-dynlink-func "free") '(*)))

  (define c:gc-zalloc
    (let ([proc
           (pointer->procedure
            '* (alloc-dynlink-func "scm_gc_calloc") `(,size_t *))])
      (lambda (size)
        (proc size what))))

  (define c:gc-free
    (let ([proc
           (pointer->procedure
            void (alloc-dynlink-func "scm_gc_free") `(* ,size_t *))]
          [size-parameter-that-thankfully-is-ignored-in-Guile-2.x 0])
      (lambda (p)
        (proc p size-parameter-that-thankfully-is-ignored-in-Guile-2.x what))))

  ) ;; end of library.
