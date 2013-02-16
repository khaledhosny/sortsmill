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

(library (sortsmill editor finalization)

  (export register-finalizer
          run-and-clear-all-finalizers
          find-finalizer)

  (import (rnrs)
          (only (srfi :18) make-mutex mutex-lock! mutex-unlock!))

  (define finalization-registry '())

  (define finalization-mutex (make-mutex "editor finalization"))

  (define (register-finalizer key finalizer)
    (mutex-lock! finalization-mutex)
    (set! finalization-registry (cons (cons key finalizer)
                                      finalization-registry))
    (mutex-unlock! finalization-mutex))

  (define (find-finalizer key)
    (assoc key finalization-registry))

  (define (run-and-clear-finalizer)
    ;; Currently only the head finalizer can be run, though that may
    ;; change. If key-based finalization is implemented, then we may
    ;; wish to export the function that does it.
    (assert (not (null? finalization-registry)))
    ((cdar finalization-registry))
    (set! finalization-registry (cdr finalization-registry)))

  (define (run-and-clear-all-finalizers)
    (unless (null? finalization-registry)
      (run-and-clear-finalizer)
      (run-and-clear-all-finalizers)))

  ) ;; end of library.
