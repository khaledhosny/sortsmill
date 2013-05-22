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

(library (sortsmill containers rnrs-hashtable)

  ;; Extensions to R⁶RS hashtables.

  (export alist->eq-hashtable
          alist->eqv-hashtable
          alist->hashtable
          hashtable->alist)

  (import (rnrs)
          (only (guile) 1-)
          (only (srfi :1) unfold))

  (define (hashtable-set-and-return! ht k.v)
    (hashtable-set! ht (car k.v) (cdr k.v))
    ht)

  (define (alist->eq-hashtable alist)
    (let ([ht (make-eq-hashtable)])
      (fold-left hashtable-set-and-return! ht alist)))

  (define (alist->eqv-hashtable alist)
    (let ([ht (make-eqv-hashtable)])
      (fold-left hashtable-set-and-return! ht alist)))

  (define (alist->hashtable hash-function equiv alist)
    (let ([ht (make-hashtable hash-function equiv)])
      (fold-left hashtable-set-and-return! ht alist)))

  (define (hashtable->alist ht)
    (let-values ([(keys values) (hashtable-entries ht)])
      (unfold negative?
              (lambda (i) (cons (vector-ref keys i)
                                (vector-ref values i)))
              1- (1- (vector-length keys)))))

  ) ;; end of library.
