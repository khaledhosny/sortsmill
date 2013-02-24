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

(library (sortsmill strings grabbed-strings)
  
  (export pointer->grabbed-string
          pointer->grabbed-string-list)

  (import (sortsmill alloc)
          (rnrs)
          (except (guile) error)
          (system foreign)
          (only (srfi :26) cut))

  (define (pointer->grabbed-string p . length-and/or-encoding)
    "Grab a freeable C string, freeing the original storage."
    (let ([s (apply pointer->string p length-and/or-encoding)])
      (c:free p)
      s))

  (define (pointer->grabbed-string-list p . length-and/or-encoding)
    "Grab a freeable, NULL-terminated array of freeable C strings,
freeing the original storage, and returning the strings in a
list. (Glib users: this is equivalent to copying the strings and then
calling g_strfreev.)"
    (let ([strings (map (cut apply
                             pointer->grabbed-string <>
                             length-and/or-encoding)
                        (get-string-pointers p))])
      (c:free p)
      strings))

  (define (get-string-pointers p)
    (let* ([string-ptr (dereference-pointer p)])
      (if (null-pointer? string-ptr)
          '()
          (cons string-ptr
                (get-string-pointers
                 (make-pointer (+ (pointer-address p) (sizeof '*))))))))

  ;;-------------------------------------------------------------------------

  ) ;; end of library.
