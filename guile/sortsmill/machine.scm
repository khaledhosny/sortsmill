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

(library (sortsmill machine)

  (export bytevector-address-native-set!
          bytevector-pointer-native-set!
          bytevector-address-native-ref
          bytevector-pointer-native-ref
          set-pointer!
          get-pointer

          SCM-ref
          SCM-set!
          bytevector-SCM-ref
          bytevector-SCM-set!
          )

  (import (rnrs)
          (except (guile) error)
          (ice-9 match)
          (system foreign))

  (define-syntax bytevector-address-native-set!
    (lambda (x)
      (syntax-case x ()
        ((_ bv index value)
         (match (sizeof '*)
           (4 #'(bytevector-u32-native-set! bv index value))
           (8 #'(bytevector-u64-native-set! bv index value))
           (else (error
                  'bytevector-address-native-set!
                  ;; Do not bother putting this in translation files.
                  "we do not know how to handle addresses of this size"
                  (sizeof '*))
                 #'#f))))))

  (define (bytevector-pointer-native-set! bv index p)
    (bytevector-address-native-set! bv index (pointer-address p)))

  (define-syntax bytevector-address-native-ref
    (lambda (x)
      (syntax-case x ()
        ((_ bv index)
         (match (sizeof '*)
           (4 #'(bytevector-u32-native-ref bv index))
           (8 #'(bytevector-u64-native-ref bv index))
           (else (error
                  'bytevector-address-native-ref
                  ;; Do not bother putting this in translation files.
                  "we do not know how to handle addresses of this size"
                  (sizeof '*))
                 #'#f))))))

  (define (bytevector-pointer-native-ref bv index)
    (make-pointer (bytevector-address-native-ref bv index)))

  (define (set-pointer! bv p)
    (bytevector-pointer-native-set! bv 0 p))

  (define (get-pointer bv)
    (bytevector-pointer-native-ref bv 0))

  ) ;; end of library.
