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

(library (sortsmill argv)

  (export string-list->argv-and-argc
          argv/argc->string-list)

  (import (sortsmill machine)
          (sortsmill kwargs)
          (rnrs)
          (only (srfi :1) iota)
          (only (srfi :26) cut)
          (system foreign))

  ;; The argv is returned first because it is the more likely value to
  ;; be needed.
  (define/kwargs (string-list->argv-and-argc args [encoding #f])
    (if encoding (string? encoding) #t)
    (let* ([n (length args)]
           [pointer-size (sizeof '*)]
           [num-bytes (* pointer-size (+ 1 n))]
           [argv (make-bytevector num-bytes 0)]
           [s->p (if encoding
                     (cut string->pointer <> encoding)
                     string->pointer)])
      (for-each
       (lambda (i s)
         (bytevector-pointer-native-set! argv (* i pointer-size) (s->p s)))
       (iota n) args)
      (values argv n)))

  (define/kwargs (argv/argc->string-list argv
                                         [argc (greatest-fixnum)]
                                         [encoding #f])
    (if encoding (string? encoding) #t)
    (assert (pointer? argv))
    (assert (fixnum? argc))
    (let ([p->s (if encoding
                    (cut pointer->string <> encoding)
                    pointer->string)])
      (map p->s (get-n-or-fewer-string-pointers argv argc))))

  (define (get-n-or-fewer-string-pointers p n)
    (let ([string-ptr (dereference-pointer p)])
      (if (= n 0)
          '()
          (if (null-pointer? string-ptr)
              '()
              (cons string-ptr
                    (get-n-or-fewer-string-pointers
                     (make-pointer (+ (pointer-address p) (sizeof '*)))
                     (- n 1)))))))

  ) ;; end of library.
