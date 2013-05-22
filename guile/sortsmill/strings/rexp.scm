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

(library (sortsmill strings rexp)

  (export rexp? pointer->rexp rexp->pointer
          rexp-match? pointer->rexp-match rexp-match->pointer

          rexp:compile rexp:compile-study rexp:compile-jit

          ;; The ‘compile-once’ Scheme procedures currently compile
          ;; once per thread, rather than once for all threads, and
          ;; are not based on the C versions. Either of these facts
          ;; may change in the future, however.
          rexp:compile-once rexp:compile-once-study rexp:compile-once-jit

          rexp:match rexp:search
          rexp:number-of-subexpressions
          rexp:interval rexp:substring)

  (import (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (system foreign)
          (ice-9 format))

  (define-wrapped-pointer-type rexp
    rexp? pointer->rexp rexp->pointer
    [lambda (obj port)
      (format port "#<rexp 0x~x>"
              (pointer-address (rexp->pointer obj)))] )

  (define-wrapped-pointer-type rexp-match
    rexp-match? pointer->rexp-match rexp-match->pointer
    [lambda (obj port)
      (format port "#<rexp-match 0x~x>"
              (pointer-address (rexp-match->pointer obj)))] )

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_sortsmill_guile_strings_rexp"))

  ) ;; end of library.
