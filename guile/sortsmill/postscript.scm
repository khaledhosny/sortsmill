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

(library (sortsmill postscript)

  (export
   ;; (scm->postscript various-types) → string
   scm->postscript
   )

  (import (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 format))

  (define (scm->postscript value)
    (cond
     [(eq? value #f) "false"]
     [(eq? value #t) "true"]
     [(string? value) value]
     [(integer? value) (format #f "~d" value)]
     [(real? value) (format #f "~f" value)]
     [(list? value)
      (let ([strings (map scm->postscript value)])
        (string-append "[" (string-join strings " ") "]"))]
     [else (assertion-violation 'scm->postscript
                                (_ "unexpected argument value")
                                value)] ))

  ) ;; end of library.
