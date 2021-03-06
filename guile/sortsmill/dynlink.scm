;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-

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

(library (sortsmill dynlink)

  (export sortsmill-dynlink-dll
          sortsmill-dynlink-pointer
          sortsmill-dynlink-func
          sortsmill-dynlink-declarations
          sortsmill-dynlink-load-extension)

  (import (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (define sortsmill-dynlink-dll
      (dynamic-link "libguile-sortsmill_symbols")))

  (eval-when (compile load eval)
    (dynamic-call
     (dynamic-func "init_guile_sortsmill_dynlink" sortsmill-dynlink-dll)
     sortsmill-dynlink-dll))

  (define (sortsmill-dynlink-pointer func-name . ignored)
    (dynamic-pointer func-name sortsmill-dynlink-dll))

  (define (sortsmill-dynlink-func func-name . ignored)
    (dynamic-func func-name sortsmill-dynlink-dll))

  (define (sortsmill-dynlink-declarations . ignored)
    *unspecified*)

  (define (sortsmill-dynlink-load-extension init-func-name . ignored)
    (dynamic-call
     (dynamic-func init-func-name sortsmill-dynlink-dll)
     sortsmill-dynlink-dll))

  ) ;; end of library.
