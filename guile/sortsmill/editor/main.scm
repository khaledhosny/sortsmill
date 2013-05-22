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

(library (sortsmill editor main)

  (export editor-main)

  (import (sortsmill dynlink)
          (sortsmill argv)
          (rnrs)
          (except (guile) error)
          (system foreign))

  (define c-main
    [pointer->procedure
     int
     (sortsmill-dynlink-func "fontforge_main"
                             "#include <sortsmill/fontforge_main.h>")
     `(,int *)] )

  (define (editor-main args)
    (let-values ([(argv argc) (string-list->argv-and-argc args)])
      (c-main argc (bytevector->pointer argv))))

  ) ;; end of library.
