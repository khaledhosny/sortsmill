;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-

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

;;;
;;; Some of the support for FontForge’s legacy Python interface.
;;;

(library (sortsmill ffcompat)

  (export no_windowing_ui-ref
          no_windowing_ui-set!
          running_script-ref
          running_script-set!)

  (import (sortsmill machine)
          (sortsmill dynlink)
          (system foreign)
          (rnrs)
          (except (guile) error))

  (define no_windowing_ui-ref
    (let ([proc (pointer->procedure
                 _Bool
                 (sortsmill-dynlink-func "get_no_windowing_ui"
                                         "#include <fontforge.h>")
                 '())])
      (lambda () (not (fxzero? (proc))))))

  (define no_windowing_ui-set!
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "set_no_windowing_ui"
                                         "#include <fontforge.h>")
                 `(,_Bool))])
      (lambda (v) (proc (if v 1 0)))))

  (define running_script-ref
    (let ([proc (pointer->procedure
                 _Bool
                 (sortsmill-dynlink-func "get_running_script"
                                         "#include <fontforge.h>")
                 '())])
      (lambda () (not (fxzero? (proc))))))

  (define running_script-set!
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "set_running_script"
                                         "#include <fontforge.h>")
                 `(,_Bool))])
      (lambda (v) (proc (if v 1 0)))))

  ) ;; end of library.
