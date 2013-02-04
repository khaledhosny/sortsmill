;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Barry Schwartz
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

(library (sortsmill i18n)

  (export _ C_
          dgettext
          dpgettext
          pkg-info:textdomain ;; Reëxported from (sortsmill pkg-info).
          )

  (import (sortsmill pkg-info)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (system foreign))

  (define _
    (lambda (msg) (dgettext pkg-info:textdomain msg)))

  (define C_
    (let ([separator (string #\eot)])
      (lambda (context msg)
        (dpgettext pkg-info:textdomain
                   (string-append context separator msg)
                   (+ 1 (string-length context))))))

  (define dgettext
    (let ([proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "g_dgettext" "#include <glib.h>")
                 `(* *))])
      (lambda (domain msgid)
        (let ([domain^ (if domain (string->pointer domain "UTF-8") %null-pointer)]
              [msgid^ (string->pointer msgid "UTF-8")])
          (pointer->string (proc domain^ msgid^))))))

  (define dpgettext
    (let ([proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "g_dpgettext" "#include <glib.h>")
                 `(* * ,size_t))])
      (lambda (domain msgctxtid msgidoffset)
        (let ([domain^ (if domain (string->pointer domain "UTF-8") %null-pointer)]
              [msgctxtid^ (string->pointer msgctxtid "UTF-8")])
          (pointer->string (proc domain^ msgctxtid^ msgidoffset))))))

  ) ;; end of library.
