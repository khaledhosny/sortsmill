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

(library (sortsmill notices)

  (export log-fontforge-warning
          post-fontforge-notice
          post-fontforge-error
          fontforge-call-with-error-handling)

  (import (rnrs)
          (system repl error-handling)
          (only (guile)
                *unspecified* define* load-extension eval-when
                format getenv)
          (only (ice-9 match) match))

  (eval-when (compile load eval)
    (load-extension "libsortsmill_fontforge"
                    "init_guile_sortsmill_notices"))

  (define* (fontforge-call-with-error-handling
            window-title thunk #:key (value-after-catch *unspecified*))
    (let ([retval value-after-catch]
          [*on-error* (match (getenv "FONTFORGE_GUILE_ON_ERROR")
                        (#f 'pass)
                        ("pass" 'pass)
                        ("debug" 'debug)
                        (_ 'pass))])
      (call-with-error-handling thunk
                                #:on-error *on-error*
                                #:post-error (post-error-handler window-title))
      retval))

  (define (post-error-handler window-title)
    (lambda (key . args)
      (post-fontforge-error
       window-title
       (let ([in-proc (lambda (proc)
                        (if proc (format #f "In procedure ~a : " proc) ""))])
         (match args
          ;;;;
          ;;;; FIXME FIXME FIXME: Add a case for R⁶RS exceptions. Or use Guile internals more.
          ;;;;

           ;; A conventional Guile-style error exception
           ;; without format arguments.
           [[(? (lambda (p) (or (eq? p #f) (string? p))) proc)
             (? string? str)
             #f
             (? (lambda (d) (or (eq? d #f) (list? d))) data)]
            (format #f "~a~a" (in-proc proc) str)]

           ;; A conventional Guile-style error exception
           ;; with format arguments.
           [[(? (lambda (p) (or (eq? p #f) (string? p))) proc)
             (? string? fmt)
             (? list? args)
             (? (lambda (d) (or (eq? d #f) (list? d))) data)]
            (format #f "~a~a" (in-proc proc) (apply format #f fmt args))]

           ;; Anything else.
           [_ (format
               #f "Guile exception thrown to key '~a with arguments ~a"
               key args)] )))))

  ) ;; end of library.
