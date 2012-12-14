;; -*- mode: bee; coding: utf-8 -*-

;; Copyright (C) 2012 Barry Schwartz
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

(define-module (sortsmillff notices))

(use-modules
   (system repl error-handling)
   (ice-9 match)
   (srfi srfi-26)                       ; ‘cut’ and ‘cute’.
   )

(export
   log-fontforge-warning
   post-fontforge-notice
   post-fontforge-error
   fontforge-call-with-error-handling
   )

(load-extension "libguile-sortsmillff_fontforge"
   "init_guile_sortsmillff_notices")

(define* (fontforge-call-with-error-handling window-title thunk
            #:key (value-after-catch *unspecified*))
   (let ((retval value-after-catch)
         (*on-error* (match (getenv "FONTFORGE_GUILE_ON_ERROR")
                        (#f 'pass)
                        ("pass" 'pass)
                        ("debug" 'debug)
                        (_ 'pass))))

      (call-with-error-handling thunk

         #:on-error *on-error*

         #:post-error
         (lambda (key . args)
            (post-fontforge-error window-title
               (let ((in-proc (lambda (proc)
                                 (if proc
                                     (simple-format #f "In procedure ~A : " proc)
                                     ""))))
                  (match args
                     ;; A conventional Guile-style error exception
                     ;; without format arguments.
                     (((? (lambda (p) (or (eq? p #f) (string? p))) proc)
                       (? string? str)
                       #f
                       (? (lambda (d) (or (eq? d #f) (list? d))) data))
                      (simple-format #f "~A~A" (in-proc proc) str))

                     ;; A conventional Guile-style error exception
                     ;; with format arguments.
                     (((? (lambda (p) (or (eq? p #f) (string? p))) proc)
                       (? string? fmt)
                       (? list? args)
                       (? (lambda (d) (or (eq? d #f) (list? d))) data))
                      (simple-format #f "~A~A" (in-proc proc)
                         (apply (cute simple-format #f fmt <...>) args)))

                     ;; Anything else.
                     (_ (simple-format #f
                           "Guile exception thrown to key '~A with arguments ~A"
                           key args)))))))
      retval))
