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
   (ice-9 match)
   (srfi srfi-26)                       ; ‘cut’ and ‘cute’.
   )

(export
   log-fontforge-warning
   post-fontforge-notice
   post-fontforge-error
   fontforge-catch
   *fontforge-pre-unwind-stack*
   *fontforge-pre-unwind-handler*
   *fontforge-throw-handler*
   )

(load-extension "libguile-sortsmillff_fontforge"
   "init_guile_sortsmillff_notices")

(define (fontforge-catch window-title key thunk)
   (let ((*fontforge-pre-unwind-stack* #f))
      (let ((*fontforge-pre-unwind-handler*
              (lambda (key . args)
                 (set! *fontforge-pre-unwind-stack* (make-stack #t))))

            (*fontforge-throw-handler*
               (lambda (key . args)

                  (when *fontforge-pre-unwind-stack*
                     (log-fontforge-warning
                        (with-output-to-string
                           (lambda ()
                              (display-backtrace *fontforge-pre-unwind-stack*
                                 (current-output-port)))))
                     (set! *fontforge-pre-unwind-stack* #f))

                  (post-fontforge-error window-title
                     (let ((in-proc (lambda (proc)
                                       (if proc
                                           (simple-format #f "In procedure ~A : " proc)
                                           ""))))
                        (match args
                           ((proc (? string? str) #f data)
                            (simple-format #f "~A~A" (in-proc proc) str))
                           ((proc (? string? fmt) (? list? args) data)
                            (simple-format #f "~A~A" (in-proc proc)
                               (apply (cute simple-format #f fmt <...>) args)))
                           (_ (simple-format #f "Uncaught throw: '~A . ~A" key args))))))))

         (catch key
            thunk
            *fontforge-throw-handler*
            *fontforge-pre-unwind-handler*))))
