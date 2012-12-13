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

(define-module (sortsmillff usermenu))

(use-modules
   (sortsmillff views)
   (sortsmillff notices))

(export register-fontforge-menu-entry)

(define (menu-entry-error-handling proc view)
   (let ((retval #f))
      (fontforge-call-with-error-handling
         "Guile exception"
         (lambda () (set! retval (proc view)))
         #:value-after-catch #f)
      retval))

(load-extension "libguile-sortsmillff_fontforgeexe"
   "init_guile_sortsmillff_usermenu")
  
(define* (register-fontforge-menu-entry
            #:key window menu-path action
            (enabled (const #t))
            (shortcut #f))
   (internal:register-fontforge-menu-entry window menu-path
      action enabled shortcut))
