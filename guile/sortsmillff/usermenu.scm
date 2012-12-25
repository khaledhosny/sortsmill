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
   (sortsmillff notices)
   (sortsmillff python)
   (system foreign)
   )

(export
   register-fontforge-menu-entry
   wrap-ff_menu_entry_action_t
   wrap-ff_menu_entry_enabled_t
   )

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

;;-------------------------------------------------------------------------
;;
;; Example use of wrappers to register C, Fortran (using BIND(C)), or
;; similar menu functions from Guile.
;;
;;   (let ((dll (dynamic-link "my_extensions"))
;;        ((my-data-ptr (bytevector->pointer my-data-bytevector))))
;;      (register-fontforge-menu-entry
;;            #:window 'glyph
;;            #:menu-path '("Tools" "My action")
;;            #:action (wrap-ff_menu_entry_action_t
;;                        (dynamic-func "my_action" dll) my-data-ptr)
;;            #:enabled (wrap-ff_menu_entry_enabled_t
;;                         (dynamic-func "my_enabled" dll) my-data-ptr)
;;            #:shortcut "My action|F10"))

(define wrapper-guard (make-guardian))

(define* (wrap-ff_menu_entry_action_t c-action
            #:optional (data %null-pointer))
   (let* ((proc (pointer->procedure void c-action (list '* '*)))
          (wrapped-action (lambda (view) (proc (view->pointer view) data))))
      (wrapper-guard proc)
      (wrapper-guard wrapped-action)
      wrapped-action))

(define* (wrap-ff_menu_entry_enabled_t c-enabled
            #:optional (data %null-pointer))
   (let* ((proc (pointer->procedure (_Bool) c-enabled (list '* '*)))
          (wrapped-enabled (lambda (view) (not (zero? (proc (view->pointer view) data))))))
      (wrapper-guard proc)
      (wrapper-guard wrapped-enabled)
      wrapped-enabled))

;;-------------------------------------------------------------------------
;;
;; Functions that are privately used in the Python API.

(define font_view_p__
   (lambda (obj) ((@ (sortsmillff views) font-view?) obj)))

(define glyph_view_p__
   (lambda (obj) ((@ (sortsmillff views) glyph-view?) obj)))

(define font_view_to_pointer__
   (lambda (obj) ((@ (sortsmillff views) font-view->pointer) obj)))

(define glyph_view_to_pointer__
   (lambda (obj) ((@ (sortsmillff views) glyph-view->pointer) obj)))

(define closure_maker__
   (lambda (cython-func py-func)
      (lambda (view) (cython-func view py-func))))

;;-------------------------------------------------------------------------
