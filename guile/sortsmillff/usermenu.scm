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

(define* (wrap-ff_menu_entry_action_t c-action
            #:optional (data %null-pointer))
   (let ((proc (pointer->procedure void c-action (list '* '*))))
      (lambda (view) (proc (view->pointer view) data))))

(define* (wrap-ff_menu_entry_enabled_t c-enabled
            #:optional (data %null-pointer))
   (let ((proc (pointer->procedure (_Bool) c-enabled (list '* '*))))
      (lambda (view) (not (zero? (proc (view->pointer view) data))))))

;;-------------------------------------------------------------------------
;;
;; Wrappers for registering Python callables.

(if-fontforge-has-python-api

   (export
      wrap-python-action
      )

   (define* (compile-python-expression pycode file-name
               #:key (catch-errors #t))
      (let* ((compiled-code
                (Py_CompileString
                   (string->pointer pycode)
                   (string->pointer file-name)
                   Py_eval_input)))             
         (if (and catch-errors (null-pointer? compiled-code))
             (if (not (null-pointer? (PyErr_Occurred)))
                 (begin
                    ;; FIXME: Better error handling.
                    (PyErr_Print)
                    (error "Python compilation failed"))
                 (py-autoref compiled-code))
             (py-autoref compiled-code))))

   (define* (eval-compiled-python-code compiled-code globals locals
               #:key (catch-errors #t))
      (let ((eval-result (PyEval_EvalCode compiled-code globals locals)))
         (if (and catch-errors (null-pointer? compiled-code))
             (if (not (null-pointer? (PyErr_Occurred)))
                 (begin
                    ;; FIXME: Better error handling.
                    (PyErr_Print)
                    (error "Python evaluation failed"))
                 (py-autoref eval-result))
             (py-autoref eval-result))))

   (define* (wrap-python-action-string pycode file-name globals locals
              #:key (catch-errors #t))
      (let* ((compiled-code (compile-python-expression pycode file-name
                               #:catch-errors catch-errors))
             (eval-result (eval-compiled-python-code compiled-code globals locals
                             #:catch-errors catch-errors)))
         
         (newline (current-error-port))
         (write eval-result (current-error-port))
         (write (pointer-address eval-result) (current-error-port))
         (newline (current-error-port))
         ))

   (define* (wrap-python-action pycode
               #:key
               (globals (py-autoincref (PyEval_GetBuiltins)))
               (locals %null-pointer)
               (file-name "usermenu.scm")
               (catch-errors #t))
      (cond ((string? pycode)
             (wrap-python-action-string pycode file-name globals locals
                #:catch-errors catch-errors))
            (else (scm-error 'wrong-type-arg "wrap-python-action"
                     "Expected a string, but got: ~S"
                     (list pycode) (list pycode)))))

   )

;;-------------------------------------------------------------------------
