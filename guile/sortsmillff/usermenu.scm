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
   (sortsmillff fontforge-api)
   (sortsmillff gdraw-api)
   (system foreign)
   (srfi srfi-69)                       ; Portable hash tables.
   )

(export
   register-fontforge-menu-entry
   wrap-ff_menu_entry_action_t
   wrap-ff_menu_entry_enabled_t
   )

;;-------------------------------------------------------------------------

;; FIXME: Don’t explicitly go through a pointer so often, once support
;; for other means is available. The current way is verbose and
;; unreadable.

;;-------------------------------------------------------------------------
;;
;; Containers where the ‘action’ and ‘enabled’ functions are stored.

(define cv-menu-info (make-hash-table))
(define fv-menu-info (make-hash-table))

(define (menu-info-exists? menu-info mid)
   (hash-table-exists? menu-info mid))

;; Find a non-negative integer that is not yet used as a key in the
;; given menu-info table.
(define (menu-info-unused-key menu-info)
   (letrec ((find (lambda (i) (if (menu-info-exists? menu-info i)
                                  (find (1+ i))
                                  i))))
      (find 0)))

(define (menu-info-ref menu-info mid)
   (hash-table-ref menu-info mid))

(define (menu-info-set! menu-info mid new-entry)
   (hash-table-set! menu-info mid new-entry))

(define get-action-func
   (case-lambda
      ((menu-info-entry) (car menu-info-entry))
      ((menu-info mid) (car (menu-info-ref menu-info mid)))))

(define get-enabled-func
   (case-lambda
      ((menu-info-entry) (cadr menu-info-entry))
      ((menu-info mid) (cadr (menu-info-ref menu-info mid)))))

(define (set-menu-info-entry-defaults menu-info-entry)
   (list
      (if (car menu-info-entry) (car menu-info-entry)
          ;; Default action is ‘do nothing’.
          (lambda (view) *unspecified*))
      (if (cadr menu-info-entry) (cadr menu-info-entry)
          ;; Default is ‘always enabled’.
          (lambda (view) #t))))

;; Add functions to one of the ‘menu-info’ containers. Return the key
;; (‘mid’) of the new entry.
(define (menu-info-add! menu-info menu-info-entry)
   (let ((mid (menu-info-unused-key menu-info)))
      (menu-info-set! menu-info mid
         (set-menu-info-entry-defaults menu-info-entry))
      mid))

;;-------------------------------------------------------------------------

(define (GTextInfo-null? ti)
   (and (null-pointer? (GTextInfo:text-ref ti))
        (null-pointer? (GTextInfo:image-ref ti))
        (not (GTextInfo:line-ref ti))))

(define (GMenuItem2-null? mi)
   (GTextInfo-null? (pointer->GTextInfo (GMenuItem2:ti->pointer mi))))

;; Convert a menu’s or submenu’s C array to a more usable form (a
;; Scheme list).
(define (GMenuItem2-internal-array->list mi)
   (letrec
         ((collect
             (lambda (i prior)
                (let ((ith-element (pointer->GMenuItem2
                                      (GMenuItem2->pointer mi i))))
                   (if (GMenuItem2-null? ith-element)
                       (reverse prior)
                       (collect (1+ i) (cons ith-element prior)))))))
      (collect 0 '())))

;; Individually enable or disable the entries in a submenu.
(define (tools-list-check menu-item view menu-info)
   (let ((item (if (pointer? menu-item)
                   (pointer->GMenuItem2 menu-item)
                   menu-item)))
      (when (not (null-pointer? (GMenuItem2:sub-ref item)))
         (for-each
            (lambda (mi)
               (let ((mid (GMenuItem2:mid-ref mi)))
                  (when (menu-info-exists? menu-info mid)
                     (let ((enabled?
                              (menu-entry-error-handling
                                 (get-enabled-func menu-info mid)
                                 view)))
                        (GTextInfo:disabled-set!
                           (pointer->GTextInfo (GMenuItem2:ti->pointer mi))
                           (not enabled?))))))
            (GMenuItem2-internal-array->list (GMenuItem2:sub-dref item))))))

;; Invoke the action for a menu entry.
(define (do-action menu-item view menu-info)
   (let ((item (if (pointer? menu-item)
                   (pointer->GMenuItem2 menu-item)
                   menu-item)))
      (let ((mid (GMenuItem2:mid-ref item)))
         (when (menu-info-exists? menu-info mid)
            (menu-entry-error-handling
               (get-action-func menu-info mid) view)))))

;; An error-handling wrapper for the invocation of ‘(proc view)’.
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
      (wrapper-guard c-action)
      (wrapper-guard proc)
      (wrapper-guard wrapped-action)
      wrapped-action))

(define* (wrap-ff_menu_entry_enabled_t c-enabled
            #:optional (data %null-pointer))
   (let* ((proc (pointer->procedure (_Bool) c-enabled (list '* '*)))
          (wrapped-enabled (lambda (view) (not (zero? (proc (view->pointer view) data))))))
      (wrapper-guard c-enabled)
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
