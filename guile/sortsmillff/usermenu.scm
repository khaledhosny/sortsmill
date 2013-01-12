;; -*- mode: scheme; coding: utf-8 -*-

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

(define-module (sortsmillff usermenu))

(export register-fontforge-menu-entry
        wrap-ff_menu_entry_action_t
        wrap-ff_menu_entry_enabled_t

        ;; FIXME: Decide what really to export.
        color-default
        glyph-view-user-menu
        font-view-user-menu
        add-menu-entry
        fill-menu-item-for-action
        fill-menu-item
        menu-entry-list->menu-items
        )

(import (sortsmillff views)
        (sortsmillff notices)
        (sortsmillff fontforge-api)
        (sortsmillff gdraw-api)
        (sortsmillff machine)
        (rnrs)
        (srfi :1)
        (only (srfi :27) random-integer)
        (ice-9 match)
        (system foreign)
        )

;;-------------------------------------------------------------------------
;;
;; A container where the ‘action’ and ‘enabled’ functions are stored.

(define menu-info (make-eq-hashtable))

(define (menu-info-contains? mid)
  (hashtable-contains? menu-info mid))

;; Find a non-negative integer that is not yet used as a key in the
;; given menu-info table.
(define (menu-info-unused-key)
  (let ((candidate (random-integer 2000000000)))
    (if (menu-info-contains? candidate)
        (menu-info-unused-key)
        candidate)))

(define (menu-info-ref mid)
  (hashtable-ref menu-info mid #f))

(define (menu-info-set! mid new-entry)
  (hashtable-set! menu-info mid new-entry))

(define (get-action-func entry)
  (if (integer? entry)
      (get-action-func (menu-info-ref entry))
      (car entry)))

(define (get-enabled-func entry)
  (if (integer? entry)
      (get-enabled-func (menu-info-ref entry))
      (cadr entry)))

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
(define (menu-info-add! menu-info-entry)
  (let ((mid (menu-info-unused-key)))
    (menu-info-set! mid (set-menu-info-entry-defaults menu-info-entry))
    mid))

;;-------------------------------------------------------------------------

(define (GTextInfo-null? ti)
  (and (null-pointer? (GTextInfo:text-ref ti))
       (null-pointer? (GTextInfo:image-ref ti))
       (not (GTextInfo:line-ref ti))))

(define (GMenuItem-null? mi)
  (GTextInfo-null? (GMenuItem:ti-ref mi)))

;; Convert a menu’s or submenu’s C array to a more usable form (a
;; Scheme list).
(define (GMenuItem-internal-array->list mi)
  (letrec
      ;; Stack depth surely is not a problem here, but I felt like
      ;; writing a tail-recursive function, anyway. :)
      ((cons-the-list-tail-recursively
        (lambda (i prior)
          (let ((ith-element (GMenuItem-ref mi i)))
            (if (GMenuItem-null? ith-element)
                (reverse prior)
                (cons-the-list-tail-recursively
                 (+ i 1) (cons ith-element prior)))))))
    (cons-the-list-tail-recursively 0 '())))

(define (get-GMenuItem-subentries menu-item)
  (let ((sub (GMenuItem:sub-ref menu-item)))
    (if (null-pointer? sub)
        '()                             ; No entries.
        (GMenuItem-internal-array->list
         (pointer->GMenuItem sub)))))

;; Individually enable or disable the entries in a submenu.
(define (tools-list-check menu-item view)
  ;; For menu-item, accept either a GMenuItem or a pointer to one.
  (if (pointer? menu-item)
      (tools-list-check (pointer->GMenuItem menu-item) view)
      (for-each
       (lambda (mi)
         (let ((mid (GMenuItem:mid-ref mi)))
           (when (menu-info-contains? mid)
             (let ((enabled?
                    (menu-entry-error-handling
                     (get-enabled-func mid) view)))
               (GTextInfo:disabled-set!
                (GMenuItem:ti-ref mi) (not enabled?))))))
       (get-GMenuItem-subentries menu-item))))

;; Invoke the action for a menu entry.
(define (do-action menu-item view)
  ;; For menu-item, accept either a GMenuItem or a pointer to one.
  (if (pointer? menu-item)
      (do-action (pointer->GMenuItem menu-item) view)
      (let ((mid (GMenuItem:mid-ref menu-item)))
        (when (menu-info-contains? mid)
          (menu-entry-error-handling
           (get-action-func mid) view)))))

;; An error-handling wrapper for the invocation of ‘(proc view)’.
(define (menu-entry-error-handling proc view)
  (let ((retval #f))
    (fontforge-call-with-error-handling
     "Guile exception"
     (lambda () (set! retval (proc view)))
     #:value-after-catch #f)
    retval))

;;-------------------------------------------------------------------------

(define font-window-flag 1)
(define glyph-window-flag 2)
(define metrics-window-flag 4)     ; Reserved for possible future use.

(define (window-name->flag window-name)
  (cond
   ((string-ci=? window-name "glyph") glyph-window-flag)
   ((string-ci=? window-name "char") glyph-window-flag)
   ((string-ci=? window-name "font") font-window-flag)
   (else (error 'window-name->flag
                "expected \"glyph\" or \"font\" for window name"
                 window-name))))

(define GDrawGetUserData
  (pointer->procedure
   '* (dynamic-func "GDrawGetUserData" (dynamic-link)) '(*)))

(define (any-view-moveto pointer->view window-ptr menu-item-ptr event)
  (tools-list-check menu-item-ptr
                    (pointer->view (GDrawGetUserData window-ptr))))

(define (any-view-invoke pointer->view window-ptr menu-item-ptr event)
  (do-action menu-item-ptr
             (pointer->view (GDrawGetUserData window-ptr))))  

(define (glyph-view-moveto window-ptr menu-item-ptr event)
  (any-view-moveto pointer->glyph-view
                   window-ptr menu-item-ptr event))

(define (font-view-moveto window-ptr menu-item-ptr event)
  (any-view-moveto pointer->font-view
                   window-ptr menu-item-ptr event))

(define (glyph-view-invoke window-ptr menu-item-ptr event)
  (any-view-invoke pointer->glyph-view
                   window-ptr menu-item-ptr event))

(define (font-view-invoke window-ptr menu-item-ptr event)
  (any-view-invoke pointer->font-view
                   window-ptr menu-item-ptr event))

(define (procedure->menu-func-pointer proc)
  (procedure->pointer void proc '(* * *)))

(define glyph-view-moveto-ptr
  (procedure->menu-func-pointer glyph-view-moveto))

(define font-view-moveto-ptr
  (procedure->menu-func-pointer font-view-moveto))

(define glyph-view-invoke-ptr
  (procedure->menu-func-pointer glyph-view-invoke))

(define font-view-invoke-ptr
  (procedure->menu-func-pointer font-view-invoke))

(define (menu-proc-ptr caller glyph-proc-ptr font-proc-ptr flag)
  (cond
   ((= flag glyph-window-flag) glyph-proc-ptr)
   ((= flag font-window-flag) font-proc-ptr)
   (else (assertion-violation
          caller
          (simple-format #f "expected ~d or ~d"
                         font-window-flag glyph-window-flag)
          flag))))
  
(define (moveto-proc-ptr flag)
  (menu-proc-ptr
   'moveto-proc-ptr glyph-view-moveto-ptr font-view-moveto-ptr flag))

(define (invoke-proc-ptr flag)
  (menu-proc-ptr
   'invoke-proc-ptr glyph-view-invoke-ptr font-view-invoke-ptr flag))

;;-------------------------------------------------------------------------

(define color-default #xfffffffe)

(define glyph-view-menu-internal
  (pointer->bytevector
   (dynamic-pointer "cv_menu" (dynamic-link)) (sizeof '*)))

(define font-view-menu-internal
  (pointer->bytevector
   (dynamic-pointer "fv_menu" (dynamic-link)) (sizeof '*)))

;;
;; FIXME: More thorough and better modularized error checking.
;;
(define (fill-menu-item! window-name menu-item menu-entry)
  (set-menu-item-defaults! menu-item)
  (let ((window-flag (window-name->flag window-name))
        (action (assq 'action menu-entry))
        (enabled (assq 'enabled menu-entry)))
    (when (and enabled (not action))
      (error 'fill-menu-item!
             "a menu entry has an 'enabled field but no 'action field"
             menu-entry))
    (if action
        (begin
          (when (or (assq 'invoke menu-entry) (assq 'moveto menu-entry))
            (error 'fill-menu-item!
                   "a menu entry has both an 'action field and an 'invoke or 'moveto field"
                   menu-entry))
          (let ((integer-key (menu-info-add!
                              (list (cdr action)
                                    (if enabled (cdr enabled) #f)))))
            
          (for-each
           (match-lambda ((key . value) (set-menu-item-value! key value)))
           `((integer-key ,integer-key)
             (moveto ,(moveto-proc-ptr window-flag))
             (invoke ,(invoke-proc-ptr window-flag))
             ,@menu-entry))))
        (for-each
         (match-lambda ((key . value) (set-menu-item-value! key value)))
         menu-entry))))

(define (set-menu-item-defaults! menu-item)
  (bytevector-fill! menu-item 0)
  (let ((ti (GMenuItem:ti-ref menu-item)))
    (GTextInfo:fg-set! ti color-default)
    (GTextInfo:bg-set! ti color-default)
    (GTextInfo:text-is-1byte-set! ti #t)
    (GTextInfo:text-has-mnemonic-set! ti #t)
    (GTextInfo:image-precedes-set! ti #t)))

(define (set-menu-item-value! key value)
  (let ((mi menu-item)
        (ti (GMenuItem:ti-ref menu-item)))
    (match key
      ('text (GTextInfo:text-set! ti (string->pointer value "UTF-8")))
      ('image (GTextInfo:image-set! ti (string->pointer value)))
      ('foreground-color (GTextInfo:fg-set! ti value))
      ('background-color (GTextInfo:bg-set! ti value))
      ('disabled (GTextInfo:disabled-set! ti value))
      ('image-precedes-text (GTextInfo:image-precedes-set! ti value))
      ('checkable (GTextInfo:checkable-set! ti value))
      ('checked (GTextInfo:checked-set! ti value))
      ('is-line (GTextInfo:line-set! ti value))
      ('shortcut (GMenuItem:shortcut-set! mi (string->pointer value "UTF-8")))
      ('integer-key (GMenuItem:mid-set! mi value))
      ('moveto (GMenuItem:moveto-set! mi (force-to-menu-func-pointer value)))
      ('invoke (GMenuItem:invoke-set! mi (force-to-menu-func-pointer value)))
      ;;;
      ;;; FIXME: Submenu support.
      ;;;
      )))

(define (force-to-menu-func-pointer proc-or-pointer)
  (if (pointer? proc-or-pointer)
      proc-or-pointer
      (procedure->menu-func-pointer proc-or-pointer)))

(define (menu-entry-list->menu-items window-name menu-entry-list)
  (let* ((menu-size (length menu-entry-list))
         (mi-array (gc-malloc-GMenuItem (+ menu-size 1))))
    (for-each
     (match-lambda ((i menu-entry)
                    (fill-menu-item! window-name
                                     (GMenuItem-ref mi-array i)
                                     menu-entry)))
     (zip (iota menu-size) menu-entry-list))
    mi-array))

;;-------------------------------------------------------------------------
;;;;;
;;;;; FIXME: Get rid of these.
;;;;;

;;;
;;; FIXME: Get rid of this.
;;;
(load-extension "libguile-sortsmillff_fontforgeexe"
                "init_guile_sortsmillff_usermenu")

;;;
;;; FIXME: Get rid of this.
;;;
(define* (register-fontforge-menu-entry #:key window menu-path action
                                        (enabled (lambda (view) #t))
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

(define wrap-ff_menu_entry_action_t
  (case-lambda
    ((c-action) (wrap-ff_menu_entry_action_t c-action %null-pointer))
    ((c-action data)
     (let* ((proc (pointer->procedure void c-action (list '* '*)))
            (wrapped-action (lambda (view) (proc (view->pointer view) data))))
       wrapped-action))))

(define wrap-ff_menu_entry_enabled_t
  (case-lambda
    ((c-enabled) (wrap-ff_menu_entry_enabled_t c-enabled %null-pointer))
    ((c-enabled data)
     (let* ((proc (pointer->procedure _Bool c-enabled (list '* '*)))
            (wrapped-enabled (lambda (view) (not (zero? (proc (view->pointer view) data))))))
       wrapped-enabled))))

;;-------------------------------------------------------------------------
;;
;; Functions that are privately used in the Python API.

(define font_view_p__
  (lambda (obj) (font-view? obj)))

(define glyph_view_p__
  (lambda (obj) (glyph-view? obj)))

(define font_view_to_pointer__
  (lambda (obj) (font-view->pointer obj)))

(define glyph_view_to_pointer__
  (lambda (obj) (glyph-view->pointer obj)))

(define closure_maker__
  (lambda (cython-func py-func)
    (lambda (view) (cython-func view py-func))))

;;-------------------------------------------------------------------------
