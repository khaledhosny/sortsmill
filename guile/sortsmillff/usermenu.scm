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
        )

(import (rnrs)
        (sortsmillff views)
        (sortsmillff notices)
        (sortsmillff fontforge-api)
        (sortsmillff gdraw-api)
        (sortsmillff machine)
        (system foreign)
        (only (srfi :1) span)
        (only (srfi :27) random-integer)
        (ice-9 match)
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
;;;;;
;;;;; Higher-level menu handling.
;;;;;
;;;;; UNDER DEVELOPMENT.
;;;;;

(define color-default #xfffffffe)

;; A menu entry (including a menu itself) is a pair. The car is a
;; header specifying the general character of the entry; for example,
;; if it is a submenu, and what is its name. The cdr is an associative
;; list of attributes.

(define glyph-view-user-menu '((menu "Tools") . ()))
(define font-view-user-menu '((menu "Tools") . ()))

(define (choose-by-window-name window-name glyph-thing font-thing)
  (cond
   ((string-ci=? window-name "glyph") glyph-thing)
   ((string-ci=? window-name "char") glyph-thing)
   ((string-ci=? window-name "font") font-thing)
   (else (error (string-append
                 "expected \"glyph\" or \"font\" for window name, but got"
                 window-name)))))

(define (choose-user-menu window-name)
  (choose-by-window-name
   window-name glyph-view-user-menu font-view-user-menu))

(define (menu-entry-head? head)
  (match head
         (((? symbol? kind) (? string? name) . rest) #t)
         (else #f)))

(define (menu-entry? entry)
  (if (pair? entry)
      (and (menu-entry-head? (car entry))
           (list? (cdr entry)))
      #f))

(define (menu-entry-head=? head1 head2)
  (assert (menu-entry-head? head1))
  (assert (menu-entry-head? head2))
  (and (eq? (car head1) (car head2))
       (string=? (cadr head1) (cadr head2))))

(define (menu-entry-head-is-kind? kind head)
  (assert (symbol? kind))
  (assert (menu-entry-head? head))
  (eq? (car head) kind))

(define (menu-entry-is-kind? kind entry)
  (menu-entry-head-is-kind? kind (car entry)))

(define (add-menu-entry menu menu-path new-entry)
  (assert (menu-entry-is-kind? 'menu menu))
  (assert (list? menu-path))
  (assert (for-all string? menu-path))
  (assert (menu-entry? new-entry))
  (match menu-path
         (() (cons (car menu)
                   (append (cdr menu) (list new-entry))))
         ((submenu-name . more-menu-path)
          (let*-values
              (((before remaining)
                (span (lambda (entry) (menu-entry-head=?
                                       (list 'menu submenu-name)
                                       (car entry)))
                      (cdr menu)))

               ((submenu after)
                (if (null? remaining)
                    (values (cons (list 'menu submenu-name) '())
                            '())
                    (if (menu-entry-is-kind? 'menu (car remaining))
                        (values (car remaining) ; Existing submenu.
                                (cdr remaining))
                        (values (cons (list 'menu submenu-name) '())
                                '())
                        ))))
            (append (list (car menu))
                    before
                    (list (add-menu-entry submenu more-menu-path
                                          new-entry))
                    after)))))

(define cv_tools_list_check
  (pointer->procedure
   void (dynamic-func "cv_tools_list_check" (dynamic-link)) '(* * *)))

(define fv_tools_list_check
  (pointer->procedure
   void (dynamic-func "fv_tools_list_check" (dynamic-link)) '(* * *)))

(define cv_do_action
  (pointer->procedure
   void (dynamic-func "cv_do_action" (dynamic-link)) '(* * *)))

(define fv_do_action
  (pointer->procedure
   void (dynamic-func "fv_do_action" (dynamic-link)) '(* * *)))

(define (choose-moveto window-name)
  (choose-by-window-name
   window-name cv_tools_list_check fv_tools_list_check))

(define (choose-invoke window-name)
  (choose-by-window-name
   window-name cv_do_action fv_do_action))

;; Take the high-level menu representation of an ‘enabled/action’ menu
;; entry, and use that to fill in a GMenuInfo struct.
(define (fill-menu-item-for-action window-name menu-item menu-entry)
  (assert (menu-entry-is-kind? 'action menu-entry))
  (let ((ti (GMenuItem:ti-ref menu-item))
        (contents (cdr menu-entry)))
    (let ((text      (assq 'text contents))
          (image     (assq 'image contents))
          (fg-color  (assq 'fg-color contents))
          (bg-color  (assq 'bg-color contents))
          (disabled  (assq 'disabled contents))
          (checkable (assq 'checkable contents))
          (checked   (assq 'checked contents))
          (shortcut  (assq 'shortcut contents))
          (action    (assq 'action contents))
          (enabled   (assq 'enabled contents)))
      (GTextInfo:text-is-1byte-set! ti #t)
      (GTextInfo:text-has-mnemonic! ti #t)
      (when text (GTextInfo:text-set! ti (string->pointer (cdr text) "UTF-8")))
      (when image (GTextInfo:image-set! ti (string->pointer (cdr image))))
      (GTextInfo:fg-set! ti (if fg-color (cdr fg-color) color-default))
      (GTextInfo:bg-set! ti (if bg-color (cdr bg-color) color-default))
      (when disabled (GTextInfo:disabled-set! ti (cdr disabled)))
      (when checkable (GTextInfo:checkable-set! ti (cdr checkable)))
      (when checked (GTextInfo:checked-set! ti (cdr checked)))
      (when shortcut (GMenuItem:shortcut-set!
                      menu-item (string->pointer (cdr shortcut) "UTF-8")))
      (GMenuItem:invoke->set! menu-item (choose-invoke window-name))
      (let ((enab (if enabled (cdr enabled) (lambda (view) #t)))
            (act  (if action (cdr action) (lambda (view) *unspecified*))))
        (GMenuItem:mid->set! menu-item (menu-info-add! (cons act enab))))
      )))


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
