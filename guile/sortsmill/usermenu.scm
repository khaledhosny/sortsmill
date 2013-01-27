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

(@ (sortsmill strings) enable-hash-guillemet-strings)

(library
 (sortsmill usermenu)

 (export glyph-view-tools
         font-view-tools
         activate-gui-tools
         activate-glyph-view-tools
         activate-font-view-tools

         action-entry
         separator-line
         submenu-entry

         c-menu-entry-action->procedure
         c-menu-entry-enabled->procedure

         register-fontforge-menu-entry   ; FIXME: Get rid of this.
         )

 (import (sortsmill fontforge-api)
         (sortsmill gdraw-api)
         (sortsmill i18n)
         (sortsmill machine)
         (sortsmill notices)
         (sortsmill pure)
         (sortsmill python)
         (sortsmill views)
         (rnrs)
         (only (guile) *unspecified* define* load-extension
               dynamic-func dynamic-link dynamic-pointer
               negate)
         (rename (guile) (export guile-export))
         (srfi :1)
         (only (srfi :26) cut)
         (only (srfi :27) random-integer)
         (only (ice-9 match) match match-lambda)
         (only (system foreign) sizeof void
               %null-pointer null-pointer?
               pointer? pointer-address string->pointer
               procedure->pointer pointer->procedure
               pointer->bytevector))

 (if-fontforge-has-pure-api
  (guile-export pure-menu-entry-function->procedure))

 (if-fontforge-has-python-api
  (guile-export python-menu-entry-callable->procedure))

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

 (define (window-name->flag window-name)
   (if (symbol? window-name)
       (window-name->flag (symbol->string window-name))
       (cond
        ((string-ci=? window-name "glyph") glyph-view-flag)
        ((string-ci=? window-name "char") glyph-view-flag)
        ((string-ci=? window-name "font") font-view-flag)
        (else (error 'window-name->flag
                     (_ "expected \"glyph\" or \"font\" for window name")
                     window-name)))))

 (define GDrawGetUserData
   (pointer->procedure
    '* (dynamic-func "GDrawGetUserData" (dynamic-link)) '(*)))

 (define (moveto-proc window-ptr menu-item-ptr event)
   (tools-list-check menu-item-ptr
                     (pointer->view (GDrawGetUserData window-ptr))))

 (define (invoke-proc window-ptr menu-item-ptr event)
   (do-action menu-item-ptr
              (pointer->view (GDrawGetUserData window-ptr))))

 (define (procedure->menu-func-pointer proc)
   (procedure->pointer void proc '(* * *)))

 (define moveto-proc-ptr
   (procedure->menu-func-pointer moveto-proc))

 (define invoke-proc-ptr
   (procedure->menu-func-pointer invoke-proc))

 ;;-------------------------------------------------------------------------

 (define color-default #xfffffffe)

 (define glyph-view-tools #f)
 (define font-view-tools #f)

 (define (tools-ref window-name)
   (let ((flag (window-name->flag window-name)))
     (cond
      ((= flag glyph-view-flag) glyph-view-tools)
      ((= flag font-view-flag) font-view-tools))))

 (define (tools-set! window-name new-tools)
   (let ((flag (window-name->flag window-name)))
     (cond
      ((= flag glyph-view-flag) (set! glyph-view-tools new-tools))
      ((= flag font-view-flag) (set! font-view-tools new-tools)))))

 (define glyph-view-tools-internal
   (pointer->bytevector
    (dynamic-pointer "cv_menu" (dynamic-link)) (sizeof '*)))

 (define font-view-tools-internal
   (pointer->bytevector
    (dynamic-pointer "fv_menu" (dynamic-link)) (sizeof '*)))

 (define-syntax bytevector-address-native-set!
   (lambda (x)
     (syntax-case x ()
       ((_ bv index value)
        (match (sizeof '*)
          (4 #'(bytevector-u32-native-set! bv index value))
          (8 #'(bytevector-u64-native-set! bv index value))
          (else (error
                 'bytevector-address-native-set!
                 ;; Do not bother putting this in translation files.
                 "we do not know how to handle addresses of this size"
                 (sizeof '*))
                #'#f))))))

 (define (bytevector-pointer-native-set! bv index p)
   (bytevector-address-native-set! bv index (pointer-address p)))

 (define (set-pointer! bv p)
   (bytevector-pointer-native-set! bv 0 p))

 (define (activate-gui-tools)
   (activate-glyph-view-tools)
   (activate-font-view-tools))

 (define (activate-glyph-view-tools)
   (let ((p (if glyph-view-tools
                (GMenuItem->pointer
                 (menu-entry-list->menu-items "glyph" glyph-view-tools))
                %null-pointer)))
     (set-pointer! glyph-view-tools-internal p)))

 (define (activate-font-view-tools)
   (let ((p (if font-view-tools
                (GMenuItem->pointer
                 (menu-entry-list->menu-items "font" font-view-tools))
                %null-pointer)))
     (set-pointer! font-view-tools-internal p)))

 ;;
 ;; FIXME: More thorough and better modularized error checking.
 ;;
 (define (fill-menu-item! window-name menu-item menu-entry)
   (check-menu-entry menu-entry)
   (set-menu-item-defaults! menu-item)
   (let ((view-flag (window-name->flag window-name))
         (action (assq 'action menu-entry))
         (enabled (assq 'enabled menu-entry)))
     (when (and enabled (not action))
       (error 'fill-menu-item!
              (_ "a menu entry has an 'enabled field but no 'action field")
              menu-entry))
     (if action
         (begin
           (when (or (assq 'invoke menu-entry) (assq 'moveto menu-entry))
             (error 'fill-menu-item!
                    (_ "a menu entry has both an 'action field and an 'invoke or 'moveto field")
                    menu-entry))
           (let ((integer-key (menu-info-add!
                               (list (cadr action)
                                     (if enabled (cadr enabled) #f)))))
             
             (for-each
              (match-lambda ((key . value) (set-menu-item-value!
                                            window-name menu-item key value)))
              `((integer-key ,integer-key)
                (invoke      ,invoke-proc-ptr)
                ,@menu-entry))))
         (for-each
          (match-lambda ((key . value) (set-menu-item-value!
                                        window-name menu-item key value)))
          menu-entry))))

 (define (set-menu-item-defaults! menu-item)
   (bytevector-fill! (cdr menu-item) 0)
   (let ((ti (GMenuItem:ti-ref menu-item)))
     (GTextInfo:fg-set! ti color-default)
     (GTextInfo:bg-set! ti color-default)
     (GTextInfo:text-is-1byte-set! ti #t)
     (GTextInfo:text-has-mnemonic-set! ti #t)
     (GTextInfo:image-precedes-set! ti #t)))

 (define (set-menu-item-value! window-name menu-item key value)
   (let ((mi menu-item)
         (ti (GMenuItem:ti-ref menu-item))
         (v (car value)))
     (match key
       ('text (GTextInfo:text-set! ti (string->pointer v "UTF-8")))
       ('image (GTextInfo:image-set! ti (string->pointer v)))
       ('foreground-color (GTextInfo:fg-set! ti v))
       ('background-color (GTextInfo:bg-set! ti v))
       ('disabled (GTextInfo:disabled-set! ti v))
       ('image-precedes-text (GTextInfo:image-precedes-set! ti v))
       ('checkable (GTextInfo:checkable-set! ti v))
       ('checked (GTextInfo:checked-set! ti v))
       ('is-line (GTextInfo:line-set! ti v))
       ('shortcut (GMenuItem:shortcut-set! mi (string->pointer v "UTF-8")))
       ('integer-key (GMenuItem:mid-set! mi v))
       ('moveto (GMenuItem:moveto-set! mi (force-to-menu-func-pointer v)))
       ('invoke (GMenuItem:invoke-set! mi (force-to-menu-func-pointer v)))
       ('enabled *unspecified*)
       ('action *unspecified*)
       ('submenu (GMenuItem:sub-set!
                  mi (GMenuItem->pointer
                      (menu-entry-list->menu-items window-name v))))
       (else (format (current-error-port)
                     (_ "Ignoring menu entry field ~s\n") key))
       )))

 (define (check-menu-entry menu-entry)
   (unless (if (list? menu-entry) (for-all pair? menu-entry) #f)
     (error 'check-menu-entry
            (_ "expected an association list") menu-entry))
   (for-each (match-lambda ((key . value)
                            (check-menu-entry-key key)
                            (check-menu-entry-value value)))
             menu-entry))

 (define (check-menu-entry-key key)
   (unless (symbol? key)
     (error 'check-menu-entry-key (_ "expected a symbol") key)))

 (define (check-menu-entry-value value)
   (unless (list? value)
     (error 'check-menu-entry-value (_ "expected a list") value))
   (unless (= 1 (length value))
     (error 'check-menu-entry-value (_ "expected a list of length 1") value)))

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

 (define* (action-entry #:key text action
                        (enabled (lambda (view) #t))
                        (shortcut #f)
                        (image #f)
                        (foreground-color color-default)
                        (background-color color-default)
                        (disabled? #f)
                        (checkable? #f)
                        (checked? #f)
                        (image-precedes-text? #t))
   (let ([action^ (cond [(pycallable? action)
                         (python-menu-entry-callable->procedure action)]
                        [else action])]
         [enabled^ (cond [(pycallable? enabled)
                          (python-menu-entry-callable->procedure enabled)]
                         [else enabled])])
     ;;
     ;; FIXME: Check types of input parameters.
     ;;
     (append `[(text      ,text)
               (action    ,action^)
               (enabled   ,enabled^)
               (foreground-color ,foreground-color)
               (background-color ,background-color)
               (disabled  ,disabled?)
               (checkable ,checkable?)
               (checked   ,checked?)
               (image-precedes-text ,image-precedes-text?)]
             [if image `((image ,image)) '()]) ))

 (define (separator-line)
   `[(is-line #t)])

 (define* (submenu-entry #:key text entries
                         (moveto moveto-proc)
                         (image #f)
                         (foreground-color color-default)
                         (background-color color-default)
                         (disabled? #f)
                         (image-precedes-text? #t))
   ;;
   ;; FIXME: Check types of input parameters.
   ;;
   (let ((moveto-proc-ptr (if (pointer? moveto-proc)
                              moveto-proc
                              (procedure->menu-func-pointer moveto-proc))))
     (append `[(text      ,text)
               (foreground-color ,foreground-color)
               (background-color ,background-color)
               (disabled  ,disabled?)
               (image-precedes-text ,image-precedes-text?)
               (submenu ,entries)]
             [if image `((image ,image)) '()])))

 ;;-------------------------------------------------------------------------
;;;;;
;;;;; FIXME: Get rid of these.
;;;;;

 (load-extension "libguile-sortsmill_fontforgeexe"
                 "init_guile_sortsmill_usermenu")

 (define* (register-fontforge-menu-entry #:key window menu-path action
                                         (enabled (lambda (view) #t))
                                         (shortcut #f))
   (let ((window-name (symbol->string window))
         (submenus (drop-right menu-path 1))
         (entry-name (last menu-path)))
     (let ((tools (tools-ref window-name))
           (new-entry (if shortcut
                          `[(text     ,entry-name)
                            (enabled  ,enabled)
                            (action   ,action)
                            (shortcut ,shortcut)]
                          `[(text     ,entry-name)
                            (enabled  ,enabled)
                            (action   ,action)])))
       (tools-set! window-name
                   (insert-menu-entry (if tools tools '())
                                      new-entry submenus))
       (activate-gui-tools))))

 (define (insert-menu-entry tools new-entry submenus)
   (match submenus

     ;; Replacement of a menu entry is not supported.
     (() (append tools (list new-entry)))

     ((submenu-name . more-submenus)
      (let ((is-the-submenu? (cut menu-entry-is-submenu-named?
                                  submenu-name <>)))
        (if (any is-the-submenu? tools)
            [let* ((before (take-while (negate is-the-submenu?) tools))
                   (tail (find-tail is-the-submenu? tools))
                   (the-submenu (car tail))
                   (submenu-tools (cadr (assq 'submenu the-submenu)))
                   (new-submenu-tools (insert-menu-entry
                                       submenu-tools new-entry
                                       more-submenus))
                   (new-submenu (replace-submenu-tools the-submenu
                                                       new-submenu-tools))
                   (after (cdr tail)))
              (append before (list new-submenu) after)]
            [insert-menu-entry
             (append tools (list `[(text    ,submenu-name)
                                   (moveto  ,moveto-proc-ptr)
                                   (submenu ())]))
             new-entry submenus])))))

 (define (replace-submenu-tools the-submenu new-tools)
   (append
    (remp (match-lambda ((key . _) (eq? key 'submenu))) the-submenu)
    `[(submenu ,new-tools)]))

 (define (menu-entry-is-submenu-named? name menu-entry)
   (if (menu-entry-is-submenu? menu-entry)
       (menu-entry-text=? name menu-entry)
       #f))

 (define (menu-entry-is-submenu? menu-entry)
   (not (not (assq 'submenu menu-entry))))

 (define (menu-entry-text=? text menu-entry)
   (let ((text-field (assq 'text menu-entry)))
     (if text-field
         (string=? (cadr text-field) text)
         #f)))

 ;;-------------------------------------------------------------------------
 ;;
 ;; Example use of wrappers to register C, Fortran (using BIND(C)), or
 ;; similar menu functions.
 ;;
 ;;   (let ((dll (dynamic-link "my_extensions"))
 ;;        ((my-data-ptr (bytevector->pointer my-data-bytevector))))
 ;;      (register-fontforge-menu-entry
 ;;            #:window 'glyph
 ;;            #:menu-path '("Tools" "My action")
 ;;            #:action (c-menu-entry-action->procedure
 ;;                        (dynamic-func "my_action" dll) my-data-ptr)
 ;;            #:enabled (c-menu-entry-enabled->procedure
 ;;                         (dynamic-func "my_enabled" dll) my-data-ptr)
 ;;            #:shortcut "My action|F10"))

 (define c-menu-entry-action->procedure
   (case-lambda
     ((c-action) (c-menu-entry-action->procedure c-action %null-pointer))
     ((c-action data)
      (let* ((proc (pointer->procedure void c-action (list '* '*)))
             (wrapped-action (lambda (view) (proc (view->pointer view) data))))
        wrapped-action))))

 (define c-menu-entry-enabled->procedure
   (case-lambda
     ((c-enabled) (c-menu-entry-enabled->procedure c-enabled %null-pointer))
     ((c-enabled data)
      (let* ((proc (pointer->procedure _Bool c-enabled (list '* '*)))
             (wrapped-enabled (lambda (view) (not (zero? (proc (view->pointer view) data))))))
        wrapped-enabled))))

 ;;-------------------------------------------------------------------------

 (if-fontforge-has-pure-api

  (define glyph-view-pointer-type "CharViewBase*")
  (define glyph-view-pointer-tag (pure-pointer-tag glyph-view-pointer-type))

  (define font-view-pointer-type "FontViewBase*")
  (define font-view-pointer-tag (pure-pointer-tag font-view-pointer-type))

  (define view->pure-view
    (lambda (v)
      (assert (view? v))
      (let ((p (pointer->pointer-pure-expr (view->pointer v))))
        (cond [(glyph-view? v) (pure-pointer-cast glyph-view-pointer-tag p)]
              [(font-view? v)  (pure-pointer-cast font-view-pointer-tag p)]
              [else (assert #f)]))))

  ;;
  ;; FIXME: Write a better exception handler.
  ;;
  (define pure-menu-entry-exc-handler
    (pure-eval
     (lines-begin-with
      ";;"
      #«
      ;; using system;
      ;;
      ;; (\menu_entry_func ->
      ;;   (\view ->
      ;;     catch handler (menu_entry_func view)
      ;;       with
      ;;          handler exc = fprintf stderr "Pure exception: %s\n" (str exc)
      ;;          $$ false;
      ;;       end))
      »#
      )))

  (define pure-menu-entry-function->procedure
    (lambda (f)
      "Wrap either an ‘action’ or an ‘enabled’ function that is written
in Pure. If the function is specified as a string, it will first be
evaluated as Pure source code. The Guile return value of the wrapped
function is always a boolean."
      (cond
       [(string? f) (pure-menu-entry-function->procedure (pure-eval f))]
       [else
        (let ((f-wrapped (pure-apply pure-menu-entry-exc-handler f)))
          (lambda (view)
            (let ((result (pure-apply f-wrapped (view->pure-view view))))
              (cond
               ((pure-expr-is-small-integer? result)
                ;; The ‘result’ is a small integer, which in Pure
                ;; doubles service as a boolean. (That is unfortunate.
                ;; See http://en.wikipedia.org/wiki/Therac-25 for an
                ;; example of what can happen when integers are used to
                ;; represent booleans.)
                (not (fxzero? (pure-expr->small-integer result))))
               (else
                ;; The ‘result’ is not a boolean. Return #f to make it
                ;; more likely, perhaps, that breakage of an ‘enabled’
                ;; function will be noticed.
                #f)))))])))
  
  ) ;; end of if-fontforge-has-pure-api

 ;;-------------------------------------------------------------------------

 (if-fontforge-has-python-api

  (define view->python-view
    (lambda (v)
      (assert (view? v))
      (let* ([views (python-module "sortsmill.views")]
             [views-dict (py-dict views)]
             [view-class-name (cond [(glyph-view? v) "glyph_view"]
                                    [(font-view? v)  "font_view"]
                                    [else (assert false)])]
             [view-class (pyindexed-ref views-dict (string->pystring view-class-name))])
        ((pycallable->procedure view-class) (pointer->pylong (view->pointer v))))))

  (define python-menu-entry-callable->procedure
    (lambda (f)
      "Wrap either a Python callable to serve as an ‘action’ or
‘enabled’ function. The Guile return value of the wrapped function is
always a boolean."
      (let ([f-wrapped (pycallable->procedure f)])
        [lambda (view)
          (let ([result (f-wrapped (view->python-view view))])
            (pybool->boolean (py-not-not result)))] )))

  ) ;; end of if-fontforge-has-python-api

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

 (define (register-fontforge-menu-entry-from-c-code
          window menu-path action enabled shortcut)
   (register-fontforge-menu-entry #:window window
                                  #:menu-path menu-path
                                  #:action action
                                  #:enabled enabled
                                  #:shortcut shortcut))

 ;;-------------------------------------------------------------------------

 ) ;; end of library.
