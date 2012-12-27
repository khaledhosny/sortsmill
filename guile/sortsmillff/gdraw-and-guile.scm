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

;;;
;;; Preliminary work on reworking gdraw material in Guile.
;;;
;;; As part of the transition, to keep the program running during the
;;; transition, provide conversions between gdraw data structures and
;;; Scheme objects.
;;;

(define-module (sortsmillff gdraw-and-guile))

(use-modules
   (sortsmillff gdraw-types)
;;   ((sortsmillff internal-types-syntax) #:select
;;                                        (struct-field->pointer-func))
   )

;;(eval-when (compile load eval)
;;   (define (normalize-names s)
;;      (cond
;;         ((list? s) (map normalize-names s))
;;         ((symbol? s) (symbol->string s))
;;         ((string? s) s)
;;         (else (scm-error 'wrong-type-arg
;;                  "normalize-name"
;;                  "Expected string or symbol, but got: ~S"
;;                  (list s)
;;                  (list s)))))
;;   )
;;
;;(define-syntax-rule (define:struct->alist func struct fields)
;;   (define (func)
;;      (let* ((struct-name (normalize-names struct))
;;             (field-names (normalize-names fields))
;;             (get-field-funcs
;;                (map
;;                   (lambda (f) (f . (eval-string
;;                                       (string-append "get-"
;;                                          struct-name "-" f))))
;;                   field-names)))
;;         (write get-field-funcs))))
;;
;;(define:struct->alist GTextInfo->alist GTextInfo (text image))
;;GTextInfo->alist)


   
   
#!
(define (GTextInfo->alist ti)
   (list
       uint32_t *text;
    GImage *image;
    Color fg;
    Color bg;
    void *userdata;
    GFont *font;
    bool disabled;
    bool image_precedes;
    bool checkable;		/* Only for menus */
    bool checked;		/* Only for menus */
    bool selected; /* Only for lists (used internally for menu(bar)s, when cursor is on the line) */
    bool line;	   /* Only for menus */
    bool text_is_1byte;	/* If passed in as 1byte (ie. iso-8859-1) text, will be converted */
    bool text_has_mnemonic; /* the text field is actually an index into the string resource table */
    bool changed;	    /* If a row/column widget changed this */
    uint32_t mnemonic;	    /* Only for menus and menubars; should
!#
