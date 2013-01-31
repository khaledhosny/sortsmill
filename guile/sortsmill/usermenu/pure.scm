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

(library (sortsmill usermenu pure)

  (export pure-menu-entry-function->procedure)

  (import (sortsmill pure)
          (sortsmill fontforge-api)
          (sortsmill gdraw-api)
          (sortsmill views)
          (rnrs))

  (define glyph-view-pointer-type "CharViewBase*")
  (define glyph-view-pointer-tag (pure-pointer-tag glyph-view-pointer-type))

  (define font-view-pointer-type "FontViewBase*")
  (define font-view-pointer-tag (pure-pointer-tag font-view-pointer-type))

  (define view->pure-view
    (lambda (v)
      (assert (view? v))
      (let ([p (pointer->pointer-pure-expr (view->pointer v))])
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
        (let ([f-wrapped (pure-apply pure-menu-entry-exc-handler f)])
          (lambda (view)
            (let ([result (pure-apply f-wrapped (view->pure-view view))])
              (cond
               [(pure-expr-is-small-integer? result)
                ;; The ‘result’ is a small integer, which in Pure
                ;; doubles service as a boolean. (That is unfortunate.
                ;; See http://en.wikipedia.org/wiki/Therac-25 for an
                ;; example of what can happen when integers are used
                ;; to represent booleans.)
                (not (fxzero? (pure-expr->small-integer result)))]
               [else
                ;; The ‘result’ is not a boolean. Return #f to make it
                ;; more likely, perhaps, that breakage of an ‘enabled’
                ;; function will be noticed.
                #f] ))))] )))

  ) ;; end of library.
