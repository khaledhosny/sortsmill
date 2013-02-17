;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2013 Barry Schwartz
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

(library (sortsmill options)

  (export option-remaining

          option-arg-none
          option-arg-string
          option-arg-int
          option-arg-callback
          option-arg-filename
          option-arg-string-array
          option-arg-filename-array
          option-arg-double
          option-arg-int64

          option-flag-hidden
          option-flag-in-main
          option-flag-reverse
          option-flag-no-arg
          option-flag-filename
          option-flag-optional-arg
          option-flag-noalias
          )

  (import (sortsmill dynlink)
          (sortsmill kwargs)
          (sortsmill pkg-info)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (system foreign)
          (ice-9 format))

  (sortsmill-dynlink-declarations "#include <glib.h>")

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_options"))

  (define option-remaining "")

  (define/kwargs (option-entry long-name
                               [short-name #\nul]
                               [flags 0]
                               [arg option-arg-none]
                               [arg-data #f]
                               [description #f]
                               [arg-description #f])
    (assert (string? long-name))
    (assert (char? short-name))
    (assert (integer? flags))
    (assert (integer? arg))
    (assert (when description (string? description)))
    (assert (when arg-description (string? arg-description)))
    (assert (<= 0 flags))
    (assert (<= flags (+ option-flag-hidden
                         option-flag-in-main
                         option-flag-reverse
                         option-flag-no-arg
                         option-flag-filename
                         option-flag-optional-arg
                         option-flag-noalias)))
    (assert (member arg (list
                         option-arg-none
                         option-arg-string
                         option-arg-int
                         #|option-arg-callback|# ;; Not yet implemented.
                         option-arg-filename
                         option-arg-string-array
                         option-arg-filename-array
                         option-arg-double
                         option-arg-int64)))
    (list long-name short-name flags arg arg-data description arg-description))

  (define (option-entries entries)
    (map (cut apply option-entry <>) entries))

  (define (list-with-keywords->GOptionEntry-array lst)
    (list->GOptionEntry-array (option-entries lst)))

  (define-wrapped-pointer-type option-context
    option-context?
    pointer->option-context option-context->pointer
    [lambda (obj port)
      (format port "#<option-context 0x~x>"
              (pointer-address (option-context->pointer obj)))] )

  (define option-context-new
    (let ([proc (pointer->procedure
                 '*
                 (sortsmill-dynlink-func "g_option_context_new")
                 '(*))])
      (compose pointer->option-context proc string->pointer)))

  (define option-context-free
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_free")
                 '(*))])
      (compose proc option-context->pointer)))

  (define option-context-add-main-entries
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_add_main_entries")
                 '(* * *))])
      (lambda* (context entries #:optional
                        [translation-domain pkg-info:textdomain])
        (proc (option-context->pointer context)
              (bytevector->pointer
               (list-with-keywords->GOptionEntry-array entries))
              (string->pointer translation-domain)))))

  (define option-context-set-summary
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_set_summary")
                 '(* *))])
      (lambda (context summary)
        (proc (option-context->pointer context)
            (string->pointer summary)))))
  
  (define option-context-set-description
    (let ([proc (pointer->procedure
                 void
                 (sortsmill-dynlink-func "g_option_context_set_description")
                 '(* *))])
      (lambda (context description)
        (proc (option-context->pointer context)
            (string->pointer description)))))
  
(write  (identity (option-context-new "sksksksskks")))
(write  (option-context-free (option-context-new "sksksksskks")))
(write  (option-context-add-main-entries (option-context-new "sksksksskks")
                                         '([#:long-name "foo"]
                                           [#:long-name "bar"])))
(write  (option-context-set-summary (option-context-new "sksksksskks")
                                    "summary"))
(write  (option-context-set-description (option-context-new "sksksksskks")
                                        "description"))


  #| NOT YET IMPLEMENTED FULLY.
  (define (procedure->GOptionArgFunc-pointer proc)
    (let ([wrapped-proc
           (lambda (option-name value data error)
             (proc (pointer->string option-name)
                   (pointer->string value)
                   data SOMETHING-HERE-TO-RECEIVE-ERROR))])
      (procedure->pointer int wrapped-proc '(* * * *)))) |#

  ) ;; end of library.
