;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-

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

(library (ff-internal extract-dynlink)

  (export extract-dynlink-symbols
          extract-dynlink-symbols-from-input
          extract-dynlink-symbols-from-files
          write-declarations-for-dynlink-symbols
          write-statements-using-dynlink-symbols
          write-c-code-using-dynlink-symbols)

  (import (sortsmill pkg-info python)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) delete-duplicates)
          (ice-9 match)
          (ice-9 format))

  (define (extract-dynlink-symbols expression)
    (match expression
      [[(or 'sortsmill-dynlink-pointer 'sortsmill-dynlink-func)
        (? string? func-name)
        (? string? declarations)]
       ;; Use of a symbol, with inline C declarations.
       (list (list func-name declarations))]

      [[(or 'sortsmill-dynlink-pointer 'sortsmill-dynlink-func)
        (? string? func-name)]
       ;; Use of a symbol, without inline C declarations.
       (list (list func-name ""))]

      [['sortsmill-dynlink-declarations
        (? string? declarations)]
       ;; Standalone C declarations.
       (list (list "" declarations))]

      [['sortsmill-dynlink-load-extension
        (? string? init-func-name)]
       (list (list init-func-name
                   (format #f "void ~a (void);" init-func-name)))]

      [(h . t) (append (extract-dynlink-symbols h)
                       (extract-dynlink-symbols t))]

      [_ '()] ))

  (define* (extract-dynlink-symbols-from-input
            #:optional (port (current-input-port)))
    (let ([expression (read port)])
      (cond [(eof-object? expression) '()]
            [else (append
                   (extract-dynlink-symbols expression)
                   (extract-dynlink-symbols-from-input port))]) ))

  (define (extract-dynlink-symbols-from-files file-name . more-names)
    (apply append
           (call-with-input-file file-name
             extract-dynlink-symbols-from-input)
           (map (lambda (f)
                  (with-input-from-file f
                    (lambda ()
                      (set-port-encoding! (current-input-port) "utf-8")
                      (extract-dynlink-symbols-from-input))))
                more-names)))

  (define* (write-declarations-for-dynlink-symbols
            dynlink-data #:optional (port (current-output-port)))
    (for-each [lambda (decl)
                (unless (string=? decl "")
                  (format port "~a\n" decl))]
              [delete-duplicates (map cadr dynlink-data)] ))

  (define* (write-statements-using-dynlink-symbols
            dynlink-data #:optional (port (current-output-port)))
    (for-each [lambda (sym)
                (unless (string=? sym "")
                  (format port "  printf (\"%p\", &~a);\n" sym))]
              [delete-duplicates (map car dynlink-data)] ))

  (define* (write-c-code-using-dynlink-symbols
            dynlink-data #:optional (port (current-output-port)))
    (format port "#include <config.h>\n")
    (when pkg-info:have-python-api?
      ;; In our experience, Python headers are badly behaved and
      ;; prefer to appear early. So include them here, and leave them
      ;; out of your dynlink calls.
      (format port "#include <Python.h>\n"))
    (format port "#include <stdio.h>\n")
    (format port "\n")
    (write-declarations-for-dynlink-symbols dynlink-data port)
    (format port "\n")
    (format port "void function_that_uses_the_dynlink_symbols (void);\n")
    (format port "\n")
    (format port "void\n")
    (format port "function_that_uses_the_dynlink_symbols (void)\n")
    (format port "{\n")
    (write-statements-using-dynlink-symbols dynlink-data port)
    (format port "}\n"))

  ) ;; end of library.
