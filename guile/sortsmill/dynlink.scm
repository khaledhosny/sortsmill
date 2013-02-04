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

(library (sortsmill dynlink)

  (export sortsmill-dynlink-dll
          sortsmill-dynlink-func
          extract-dynlink-symbols
          extract-dynlink-symbols-from-input
          extract-dynlink-symbols-from-files
          write-dynlink-include-files
          write-dynlink-symbol-imports
          write-dynlink-symbol-use-statements
          write-dynlink-symbol-use-c-code)

  (import (rnrs)
          (except (guile) error)
          (ice-9 match)
          (ice-9 format))

  (eval-when (compile load eval)
    (define sortsmill-dynlink-dll
      (dynamic-link "libguile-sortsmill_symbols")))

  (define (sortsmill-dynlink-func func-name include-files)
    (dynamic-func func-name sortsmill-dynlink-dll))

  (define extract-dynlink-symbols
    (match-lambda
     [['sortsmill-dynlink-func
       (? string? symbol-name)
       (? string? include-files)]
      (list (list symbol-name include-files))]
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

  (define (extract-dynlink-symbols-from-file-list file-names)
    (append (map extract-dynlink-symbols-from-file file-names)))

  (define* (write-dynlink-include-files
            dynlink-data #:optional (port (current-output-port)))
    (match dynlink-data
      ['() *unspecified*]
      [((symbol-name include-files) . tail)
       (format port "~a\n" include-files)
       (write-dynlink-include-files tail port)] ))

  (define* (write-dynlink-symbol-use-statements
            dynlink-data #:optional (port (current-output-port)))
    (match dynlink-data
      ['() *unspecified*]
      [((symbol-name include-files) . tail)
       (format port "  printf (\"%p\", &~a);\n" symbol-name)
       (write-dynlink-symbol-use-statements tail port)] ))

  (define* (write-dynlink-symbol-use-c-code
            dynlink-data #:optional (port (current-output-port)))
    (format port "#include <config.h>\n")
    (format port "#include <stdio.h>\n")
    (format port "\n")
    (write-dynlink-include-files dynlink-data port)
    (format port "\n")
    (format port "void function_that_imports_symbols (void);\n")
    (format port "\n")
	(format port "void\n")
	(format port "function_that_imports_symbols (void)\n")
	(format port "{\n")
    (write-dynlink-symbol-use-statements dynlink-data port)
	(format port "}\n"))

  ) ;; end of library.
