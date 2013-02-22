#! /usr/bin/guile \ 	-*- mode: scheme; coding: utf-8 -*-
--no-auto-compile -s
!#

(import (ff-internal find-imports)
        (sortsmill hash-guillemet)
        (rnrs)
        (except (guile) error)
        (ice-9 match))

(enable-hash-guillemet-strings)

(set-port-encoding! (current-output-port) "utf-8")

(define (library-is-sortsmill? lib-name)
  (match lib-name
    [('sortsmill _ ...) #t]
    [('ff-internal _ ...) #t]
    [_ #f] ))

(define (lib-name->go-file-name lib-name)
  (string-append (string-join (map symbol->string lib-name) "/") ".go"))

(define (lib-name->scm-file-name lib-name)
  (string-append (string-join (map symbol->string lib-name) "/") ".scm"))

(define (imports->dependencies imports)
  (match imports
    [() '()]
    [((lib-name) . t) (imports->dependencies t)]
    [((lib-name . import-list) . t)
     (cons (string-append (lib-name->go-file-name lib-name) ": "
                          (string-join
                           ;; (map lib-name->scm-file-name import-list))
                           (map lib-name->go-file-name import-list))
                          "\n")
           (imports->dependencies t))] ))

(match (command-line)
  [(_ . file-names)
   (let ([imports (peel-imports
                   (apply find-imports-in-files #t file-names))])
     (for-each display
      (imports->dependencies
       (filter-imports library-is-sortsmill? imports))))])
