#! /usr/bin/guile \ 	-*- mode: scheme; coding: utf-8 -*-
--no-auto-compile -s
!#

(import (ff-internal find-imports)
        (sortsmill strings hash-guillemet)
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

(define (sortsmill-only peeled-imports)
  (filter-imports library-is-sortsmill? peeled-imports))

(match (command-line)
  [(_ . file-names)
   (let* ([imports
           (sortsmill-only
            (peel-imports (apply find-imports-in-files #t file-names)))]
          [dot-code (imports->dot imports)])
     (for-each display dot-code))] )