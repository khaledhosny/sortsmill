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

(library (ff-internal find-imports)

  (export find-imports
          find-imports-in-input
          find-imports-in-files
          filter-imports
          peel-imports
          peel-import
          imports->dot)

  (import (rnrs)
          (only (srfi :1) delete-duplicates)
          (only (srfi :26) cut)
          (only (guile) set-port-encoding!)
          (ice-9 match)
          (ice-9 format))

  (define (find-imports library-name expression)
    (match expression
      [(or ('library lib-name ('export . exports-list) ('import . imports-list) . body)
           ('library lib-name ('import . imports-list) . body)) ; ← Is this pattern possible?
       (if (or (eq? library-name #t) (equal? library-name lib-name))
           (cons lib-name imports-list)
           #f)]
      [_ #f]))

  (define find-imports-in-input
    (case-lambda
      [(library-name)
       (find-imports-in-input library-name (current-input-port))]
      [(library-name port)
       (letrec
           ([find-them
             (lambda (prior)
               (let ([expression (read port)])
                 (if (eof-object? expression)
                     prior
                     (let ([imports
                            (find-imports library-name expression)])
                       (if imports
                           (find-them (append prior (list imports)))
                           (find-them prior))))))])
         (find-them '()))] ))

  (define (find-imports-in-files library-name . file-names)
    (let ([find-in-one-file
           (lambda (f)
             (with-input-from-file f
               (lambda ()
                 (set-port-encoding! (current-input-port) "utf-8")
                 (find-imports-in-input library-name))))])
      (fold-left (lambda (prior file-name)
                   (let ([new-imports (find-in-one-file file-name)])
                     (if new-imports
                         (append prior new-imports)
                         prior)))
                 '() file-names)))

  (define (filter-imports pred? imports)
    (map (match-lambda [(lib-name . imports-list)
                        (cons lib-name (filter pred? imports-list))])
         imports))

  (define (peel-imports imports)
    (map (match-lambda [(lib-name . imports-list)
                        (cons lib-name (map peel-import imports-list))])
         imports))

  (define (peel-import import-spec)
    (match import-spec
      [(? list-of-symbols? _) import-spec]
      [(_ *** (and (? list-of-symbols? _) inner-part)) inner-part] ))

  (define (list-of-symbols? lst)
    (for-all symbol? lst))

  (define (imports->dot imports)
    `["digraph MyGraph {\n"
      ,@(delete-duplicates (imports->dot-fragments (peel-imports imports)))
      "}\n"] )

  (define (imports->dot-fragments imports)
    (fold-left
     (lambda (prior imports-entry)
       (append prior (imports-entry->dot-fragments imports-entry)))
     '() imports))

  (define (imports-entry->dot-fragments imports-entry)
    (match imports-entry
      [(lib-name . imports-list)
       (map (lambda (imp)
              (format #f "~a -> ~a;\n"
                      (library-name->node-name lib-name)
                      (library-name->node-name imp)))
            imports-list)] ))

  (define (library-name->node-name lib-name)
    (format #f "\"~a\"" lib-name))

  ) ;; end of library.
