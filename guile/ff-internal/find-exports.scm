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

(library (ff-internal find-exports)

  (export find-exports
          find-exports-in-input
          find-exports-in-files)

  (import (rnrs)
          (only (srfi :26) cut)
          (ice-9 match))

  (define (find-exports library-name expression)
    (match expression
      [('library lib-name ('export . exports-list) . imports-and-body)
       (if (or (eq? library-name #t) (equal? library-name lib-name))
           (cons lib-name exports-list)
           #f)]
      [_ #f]))

  (define find-exports-in-input
    (case-lambda
      [(library-name)
       (find-exports-in-input library-name (current-input-port))]
      [(library-name port)
       (letrec
           ([find-them
             (lambda (prior)
               (let ([expression (read port)])
                 (if (eof-object? expression)
                     prior
                     (let ([exports
                            (find-exports library-name expression)])
                       (if exports
                           (find-them (append prior (list exports)))
                           (find-them prior))))))])
         (find-them '()))] ))

  (define (find-exports-in-files library-name . file-names)
    (let ([find-in-one-file
           (lambda (f)
             (call-with-input-file f
               (cut find-exports-in-input library-name <>)))])
      (fold-left (lambda (prior file-name)
                   (let ([new-exports (find-in-one-file file-name)])
                     (if new-exports
                         (append prior new-exports)
                         prior)))
                 '() file-names)))

  ) ;; end of library.
