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

(library (ff-internal reexporters)

  (export generate-reexporter
          pretty-print-reexporter)

  (import (ff-internal find-exports)
          (rnrs)
          (only (srfi :1) delete-duplicates)
          (ice-9 pretty-print))

  (define (generate-reexporter reexporter-name library-names file-names)
    (let ([reexports
           (fold-left
            (lambda (prior lib-name)
              (append
               prior (apply find-exports-in-files lib-name file-names)))
            '() library-names)])
      `(library ,reexporter-name
         (export ,@(delete-duplicates
                    (fold-left append '() (map cdr reexports))))
         (import ,@(delete-duplicates (map car reexports))))))

  (define (pretty-print-reexporter reexporter-name library-names
                                   file-names)
    (pretty-print (generate-reexporter reexporter-name library-names
                                       file-names)))

  ) ;; end of library.
