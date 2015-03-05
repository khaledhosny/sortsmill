;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2015 Khaled Hosny and Barry Schwartz
;;
;; This file is part of Sorts Mill Tools.
;; 
;; Sorts Mill Tools is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Sorts Mill Tools is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sortsmill editor keyboard-shortcuts)

  (export
   shortcut-ref
   shortcut-set!
   shortcut-remove!
   shortcut-entries
   )

  (import (rnrs)
          (srfi :42))

  (define shortcuts-table (make-hashtable string-hash string=?))

  (define (shortcut-ref key)
    (hashtable-ref shortcuts-table key #f))

  (define (shortcut-set! key value)
    (hashtable-set! shortcuts-table key value))

  (define (shortcut-remove! key)
    (hashtable-delete! shortcuts-table key))

  (define (shortcut-entries)
    (let-values ([(keys values) (hashtable-entries shortcuts-table)])
      (list-ec (:range i (vector-length keys))
        (cons (vector-ref keys i) (vector-ref values i)))))

  ) ;; end of library.
