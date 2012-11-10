#! /usr/bin/guile \                 -*- coding: utf-8 -*-
--no-auto-compile -s
!#

;; Copyright (C) 2012 Barry Schwartz
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

(use-modules
 (sortsmillff brentroot)
 (ice-9 receive)
 (ice-9 format))

(define func-string (cadr (command-line)))
(define t1 (string->number (caddr (command-line))))
(define t2 (string->number (cadddr (command-line))))

(define func (eval (call-with-input-string func-string read)
                   (interaction-environment)))

(receive (root err iter-no) (brentroot-values t1 t2 func)
  (if (zero? err)
      (format #t "err = ~d, root = ~,6f, iter_no = ~d" err root iter-no)
      (format #t "err = ~d" err))

  ;; Check that brentroot returns the same result at brentroot-values.
  (if (equal? root (brentroot t1 t2 func))
      (exit 0)
      (exit 1)))
