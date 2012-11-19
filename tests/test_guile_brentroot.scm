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

(define argc (length (command-line)))

(define brent-values (eval-string (list-ref (command-line) 1)))
(define brent (eval-string (list-ref (command-line) 2)))
(define func (eval-string (list-ref (command-line) 3)))
(define t1 (string->number (list-ref (command-line) 4)))
(define t2 (string->number (list-ref (command-line) 5)))

(define max-iters
  (if (<= 7 argc)
      (string->number (list-ref (command-line) 6))
      -1))

(define tol
  (if (<= 8 argc)
      (string->number (list-ref (command-line) 7))
      -1))

;; FIXME: This currently is not used, because flbrentroot doesn’t
;; accept it as an argument.
(define epsilon
  (if (<= 9 argc)
      (string->number (list-ref (command-line) 8))
      -1))

(receive (root err iter-no)
    (brent-values t1 t2 func #:max-iters max-iters #:tol tol)
  (if (zero? err)
      (begin
        (format #t "err = ~d, root = ~,6f, iter_no = ~d" err root iter-no)
        (if (exact? root)
            (display ", exact")))
      (format #t "err = ~d" err))

  ;; Check that brent returns the same result as brent-values.
  (if (equal? root (brent t1 t2 func #:max-iters max-iters #:tol tol))
      (exit 0)
      (exit 1)))
