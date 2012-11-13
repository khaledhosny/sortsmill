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
 (sortsmillff polyspline)
 ((sortsmillff math-constants) #:select (c-dbl-epsilon))
 (srfi srfi-1)                          ; List operations.
 (srfi srfi-8)                          ; (receive ...)
 (ice-9 format))

(define epsilon c-dbl-epsilon)

(define mono (list->f64vector
              (map string->number (cdr (command-line)))))
(define sbern (f64vector-mono->sbern mono))
(define bern (f64vector-mono->bern mono))
(define mono2s (f64vector-sbern->mono sbern))
(define mono2b (f64vector-bern->mono bern))

(define mono2s-error
  (apply max (map (lambda (pair) (abs (apply - pair)))
                  (zip (f64vector->list mono)
                       (f64vector->list mono2s)))))
(if (< (* 10 epsilon) mono2s-error)
    (exit 10))

(define mono2b-error
  (apply max (map (lambda (pair) (abs (apply - pair)))
                  (zip (f64vector->list mono)
                       (f64vector->list mono2b)))))
(if (< (* 10 epsilon) mono2b-error)
    (exit 20))

(do ((i 0 (1+ i))) ((< 100 i))
  (let* ((t (/ i 100.0))
         (x1 (f64vector-eval-mono mono t))
         (x2 (f64vector-eval-sbern sbern t))
         (x3 (f64vector-eval-bern bern t)))
    (if (< (* 10 epsilon) (abs (- x1 x2)))
        (exit 30))
    (if (< (* 10 epsilon) (abs (- x1 x3)))
        (exit 40))))

(exit 0)
