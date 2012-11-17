;; -*- coding: utf-8 -*-

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

(define-module (sortsmillff precompute)
  #:use-module (srfi srfi-1)            ; List operations.
  #:use-module (srfi srfi-4)            ; Uniform vectors.
  #:use-module ((rnrs) :version (6) #:select (assert))
  #:use-module (sortsmillff linalg)
  #:export (binomial-coefficients
            binomial-coefficients-f64vector
            altsigns
            altsigns-f64vector
            binomial-coefficients-altsigns
            binomial-coefficients-altsigns-f64vector
            sbern-basis-in-mono
            sbern-basis-in-mono-f64vector
            mono-basis-in-sbern
            mono-basis-in-sbern-f64vector
            sbern-basis-in-spower
            sbern-basis-in-spower-f64vector
            spower-basis-in-sbern
            spower-basis-in-sbern-f64vector))

(define (binomial-coefficients n)
  (assert (integer? n))
  (assert (<= 0 n))
  (if (zero? n)
      '(1)
      (let ((previous (binomial-coefficients (1- n))))
        (map + (cons 0 previous) (append previous '(0))))))

(define (binomial-coefficients-f64vector n)
  (let ((bc (binomial-coefficients n)))
    (list->f64vector bc)))

(define (altsigns n)
  (assert (integer? n))
  (assert (<= 0 n))
  (if (zero? n)
      '(1)
      (let* ((previous (altsigns (1- n)))
             (last (car (last-pair previous))))
        (append previous (list (* -1 last))))))

(define (altsigns-f64vector n)
  (let ((as (altsigns n)))
    (list->f64vector as)))

(define (binomial-coefficients-altsigns n)
  (assert (integer? n))
  (assert (<= 0 n))
  (map * (binomial-coefficients n) (altsigns n)))

(define (binomial-coefficients-altsigns-f64vector n)
  (let ((bca (binomial-coefficients-altsigns n)))
    (list->f64vector bca)))

(define (sbern-basis-in-mono n)
  (assert (integer? n))
  (assert (<= 0 n))
  (let ((row (lambda (i)
               (append (make-list i 0)
                       (binomial-coefficients (- n i))))))
    (list-tabulate (1+ n) row)))

(define (sbern-basis-in-mono-f64vector n)
  (let ((basis (sbern-basis-in-mono n)))
    (list->f64vector (apply append basis))))

(define (mono-basis-in-sbern n)
  (assert (integer? n))
  (assert (<= 0 n))
  (let ((row (lambda (i)
               (append (make-list i 0)
                       (binomial-coefficients-altsigns (- n i))))))
    (list-tabulate (1+ n) row)))

(define (mono-basis-in-sbern-f64vector n)
  (let ((basis (mono-basis-in-sbern n)))
    (list->f64vector (apply append basis))))

(define (sbern-basis-in-spower n)
  (assert (integer? n))
  (assert (<= 0 n))
  (let* ((q (+ (quotient n 2) (remainder n 2)))
         (top-row
          (lambda (i) (append (make-list i 0)
                              (binomial-coefficients (- n (* i 2) 1))
                              (make-list (1+ i) 0))))
         (middle-row
          (lambda () (append (make-list q 0)
                             '(1)
                             (make-list q 0))))
         (bottom-row
          (lambda (i) (reverse (top-row (- q i 1))))))
    (if (even? n)
        (append
         (list-tabulate q top-row)
         (list (middle-row))
         (list-tabulate q bottom-row))
        (append
         (list-tabulate q top-row)
         (list-tabulate q bottom-row)))))

(define (sbern-basis-in-spower-f64vector n)
  (let ((basis (sbern-basis-in-spower n)))
    (list->f64vector (apply append basis))))

(define (spower-basis-in-sbern n)
  (matrix-invert (sbern-basis-in-spower n)))

(define (spower-basis-in-sbern-f64vector n)
  (let ((basis (spower-basis-in-sbern n)))
    (list->f64vector (apply append basis))))
