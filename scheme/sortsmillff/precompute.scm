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
  #:use-module (srfi srfi-11)           ; (let-values ...)
  #:use-module ((rnrs) :version (6) #:select (assert))
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
            invert-by-gauss-jordan))

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

;;-------------------------------------------------------------------------

(define (invert-by-gauss-jordan mat)
  (let ((n (length mat)))

    ;; Check that the rows are all of length @var{n}.
    (assert (fold (lambda (row truth) (and (= (length row) n)) truth) #t mat))

    (let* ((augmented-mat (map append mat (identity-matrix n)))

           (gauss-jordan-step
            (lambda (i amat)
              (let ((find-pivot
                     (lambda (k)
                       (let* ((pivot-data
                               (fold (lambda (candidate current)
                                       (if (< (cadr current) (cadr candidate))
                                           candidate
                                           current))
                                     (list k (abs (list-ref (list-ref amat k) k)))
                                     (map (lambda (r)
                                            (list r (abs (list-ref (list-ref amat r) k))))
                                          (iota (- n k 1) (1+ k)))))
                              (i-pivot (car pivot-data))
                              (pivot-size (cadr pivot-data)))
                         (if (zero? pivot-size)
                             (error "singular matrix:" mat))
                         i-pivot))))
                (let* ((p (find-pivot i))
                       (m1 (swap-list-elements amat i p))
                       (m2 (normalize-row m1 i)))
                  m2))))
                
           (inv-amat (fold gauss-jordan-step augmented-mat (iota n))))

      (map (lambda (row) (drop row n)) inv-amat))))

;; Adjust the lead coefficient of row @var{i} to equal 1.
;; Adjust all the other rows so they are zero in column @var{i}.
(define (normalize-row amat i)
  (let*-values
      (((top-rows other-rows) (split-at amat i))
       ((row) (car other-rows))
       ((bottom-rows) (cdr other-rows))
       ((prefix the-rest) (split-at row i))
       ((lead) (car the-rest))
       ((new-row) (append prefix (map (lambda (x) (/ x lead)) the-rest)))
       ((adjust-row)
        (lambda (row)
          (let*-values
              (((prefix the-rest) (split-at row i))
               ((value) (car the-rest))
               ((pivrow) (map (lambda (x) (* x value)) (drop new-row i))))
            (append prefix (map (lambda (x p) (- x p)) the-rest pivrow))))))
    (append (map adjust-row top-rows)
            (list new-row)
            (map adjust-row bottom-rows))))

(define (identity-matrix n)
  (let ((ident-row (lambda (i)
                     (append (make-list i 0)
                             '(1)
                             (make-list (- n i 1) 0)))))
    (list-tabulate n ident-row)))

(define (swap-list-elements lst i j)
  (if (= i j)
      lst
      (let*-values
          (((i1) (min i j))
           ((i2) (max i j))
           ((prefix after-prefix) (split-at lst i1))
           ((elem1) (car after-prefix))
           ((middle after-middle) (split-at (cdr after-prefix)
                                            (- i2 i1 1)))
           ((elem2) (car after-middle))
           ((suffix) (cdr after-middle)))
        (append prefix (list elem2) middle (list elem1) suffix))))

;;-------------------------------------------------------------------------
