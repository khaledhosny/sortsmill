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
(define spline1 '(5 4 -3 2 1 0 1 -2 3 4 5))
(define times '(0 0.25 0.5 0.75 1))

(display "fl_eval_sbern\n")
(do ((deg 0 (1+ deg))) ((= 11 deg))
  (for-each
   (lambda (t)
     (format #t "~,6f|"
             (f64vector-eval-sbern
              (list->f64vector (take spline1 (1+ deg)))
              t)))
   times)
  (display "\n"))

(display "fl_eval_bern\n")
(do ((deg 0 (1+ deg))) ((= 11 deg))
  (for-each
   (lambda (t)
     (format #t "~,6f|"
             (f64vector-eval-bern
              (list->f64vector (take spline1 (1+ deg)))
              t)))
   times)
  (display "\n"))

(display "fl_evaldc_sbern\n")
(do ((deg 0 (1+ deg))) ((= 11 deg))
  (for-each
   (lambda (t)
     (format #t "~,6f|"
             (f64vector-evaldc-sbern
              (list->f64vector (take spline1 (1+ deg)))
              t)))
   times)
  (display "\n"))

(display "fl_evaldc_bern\n")
(do ((deg 0 (1+ deg))) ((= 11 deg))
  (for-each
   (lambda (t)
     (format #t "~,6f|"
             (f64vector-evaldc-bern
              (list->f64vector (take spline1 (1+ deg)))
              t)))
   times)
  (display "\n"))

(display "fl_subdiv_sbern\n");
(do ((deg 0 (1+ deg))) ((= 5 deg))
    (for-each
     (lambda (t)
       (format #t "t=~,6f|" t)
       (receive (a b)
           (f64vector-subdiv-sbern
            (list->f64vector (take spline1 (1+ deg)))
            t)
         (do ((j 0 (1+ j))) ((< deg j))
           (format #t "~,6f|" (f64vector-ref a j)))
         (do ((j 0 (1+ j))) ((< deg j))
           (format #t "~,6f|" (f64vector-ref b j)))
         (display "\n")))
     times))
    
(display "fl_subdiv_bern\n");
(do ((deg 0 (1+ deg))) ((= 5 deg))
    (for-each
     (lambda (t)
       (format #t "t=~,6f|" t)
       (receive (a b)
           (f64vector-subdiv-bern
            (list->f64vector (take spline1 (1+ deg)))
            t)
         (do ((j 0 (1+ j))) ((< deg j))
           (format #t "~,6f|" (f64vector-ref a j)))
         (do ((j 0 (1+ j))) ((< deg j))
           (format #t "~,6f|" (f64vector-ref b j)))

         ;; Check that subdivision gives the same result as
         ;; evaluation.
         (let* ((v (f64vector-eval-bern
                    (list->f64vector (take spline1 (1+ deg)))
                    t))
                (difference (abs (- (f64vector-ref b 0) v)))
                (close-enough? (<= difference (* 10 epsilon))))
           (display (if close-enough? "1" "0")))

         (display "\n")))
     times))
