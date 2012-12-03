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

;;
;; FIXME: Rename these functions and use keyword parameters with
;; settings to select implementation, maybe including an 'auto mode
;; that chooses the C implementation if t1 or t2 is ‘inexact’.
;;

(define-module (sortsmillff brentroot)
  #:use-module ((rnrs) :version (6))
  #:use-module (srfi srfi-31)           ; (rec ...)
  #:use-module (sortsmillff math-constants)
  #:export (flbrentroot
            flbrentroot-values
            qbrentroot
            qbrentroot-values))

(load-extension "libguile-sortsmillff_brentroot"
                "init_guile_sortsmillff_brentroot")

;;-------------------------------------------------------------------------

(define* (flbrentroot-values t1 t2 func #:key (max-iters -1) (tol -1))
  (f64-brentroot max-iters tol t1 t2 func))

(define* (flbrentroot t1 t2 func #:key (max-iters -1) (tol -1))
  (let-values (((root _err _iter-no)
                (f64-brentroot max-iters tol t1 t2 func)))
    root))

;;-------------------------------------------------------------------------

(define* (qbrentroot-values t1 t2 func
                            #:key (max-iters -1) (tol -1) (epsilon -1))
  (mpq-brentroot max-iters tol epsilon t1 t2 func))

(define* (qbrentroot t1 t2 func
                     #:key (max-iters -1) (tol -1) (epsilon -1))
  (let-values (((root _err _iter-no)
                (mpq-brentroot max-iters tol epsilon t1 t2 func)))
    root))

;;-------------------------------------------------------------------------
