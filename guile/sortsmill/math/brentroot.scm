;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Barry Schwartz
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

(library (sortsmill math brentroot)

  (export brentroot-f64
          brentroot-f64-values
          brentroot-mpq
          brentroot-mpq-values)

  (import (sortsmill math math-constants)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_math_brentroot"))

  ;;-------------------------------------------------------------------------

  (define* (brentroot-f64-values t1 t2 func
                                 #:key (max-iters -1) (tol -1) (epsilon -1))
    (f64-brentroot max-iters tol epsilon t1 t2 func))

  (define* (brentroot-f64 t1 t2 func
                          #:key (max-iters -1) (tol -1) (epsilon -1))
    (let-values (((root _err _iter-no)
                  (f64-brentroot max-iters tol epsilon t1 t2 func)))
      root))

  ;;-------------------------------------------------------------------------

  (define* (brentroot-mpq-values t1 t2 func
                                 #:key (max-iters -1) (tol -1) (epsilon -1))
    (mpq-brentroot max-iters tol epsilon t1 t2 func))

  (define* (brentroot-mpq t1 t2 func
                          #:key (max-iters -1) (tol -1) (epsilon -1))
    (let-values (((root _err _iter-no)
                  (mpq-brentroot max-iters tol epsilon t1 t2 func)))
      root))

  ;;-------------------------------------------------------------------------

  ) ;; end of library.
