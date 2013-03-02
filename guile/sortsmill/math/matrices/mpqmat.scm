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

(library (sortsmill math matrices mpqmat)

  (export mpqmat?
          pointer->mpqmat
          mpqmat->pointer
          matrix->mpqmat
          mpqmat->matrix)

  (import (sortsmill math matrices base)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (system foreign)
          (ice-9 format))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_math_matrices_mpqmat"))

  (define-wrapped-pointer-type mpqmat
    mpqmat? pointer->mpqmat mpqmat->pointer
    (lambda (matrix port)
      (format port "#<mpqmat ~s 0x~x>"
              (zero-based (mpqmat->matrix matrix))
              (pointer-address (mpqmat->pointer matrix)))))

  (define (private:mpqmat? mpqmat)
    (mpqmat? mpqmat))

  (define (private:pointer->mpqmat p)
    (pointer->mpqmat p))

  (define (private:mpqmat->pointer mpqmat)
    (mpqmat->pointer mpqmat))

  ) ;; end of library.
