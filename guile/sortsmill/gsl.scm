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

(library (sortsmill gsl)

  (export

   ;; Reëxported from (sortsmill gsl error).
   gsl-set-error-handler
   gsl-set-error-handler-default
   gsl-set-error-handler-off
   raise-gsl-error
   procedure->gsl-error-handler
   gsl-error-handler:raise-gsl-error
   gsl-set-error-handler:raise-gsl-error
   make-gsl-error-condition
   gsl-error-condition?
   make-gsl-errno-condition
   gsl-errno-condition?
   condition-gsl-errno
   make-gsl-errno-symbol-condition
   gsl-errno-symbol-condition?
   condition-gsl-errno-symbol
   make-gsl-strerror-condition
   gsl-strerror-condition?
   condition-gsl-strerror
   GSL_SUCCESS
   GSL_FAILURE
   GSL_CONTINUE
   GSL_EDOM
   GSL_ERANGE
   GSL_EFAULT
   GSL_EINVAL
   GSL_EFAILED
   GSL_EFACTOR
   GSL_ESANITY
   GSL_ENOMEM
   GSL_EBADFUNC
   GSL_ERUNAWAY
   GSL_EMAXITER
   GSL_EZERODIV
   GSL_EBADTOL
   GSL_ETOL
   GSL_EUNDRFLW
   GSL_EOVRFLW
   GSL_ELOSS
   GSL_EROUND
   GSL_EBADLEN
   GSL_ENOTSQR
   GSL_ESING
   GSL_EDIVERGE
   GSL_EUNSUP
   GSL_EUNIMPL
   GSL_ECACHE
   GSL_ETABLE
   GSL_ENOPROG
   GSL_ENOPROGJ
   GSL_ETOLF
   GSL_ETOLX
   GSL_ETOLG
   GSL_EOF
   gsl-errno->symbol
   gsl-errno-symbols
   gsl-errno->strerror)

  (import (sortsmill gsl error))

  ) ;; end of library.
