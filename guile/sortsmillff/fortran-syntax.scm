;; -*- mode: bee; coding: utf-8 -*-

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

(define-module (sortsmillff fortran-syntax))

(use-modules
   (sortsmillff node)
   (srfi srfi-9 gnu)
   (ice-9 format)
   (ice-9 match)
   )

(define-public-node-type fortran-type name kind qualifiers)
(define-public-node-type fortran-type-decl kind fields)
(define-public-node-type fortran-var-decl name type)
(define-public-node-type fortran-func-decl name type args)
(define-public-node-type fortran-overload name procedure)

(export
   display-fortran-type
   display-fortran-type-decl
   display-fortran-var-decl
   display-fortran-overload
   )

;; FIXME: Handle qualifiers.
(define* (display-fortran-type node
            #:optional (port (current-output-port)))
   (if kind
       (format port "~a(~a)"
          (get-fortran-type-name node)
          (get-fortran-type-kind node))
       (format port "~a" (get-fortran-type-name node))))

(define* (display-fortran-var-decl node
            #:optional (port (current-output-port)))
   (display-fortran-type-decl
      (get-fortran-var-decl-type node) port)
   (format port " :: ")
   (format port "~a\n" (get-fortran-var-decl-name node)))

(define* (display-fortran-type-decl node
            #:optional (port (current-output-port)))
   (format port "type ~a\n"
      (get-fortran-type-decl-kind node))
;   (for-each
;      (lambda (f)
;         (match f
;            ((
   (format port "end type ~a\n\n"
      (get-fortran-type-decl-kind node)))

(define* (display-fortran-overload node
            #:optional (port (current-output-port)))
   (format port "interface ~a\n"
      (get-fortran-overload-name node))
   (format port "  module procedure ~a\n"
      (get-fortran-overload-procedure node))
   (format port "end interface ~a\n\n"
      (get-fortran-overload-name node)))
