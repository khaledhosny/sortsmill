#! @GUILE@ \ -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
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

(import (ff-internal generate-types)
        (sortsmill machine)
        (rnrs)
        (ice-9 match)
        (ice-9 format))

(case intptr_t-size
  [(4 8) (lambda () *unspecified*)]
  [else
   (error "Only sizeof(intptr_t) equal to 4 or 8 is supported. Your sizeof(intptr_t) = ~d."
          intptr_t-size)])

(define (write-declarations instructions)
  (for-each
   write-declarations-for-one-instruction
   instructions))

(define (write-declarations-for-one-instruction instruction)
  (match instruction
    [('struct (? symbol? struct-name) (? integer? size))
     (format #t "type ~a\n" struct-name)
     (format #t "  ! Represent a C struct or union as a bytevector.\n")
     (format #t "  integer(c_int8_t), dimension(:), pointer :: bv\n")
     (format #t "end type ~a\n" struct-name)
     (format #t "\n")
     (format #t "! Constructors for ~a.\n" struct-name)
     (format #t "interface ~a\n" struct-name)
     (format #t "  module procedure c_ptr_to_~a\n" struct-name)
     (format #t "  module procedure c_intptr_t_to_~a\n" struct-name)
     (format #t "end interface ~a\n" struct-name)
     (format #t "\n")]

    [('field (and (or 'struct 'array) field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (format (current-error-port) "Ignoring '~a in declarations\n" field-type)]

    [('field (? symbol? field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (format #t "interface get_~a\n" field-name)
     (format #t "  module procedure get_~a_~a\n" struct-name field-name)
     (format #t "end interface get_~a\n" field-name)
     (format #t "\n")
     (format #t "interface set_~a\n" field-name)
     (format #t "  module procedure set_~a_~a\n" struct-name field-name)
     (format #t "end interface set_~a\n" field-name)
     (format #t "\n")]

    [('field ((and (or '* 'struct 'array) field-type) (? symbol? pointer-type))
             (? symbol? struct-name) (? symbol? field-name) (? integer? offset)
             (? integer? size))
     (write-declarations-for-one-instruction
      (list 'field field-type struct-name field-name offset size))
     ;;
     ;; FIXME: Dereferencing and array procedures go here.
     ;;
     ]

    [('struct-> . _) *unspecified*]     ; Ignore 'struct-> silently.

    [(instruction-symbol . _)
     (format (current-error-port)
             "Ignoring '~a in declarations\n" instruction-symbol)] ))

(define (write-definitions instructions)
  (for-each
   write-definitions-for-one-instruction
   instructions))

(define (write-definitions-for-one-instruction instruction)
  (match instruction
    [('struct (? symbol? struct-name) (? integer? size))
     (begin
       (format #t "type(~a) function c_ptr_to_~a (p) result(q)\n"
               struct-name struct-name)
       (format #t "  type(c_ptr), intent(in) :: p\n")
       (format #t "  call c_f_pointer (p, q%bv, [~d])\n" size)
       (format #t "end function c_ptr_to_~a\n" struct-name)
       (format #t "\n")
       (format #t "type(~a) function c_intptr_t_to_~a (p) result(q)\n"
               struct-name struct-name)
       (format #t "  integer(c_intptr_t), intent(in) :: p\n")
       (format #t "  call c_f_pointer (.cptr. p, q%bv, [~d])\n" size)
       (format #t "end function c_intptr_t_to_~a\n" struct-name)
       (format #t "\n"))]

    [('field (and (or 'struct 'array) field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (format (current-error-port) "Ignoring '~a in definitions\n" field-type)]

    [('field (? symbol? field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (begin
       (write-field-definitions field-type struct-name field-name offset size))]

    [('field ((and (or '* 'struct 'array) field-type) (? symbol? pointer-type))
             (? symbol? struct-name) (? symbol? field-name) (? integer? offset)
             (? integer? size))
     (begin
       (write-definitions-for-one-instruction
        (list 'field field-type struct-name field-name offset size))
       ;;
       ;; FIXME: Dereferencing and array procedures go here.
       ;;
       )]

    [('struct-> . _) *unspecified*]   ; Ignore 'struct-> silently.

    [(instruction-symbol . _)
     (format (current-error-port)
             "Ignoring '~a in definitions\n" instruction-symbol)] ))

(define (write-field-definitions field-type struct-name field-name offset size)
  (match field-type
    ['int (write-int-field-definitions struct-name field-name offset size)]
    ['uint (write-uint-field-definitions struct-name field-name offset size)]
    ['bool (write-bool-field-definitions struct-name field-name offset size)]
    ['float (write-float-field-definitions struct-name field-name offset size)]
    ['* (write-pointer-field-definitions struct-name field-name offset size)]
    ['SCM (write-SCM-field-definitions struct-name field-name offset size)] ))

(define (write-int-field-definitions struct-name field-name offset size)
  (write-get-int-field-definition struct-name field-name offset size)
  (write-set-int-field-definition struct-name field-name offset size))

(define (write-get-int-field-definition struct-name field-name offset size)
  (case size
    [(1 2 4)
     (begin
       (format #t "integer function get_~a_~a (p) result(q)\n" struct-name field-name)
       (format #t "  type(~a), intent(in) :: p\n" struct-name)
       (format #t "  q = int (transfer (p%bv(~d:~d), 1_c_int~d_t), kind (1))\n"
               (1+ offset) (+ offset size) (* 8 size))
       (format #t "end function get_~a_~a\n" struct-name field-name)
       (format #t "\n"))]
    [(8)
     (begin
       (format #t "integer(c_int64_t) function get_~a_~a (p) result(q)\n" struct-name field-name)
       (format #t "  type(~a), intent(in) :: p\n" struct-name)
       (format #t "  q = transfer (p%bv(~d:~d), q)\n" (1+ offset) (+ offset size))
       (format #t "end function get_~a_~a\n" struct-name field-name)
       (format #t "\n"))] ))

(define (write-set-int-field-definition struct-name field-name offset size)
  (case size
    [(1 2 4)
     (begin
       (format #t "subroutine set_~a_~a (p, v)\n" struct-name field-name)
       (format #t "  type(~a), intent(inout) :: p\n" struct-name)
       (format #t "  integer, intent(in) :: v\n")
       (format #t "  p%bv(~d:~d) = transfer (int (v, kind=c_int~d_t), p%bv)\n"
               (1+ offset) (+ offset size) (* 8 size))
       (format #t "end subroutine set_~a_~a\n" struct-name field-name)
       (format #t "\n"))]
    [(8)
     (begin
       (format #t "subroutine set_~a_~a (p, v)\n" struct-name field-name)
       (format #t "  type(~a), intent(inout) :: p\n" struct-name)
       (format #t "  integer(c_int64_t), intent(in) :: v\n")
       (format #t "  p%bv(~d:~d) = transfer (v, p%bv)\n" (1+ offset) (+ offset size))
       (format #t "end subroutine set_~a_~a\n" struct-name field-name)
       (format #t "\n"))] ))

(define (write-uint-field-definitions struct-name field-name offset size)
  (write-get-uint-field-definitions struct-name field-name offset size)
  (write-set-uint-field-definitions struct-name field-name offset size))

(define (write-get-uint-field-definitions struct-name field-name offset size)
  (format #t "! Unsigned ints are returned as INTEGER(KIND=C_INT64_T).\n")
  (when (= size 8)
    (format #t "!\n")
    (format #t "! WARNING: The returned 64-bit integer is SIGNED.\n")
    (format #t "! Unless the unsigned value is very large, this handling\n")
    (format #t "! likely is sufficient. However, in gfortran you can\n")
    (format #t "! get an actual unsigned integer by transferring the\n")
    (format #t "! data to an INTEGER(KIND=C_INT128_T).\n"))
  (format #t "integer(c_int64_t) function get_~a_~a (p) result(q)\n" struct-name field-name)
  (format #t "  type(~a), intent(in) :: p\n" struct-name)
  (format #t "  q = uint~d_to_uint64 (transfer (p%bv(~d:~d), 1_c_int~d_t))\n"
          (* 8 size) (1+ offset) (+ offset size) (* 8 size))
  (format #t "end function get_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-set-uint-field-definitions struct-name field-name offset size)
  (format #t "subroutine set_~a_~a (p, v)\n" struct-name field-name)
  (format #t "  type(~a), intent(inout) :: p\n" struct-name)
  (format #t "  integer(c_int64_t), intent(in) :: v\n")
  (format #t "  p%bv(~d:~d) = transfer (uint64_to_uint~d (v), p%bv)\n"
          (1+ offset) (+ offset size) (* 8 size))
  (format #t "end subroutine set_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-bool-field-definitions struct-name field-name offset size)
  (write-get-bool-field-definition struct-name field-name offset size)
  (write-set-bool-field-definition struct-name field-name offset size))

(define (write-get-bool-field-definition struct-name field-name offset size)
  (format #t "logical function get_~a_~a (p) result(q)\n" struct-name field-name)
  (format #t "  type(~a), intent(in) :: p\n" struct-name)
  (format #t "  q = (transfer (p%bv(~d:~d), 1_c_int~d_t) /= 0)\n"
          (1+ offset) (+ offset size) (* 8 size))
  (format #t "end function get_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-set-bool-field-definition struct-name field-name offset size)
  (format #t "subroutine set_~a_~a (p, v)\n" struct-name field-name)
  (format #t "  type(~a), intent(inout) :: p\n" struct-name)
  (format #t "  logical, intent(in) :: v\n")
  (format #t "  integer(c_int8_t) :: i\n")
  (format #t "  if (v) then\n")
  (format #t "    i = 1_c_int8_t\n")
  (format #t "  else\n")
  (format #t "    i = 0_c_int8_t\n")
  (format #t "  endif\n")
  (format #t "  p%bv(~d:~d) = transfer (uint8_to_uint~d (i), p%bv)\n"
          (1+ offset) (+ offset size) (* 8 size))
  (format #t "end subroutine set_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-float-field-definitions struct-name field-name offset size)
  (write-get-float-field-definition struct-name field-name offset size)
  (write-set-float-field-definition struct-name field-name offset size))

(define (write-get-float-field-definition struct-name field-name offset size)
  (format #t "real(c_double) function get_~a_~a (p) result(q)\n" struct-name field-name)
  (format #t "  type(~a), intent(in) :: p\n" struct-name)
  (format #t "  q = real (transfer (p%bv(~d:~d), 1.0_c_~a), kind=c_double)\n"
          (1+ offset) (+ offset size) (c:float-type size))
  (format #t "end function get_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-set-float-field-definition struct-name field-name offset size)
  (format #t "subroutine set_~a_~a (p, v)\n" struct-name field-name)
  (format #t "  type(~a), intent(inout) :: p\n" struct-name)
  (format #t "  real(c_double), intent(in) :: v\n")
  (format #t "  p%bv(~d:~d) = transfer (real (v, kind=c_~a), p%bv)\n"
          (1+ offset) (+ offset size) (c:float-type size))
  (format #t "end subroutine set_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-pointer-field-definitions struct-name field-name offset size)
  (write-get-pointer-field-definition struct-name field-name offset size)
  (write-set-pointer-field-definition struct-name field-name offset size))

(define (write-get-pointer-field-definition struct-name field-name offset size)
  (format #t "integer(c_intptr_t) function get_~a_~a (p) result(q)\n" struct-name field-name)
  (format #t "  type(~a), intent(in) :: p\n" struct-name)
  (format #t "  q = uint~d_to_uint~d (transfer (p%bv(~d:~d), 1_c_int~d_t))\n"
          (* 8 size) (* 8 intptr_t-size) (1+ offset) (+ offset size) (* 8 size))
  (format #t "end function get_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-set-pointer-field-definition struct-name field-name offset size)
  (format #t "subroutine set_~a_~a (p, v)\n" struct-name field-name)
  (format #t "  type(~a), intent(inout) :: p\n" struct-name)
  (format #t "  integer(c_intptr_t), intent(in) :: v\n")
  (format #t "  p%bv(~d:~d) = transfer (uint~d_to_uint~d (v), p%bv)\n"
          (1+ offset) (+ offset size) (* 8 intptr_t-size) (* 8 size))
  (format #t "end subroutine set_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-SCM-field-definitions struct-name field-name offset size)
  (write-get-SCM-field-definition struct-name field-name offset size)
  (write-set-SCM-field-definition struct-name field-name offset size))

(define (write-get-SCM-field-definition struct-name field-name offset size)
  (format #t "integer(c_intptr_t) function get_~a_~a (p) result(q)\n" struct-name field-name)
  (format #t "  type(~a), intent(in) :: p\n" struct-name)
  (format #t "  !\n")
  (format #t "  ! NOT YET IMPLEMENTED\n")
  (format #t "  !\n")
;;;;;;;  
;;;;;;; FIXME: Implement SCM type for Fortran.
;;;;;;;
  (format #t "end function get_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-set-SCM-field-definition struct-name field-name offset size)
  (format #t "subroutine set_~a_~a (p, v)\n" struct-name field-name)
  (format #t "  type(~a), intent(inout) :: p\n" struct-name)
  (format #t "  integer(c_intptr_t), intent(in) :: v\n")
  (format #t "  !\n")
  (format #t "  ! NOT YET IMPLEMENTED\n")
  (format #t "  !\n")
;;;;;;;  
;;;;;;; FIXME: Implement SCM type for Fortran.
;;;;;;;
  (format #t "end subroutine set_~a_~a\n" struct-name field-name)
  (format #t "\n"))

(define (write-unsigned-int-conversion from-size to-size)
  (format #t "integer(c_int~d_t) function uint~d_to_uint~d (i) result(j)\n"
          (* 8 to-size) (* 8 from-size) (* 8 to-size))
  (format #t "  integer(c_int~d_t), intent(in) :: i\n" (* 8 from-size))
  (cond
   [(= from-size to-size) (format #t "  j = i\n")]
   [(< from-size to-size)
    (format #t "  integer(c_int8_t), dimension(~d) :: temp\n" to-size)
    (format #t "  temp = transfer (0_c_int~d_t, temp)\n" (* 8 to-size) (* 8 to-size))
    (if (eq? (native-endianness) 'little)
        [format #t "  temp(:~d) = transfer (i, temp)\n" from-size]
        ;;
        ;; FIXME: This will need testing on a big-endian
        ;; architecture.
        [format #t "  temp(~d:) = transfer (i, temp)\n" (1+ (- to-size from-size))])
    (format #t "  j = transfer (temp, j)\n")]
   [else
    (format #t "  integer(c_int8_t), dimension(~d) :: temp\n" from-size)
    (format #t "  temp = transfer (i, temp)\n")
    (if (eq? (native-endianness) 'little)
        [format #t "  j = transfer (temp(:~d), j)\n" to-size]
        ;;
        ;; FIXME: This will need testing on a big-endian
        ;; architecture.
        [format #t "  j = transfer (temp(~d:), j)\n" (1+ (- from-size to-size))])
    ])
  (format #t "end function uint~d_to_uint~d\n" (* 8 from-size) (* 8 to-size))
  (format #t "\n"))

(let ((instructions (read-instructions-from-program-input)))
  (format #t "! Generated by ~s\n" (car (command-line)))
  (format #t "\n")
  (format #t "module sortsmill_fontforge_api\n")
  (format #t "use iso_c_binding\n")
  (format #t "implicit none\n")
  (format #t "\n")
  (format #t "interface operator(.cptr.)\n")
  (format #t "  module procedure c_intptr_t_to_c_ptr\n")
  (format #t "end interface operator(.cptr.)\n")
  (format #t "\n")
  (format #t "interface operator(.cfunptr.)\n")
  (format #t "  module procedure c_intptr_t_to_c_funptr\n")
  (format #t "end interface operator(.cfunptr.)\n")
  (format #t "\n")
  (format #t "interface operator(.cintptr.)\n")
  (format #t "  module procedure c_ptr_to_c_intptr_t\n")
  (format #t "  module procedure c_funptr_to_c_intptr_t\n")
  (format #t "end interface operator(.cintptr.)\n")
  (format #t "\n")
  (write-declarations instructions)
  (format #t "contains\n")
  (format #t "\n")
  (format #t "type(c_ptr) function c_intptr_t_to_c_ptr (p) result(q)\n")
  (format #t "  integer(c_intptr_t), value, intent(in) :: p\n")
  (format #t "  q = transfer (p, q)\n")
  (format #t "end function c_intptr_t_to_c_ptr\n")
  (format #t "\n")
  (format #t "type(c_funptr) function c_intptr_t_to_c_funptr (p) result(q)\n")
  (format #t "  integer(c_intptr_t), value, intent(in) :: p\n")
  (format #t "  q = transfer (p, q)\n")
  (format #t "end function c_intptr_t_to_c_funptr\n")
  (format #t "\n")
  (format #t "integer(c_intptr_t) function c_ptr_to_c_intptr_t (p) result(q)\n")
  (format #t "  type(c_ptr), value, intent(in) :: p\n")
  (format #t "  q = transfer (p, q)\n")
  (format #t "end function c_ptr_to_c_intptr_t\n")
  (format #t "\n")
  (format #t "integer(c_intptr_t) function c_funptr_to_c_intptr_t (p) result(q)\n")
  (format #t "  type(c_funptr), value, intent(in) :: p\n")
  (format #t "  q = transfer (p, q)\n")
  (format #t "end function c_funptr_to_c_intptr_t\n")
  (format #t "\n")
  (for-each
   (lambda (i)
     (for-each
      (lambda (j) (write-unsigned-int-conversion i j))
      '(1 2 4 8)))
   '(1 2 4 8))
  (write-definitions instructions)
  (format #t "end module sortsmill_fontforge_api\n"))
