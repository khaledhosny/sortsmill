#! @GUILE@ \           -*- mode: bee; coding: utf-8 -*-
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
   (ff-internal generate-types)
   (ice-9 match)
   (ice-9 format)
   )

(define (write-instruction instruction)
   (match instruction
      (('struct (? string? struct-name) (? integer? size))
       (format #t "VISIBLE void *malloc_ff_~a (void);\n" struct-name)
       (format #t "VISIBLE void free_ff_~a (void *);\n" struct-name)
       (format #t "VISIBLE void *gc_malloc_ff_~a (void);\n" struct-name)
       (format #t "VISIBLE void gc_free_ff_~a (void *);\n" struct-name)
       (format #t "\n")
       )
      (('sizeof (? string? struct-name) (? integer? size))
       (format #t "VISIBLE size_t sizeof_ff_~a (void);\n" struct-name)
       (format #t "\n")
       )
      (('field (and (or 'struct 'array) field-type) (? string? struct-name)
          (? string? field-name) (? integer? offset) (? integer? size))
       (format #t "VISIBLE void *ptr_ff_~a_~a (void *);\n"
          struct-name field-name)
       (format #t "\n")       
       )
      (('field (? symbol? field-type) (? string? struct-name)
          (? string? field-name) (? integer? offset) (? integer? size))
       (format #t "VISIBLE ~a get_ff_~a_~a (void *);\n"
          (value-c-type field-type size) struct-name field-name)
       (format #t "VISIBLE void set_ff_~a_~a (void *, ~a);\n"
          struct-name field-name (value-c-type field-type size))
       (format #t "VISIBLE void *ptr_ff_~a_~a (void *);\n"
          struct-name field-name)
       (format #t "\n")
       )
      (('field ((and (or '* 'struct 'array ) field-type) (? symbol? pointer-type))
          (? string? struct-name) (? string? field-name) (? integer? offset)
          (? integer? size))
       (write-instruction (list 'field field-type struct-name field-name offset size))
       ;;
       ;; FIXME: Dereferencing and array procedures go here.
       ;;
       )
      
      ((instruction-symbol . _)
       (format (current-error-port) "Ignoring '~a\n" instruction-symbol))
      ))

(define (value-c-type field-type size)
   (match (cons field-type size)
      (('int . 1) "int")
      (('int . 2) "int")
      (('int . 4) "int")
      (('int . 8) "int64_t")
      (('uint . 1) "unsigned int")
      (('uint . 2) "unsigned int")
      (('uint . 4) "unsigned int")
      (('uint . 8) "uint64_t")
      (('bool . _) "bool")
      (('float . _) "double")
      (('* . _) "void *")
      (('struct . _) (error "NOT YET IMPLEMENTED"))
      (('array . _) (error "NOT YET IMPLEMENTED"))
      ))

(let ((instructions (read-instructions-from-program-input)))
   (format #t "/* Generated by ~s */\n" (car (command-line)))
   (format #t "\n")
   (format #t "#include <config.h>\n")
   (format #t "#include <sortsmillff/fontforge_api.h>\n")
   (format #t "\n")
   (format #t "/* Extern-addressable API functions. */\n")
   (format #t "\n")
   (for-each write-instruction instructions)
   )