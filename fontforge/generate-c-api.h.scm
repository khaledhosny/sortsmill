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
        (sortsmillff machine)
        (rnrs)
        (ice-9 match)
        (ice-9 format))

(define (write-instruction instruction)
  (match instruction
    [('struct (? symbol? struct-name) (? integer? size))
     (format #t "inline void *malloc_ff_~a (void);\n" struct-name)
     (format #t "inline void *malloc_ff_~a (void) { return xdie_on_null (calloc (1, ~d)); }\n"
             struct-name size)
     (format #t "\n")
     (format #t "inline void free_ff_~a (void *);\n" struct-name)
     (format #t "inline void free_ff_~a (void *p) { free (p); }\n" struct-name)
     (format #t "\n")
     (format #t "inline void *gc_malloc_ff_~a (void);\n" struct-name)
     (format #t "inline void *gc_malloc_ff_~a (void) { return x_gc_malloc (~d); }\n"
             struct-name size)
     (format #t "\n")
     (format #t "inline void gc_free_ff_~a (void *);\n" struct-name)
     (format #t "inline void gc_free_ff_~a (void *p) { GC_FREE (p); }\n" struct-name)
     (format #t "\n")]

    [('sizeof (? symbol? struct-name) (? integer? size))
     (format #t "inline size_t sizeof_ff_~a (void);\n" struct-name)
     (format #t "inline size_t sizeof_ff_~a (void) { return ~d; };\n" struct-name size)
     (format #t "\n")]

    [('field (and (or 'struct 'array) field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (format #t "inline void *ptr_ff_~a_~a (void *);\n"
             struct-name field-name)
     (format #t "inline void *ptr_ff_~a_~a (void *p) { return (void *) &((char *) p)[~d]; }\n"
             struct-name field-name offset)
     (format #t "\n")]

    [('field (? symbol? field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (format #t "inline ~a get_ff_~a_~a (void *);\n"
             (value-c-type field-type size) struct-name field-name)
     (format #t "inline ~a get_ff_~a_~a (void *p) { return ~a; }\n"
             (value-c-type field-type size) struct-name field-name
             (get-value-expression field-type offset size "p"))
     (format #t "\n")
     (format #t "inline void set_ff_~a_~a (void *, ~a);\n"
             struct-name field-name (value-c-type field-type size))
     (format #t "inline void set_ff_~a_~a (void *p, ~a v) { ~a; }\n"
             struct-name field-name (value-c-type field-type size)
             (set-value-expression field-type offset size "p" "v"))
     (format #t "\n")
     (format #t "inline void *ptr_ff_~a_~a (void *);\n"
             struct-name field-name)
     (format #t "inline void *ptr_ff_~a_~a (void *p) { return (void *) &((char *) p)[~d]; }\n"
             struct-name field-name offset)
     (format #t "\n")]

    [('field ((and (or '* 'struct 'array) field-type) (? symbol? field-subtype))
             (? symbol? struct-name) (? symbol? field-name) (? integer? offset) (? integer? size))
     (write-instruction (list 'field field-type struct-name field-name offset size))
     ;;
     ;; FIXME: Dereferencing and array procedures go here.
     ;;
     ]

    [('struct-> . _) *unspecified*]   ; Ignore 'struct-> silently.

    [(instruction-symbol . _)
     (format (current-error-port) "Ignoring '~a\n" instruction-symbol)] ))

(define (value-c-type field-type size)
  (match (cons field-type size)
    [('int . 1) "int"]
    [('int . 2) "int"]
    [('int . 4) "int"]
    [('int . 8) "int64_t"]
    [('uint . 1) "unsigned int"]
    [('uint . 2) "unsigned int"]
    [('uint . 4) "unsigned int"]
    [('uint . 8) "uint64_t"]
    [('bool . _) "bool"]
    [('float . _) "double"]
    [('* . _) "void *"]
    [('struct . _) (error "NOT YET IMPLEMENTED")]
    [('array . _) (error "NOT YET IMPLEMENTED")] ))

(define (get-value-expression field-type offset size pointer-expression)
  (let ((address (format #f "((void *) &((char *) (~a))[~d])"
                         pointer-expression offset)))
    (match (cons field-type size)
      [('int . n) (format #f "(*(~a *) ~a)" (c-int-type n) address)]
      [('uint . n) (format #f "(*(~a *) ~a)" (c-uint-type n) address)]
      [('bool . n) (format #f "((bool) (*(~a *) ~a != 0))" (c-uint-type n) address)]
      [('float . n) (format #f "(*(~a *) ~a)" (c-float-type n) address)]
      [('* . n) (format #f "((void *) *(~a *) ~a)" (c-uint-type n) address)]
      [('struct . _) (error "NOT YET IMPLEMENTED")]
      [('array . _) (error "NOT YET IMPLEMENTED")] )))

(define (set-value-expression field-type offset size pointer-expression
                              value-expression)
  (let ((address (format #f "((void *) &((char *) (~a))[~d])"
                         pointer-expression offset)))
    (match (cons field-type size)
      [('int . n) (format #f "(*(~a *) ~a = (~a))"
                          (c-int-type n) address value-expression)]
      [('uint . n) (format #f "(*(~a *) ~a = (~a))"
                           (c-uint-type n) address value-expression)]
      [('bool . n) (format #f "(*(~a *) ~a = ((~a) != 0))"
                           (c-uint-type n) address value-expression)]
      [('float . n) (format #f "(*(~a *) ~a = (~a))"
                            (c-float-type n)  address value-expression)]
      [('* . n) (format #f "(*(~a *) ~a = (uint8_t) (uintptr_t) (~a))"
                        (c-uint-type n) address value-expression)]
      [('struct . _) (error "NOT YET IMPLEMENTED")]
      [('array . _) (error "NOT YET IMPLEMENTED")] )))

(let ((instructions (read-instructions-from-program-input)))
  (format #t "/* Generated by ~s */\n" (car (command-line)))
  (format #t "\n")
  (format #t "#ifndef SORTSMILLFF_INTERNAL_TYPES_H\n")
  (format #t "#define SORTSMILLFF_INTERNAL_TYPES_H\n")
  (format #t "\n")
  (format #t "#include <stdlib.h>\n")
  (format #t "#include <stdint.h>\n")
  (format #t "#include <stdbool.h>\n")
  (format #t "#include <sortsmillff/xgc.h>\n")
  (format #t "#include <sortsmillff/xdie_on_null.h>\n")
  (format #t "\n")
  (format #t "#ifdef __cplusplus\n")
  (format #t "extern \"C\" {\n")
  (format #t "#endif\n")
  (format #t "\n")
  (for-each write-instruction instructions)
  (format #t "#ifdef __cplusplus\n")
  (format #t "}\n")
  (format #t "#endif\n")
  (format #t "\n")
  (format #t "#endif /* SORTSMILLFF_INTERNAL_TYPES_H */\n"))
