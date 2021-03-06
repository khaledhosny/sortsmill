#!/bin/sh
# -*- mode: scheme; coding: utf-8 -*-
test -z "${GUILE}" && GUILE=guile
GUILE_AUTO_COMPILE=0 exec ${GUILE} ${GUILE_FLAGS} -s "${0}" ${1+"$@"}
!#

;; Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
;; This file is part of the Sorts Mill Tools.
;; 
;; Sorts Mill Tools is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Sorts Mill Tools is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(import (ff-internal generate-types)
        (sortsmill core)
        (rnrs)
        (system foreign)
        (ice-9 match)
        (ice-9 format))

(define (write-instruction instruction)
  (match instruction
    [('struct (? symbol? struct-name) (? integer? size))
     (format #t "cdef class ~a (object):\n" struct-name)
     (format #t "\n")
     (format #t "  # The base address of the C object, as an unsigned integer.\n")
     (format #t "  cdef readonly uintptr_t ptr\n")
     (format #t "\n")
     (format #t "  def __cinit__ (self):\n")
     (format #t "    self.ptr = <uintptr_t> NULL\n")
     (format #t "\n")
     (format #t "  def __init__ (self, uintptr_t ptr):\n")
     (format #t "    self.ptr = ptr\n")
     (format #t "\n")
     (format #t "  def __get_c_void_p (self):\n")
     (format #t "    return ctypes.c_void_p (self.ptr)\n")
     (format #t "\n")
     (format #t "  c_void_p = property (__get_c_void_p,\n")
     (format #t "                       doc = 'The base address of the C object as a ctypes.c_void_p.')\n")
     (format #t "\n")
     (format #t "  @classmethod\n")
     (format #t "  def malloc (cls):\n")
     (format #t "    \"\"\"Create a new C object, using malloc and filling with zeros.\"\"\"\n")
     (format #t "    return cls (<uintptr_t> xzalloc (~d))\n" size)
     (format #t "\n")
     (format #t "  @classmethod\n")
     (format #t "  def gc_malloc (cls):\n")
     (format #t "    \"\"\"Create a new C object, using the Boehm GC and filling with zeros.\"\"\"\n")
     (format #t "    return cls (<uintptr_t> x_gc_malloc (~d))\n" size)
     (format #t "\n")
     (format #t "  def __free__ (self):\n")
     (format #t "    \"\"\"Reclaim the C object, assuming it was allocated with malloc. (Dangerous.)\"\"\"\n")
     (format #t "    free (<void *> self.ptr)\n")
     (format #t "\n")
     (format #t "  def __gc_free__ (self):\n")
     (format #t "    \"\"\"Reclaim the C object, assuming it was allocated with the Boehm GC. (Dangerous.)\"\"\"\n")
     (format #t "    GC_free (<void *> self.ptr)\n")
     (format #t "\n")]

    [('sizeof (? symbol? struct-name) (? integer? size))
     (format #t "  def sizeof (self):\n")
     (format #t "    \"\"\"The size of the C object.\"\"\"\n")
     (format #t "    return ~d\n" size)
     (format #t "\n")]

    [('field (and (or 'struct 'array) field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (format #t "  def __get_~a_ptr (self):\n" field-name)
     (format #t "    return (self.ptr + ~d)\n" offset)
     (format #t "\n")
     (format #t "  _~a_ptr = property (__get_~a_ptr, " field-name field-name)
     (format #t "doc = \"The address of the `~a' field, as an unsigned integer.\")\n" field-name)
     (format #t "\n")]

    [('field (? symbol? field-type) (? symbol? struct-name)
             (? symbol? field-name) (? integer? offset) (? integer? size))
     (format #t "  def __get_~a (self):\n" field-name)
     (format #t "    return ~a (self.ptr + ~d)\n"
             (get-value-function field-type size)
             offset)
     (format #t "\n")
     (format #t "  def __set_~a (self, ~a v):\n"
             field-name (value-c-type field-type size))
     (format #t "    ~a (self.ptr + ~d, v)\n"
             (set-value-function field-type size)
             offset)
     (format #t "\n")
     (format #t "  _~a = property (__get_~a, __set_~a, " field-name field-name field-name)
     (format #t "doc = \"The `~a' field of the C object.\")\n" field-name)
     (format #t "\n")
     (format #t "  def __get_~a_ptr (self):\n" field-name)
     (format #t "    return (self.ptr + ~d)\n" offset)
     (format #t "\n")
     (format #t "  _~a_ptr = property (__get_~a_ptr, " field-name field-name)
     (format #t "doc = \"The address of the `~a' field, as an unsigned integer.\")\n" field-name)
     (format #t "\n")
     (format #t "  def __get_~a_c_void_p (self):\n" field-name)
     (format #t "    return ctypes.c_void_p (self.ptr + ~d)\n" offset)
     (format #t "\n")
     (format #t "  _~a_c_void_p = property (__get_~a_c_void_p, " field-name field-name)
     (format #t "doc = \"The address of the `~a' field, as a ctypes.c_void_p.\")\n" field-name)
     (format #t "\n")]

    [('field ((and (or '* 'struct 'array) field-type) (? symbol? pointer-type))
             (? symbol? struct-name) (? symbol? field-name) (? integer? offset)
             (? integer? size))
     (write-instruction (list 'field field-type struct-name field-name offset size))
     ;;
     ;; FIXME: Put dereferencing and array access here.
     ;;
     ]

    [('struct-> (? symbol? struct-name) . fields)
     (format #t "  def fields (self):\n")
     (format #t "    \"\"\"Return a dictionary of field values and struct/array-field addresses.\"\"\"\n")
     (format #t "    return \\\n")
     (format #t "      {\n")
     (for-each
      (lambda (flds)
        (match flds
          [[(? symbol? field-name)
            (? symbol? kind)
            (? (lambda (x) (or (not x) (field-type? x))) field-type)
            (? integer? offset)
            (? integer? size)]
           (format #t "        \"~a\" : " field-name)
           (match (ignore-subtype field-type)
             [(or 'struct 'array) (format #t "self.ptr + ~d" offset)]
             [else (format #t "~a (self.ptr + ~d)"
                           (get-value-function field-type size)
                           offset)] )
           (format #t ",\n" field-name)] ))
      fields)
     (format #t "      }\n")
     (format #t "\n")]
    
    [(instruction-symbol . _)
     (format (current-error-port) "Ignoring '~a\n" instruction-symbol)] ))

(define (field-type? ft)
  (match ft
    [(? symbol? _) #t]                 ; Examples: uint, bool, *, etc.
    [((or '* 'struct 'array)
      (? symbol? _)) #t]                ; Example: (* SplineChar)
    [else #f] ))

(define (ignore-subtype ft)
  (match ft
    [(? symbol? _) ft]                 ; Examples: uint, bool, *, etc.
    [('* (? symbol? _)) '*]            ; Example: (* SplineChar)
    [('struct (? symbol? _)) 'struct]  ; Example: (struct DBounds)
    [('array (? symbol? _)) 'array]    ; Example: FIXME FIXME
    [else (assert #f)] ))

(define (value-c-type field-type size)
  (match (cons (ignore-subtype field-type) size)
    [('int . 1) "int8_t"]
    [('int . 2) "int16_t"]
    [('int . 4) "int32_t"]
    [('int . 8) "int64_t"]
    [('uint . 1) "uint8_t"]
    [('uint . 2) "uint16_t"]
    [('uint . 4) "uint32_t"]
    [('uint . 8) "uint64_t"]
    [('bool . _) "bint"]
    [('float . n) (symbol->string (float_t-at-size n))]
    [('* . _) "uintptr_t"]
    [('SCM . _) "object"]
    [('struct . _) (error "NOT YET IMPLEMENTED")]
    [('array . _) (error "NOT YET IMPLEMENTED")] ))

(define (get-value-function field-type size)
  (match (cons (ignore-subtype field-type) size)
    [('int . 1) "__get_int8"]
    [('int . 2) "__get_int16"]
    [('int . 4) "__get_int32"]
    [('int . 8) "__get_int64"]
    [('uint . 1) "__get_uint8"]
    [('uint . 2) "__get_uint16"]
    [('uint . 4) "__get_uint32"]
    [('uint . 8) "__get_uint64"]
    [('bool . 1) "__get_bool8"]
    [('bool . 2) "__get_bool16"]
    [('bool . 4) "__get_bool32"]
    [('bool . 8) "__get_bool64"]
    [('float . n) (cond
                   ((= n (sizeof float)) "__get_float")
                   ((= n (sizeof double)) "__get_double"))]
    [('* . 1) "__get_ptr8"]
    [('* . 2) "__get_ptr16"]
    [('* . 4) "__get_ptr32"]
    [('* . 8) "__get_ptr64"]
    [('SCM . _) "__get_SCM"]
    [('struct . _) (error "NOT YET IMPLEMENTED")]
    [('array . _) (error "NOT YET IMPLEMENTED")] ))

(define (set-value-function field-type size)
  (match (cons (ignore-subtype field-type) size)
    [('int . 1) "__set_int8"]
    [('int . 2) "__set_int16"]
    [('int . 4) "__set_int32"]
    [('int . 8) "__set_int64"]
    [('uint . 1) "__set_uint8"]
    [('uint . 2) "__set_uint16"]
    [('uint . 4) "__set_uint32"]
    [('uint . 8) "__set_uint64"]
    [('bool . 1) "__set_bool8"]
    [('bool . 2) "__set_bool16"]
    [('bool . 4) "__set_bool32"]
    [('bool . 8) "__set_bool64"]
    [('float . n) (cond
                   ((= n (sizeof float)) "__set_float")
                   ((= n (sizeof double)) "__set_double"))]
    [('* . 1) "__set_ptr8"]
    [('* . 2) "__set_ptr16"]
    [('* . 4) "__set_ptr32"]
    [('* . 8) "__set_ptr64"]
    [('SCM . _) "__set_SCM"]
    [('struct . _) (error "NOT YET IMPLEMENTED")]
    [('array . _) (error "NOT YET IMPLEMENTED")] ))

(format #t "# Generated by ~s\n" (car (command-line)))
(format #t "\n")
(format #t "import ctypes\n")
(format #t "import sortsmill.pyguile\n")
(format #t "\n")
(format #t "from libc.stdint cimport uintptr_t\n")
(format #t "from libc.stdint cimport int8_t, int16_t, int32_t, int64_t\n")
(format #t "from libc.stdint cimport uint8_t, uint16_t, uint32_t, uint64_t\n")
(format #t "cimport sortsmill.cython.guile\n")
(format #t "\n")
(format #t "cdef extern from 'sortsmill/core.h':\n")
(format #t "  void *x_gc_malloc (size_t sz)\n")
(format #t "  void GC_free (void *)\n")
(format #t "\n")
(format #t "cdef extern from 'xalloc.h':\n")
(format #t "  void *xzalloc (size_t sz)\n")
(format #t "  void free (void *)\n")
(format #t "\n")
(format #t "cdef inline int8_t __get_int8 (uintptr_t p):\n")
(format #t "  return (<int8_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline int16_t __get_int16 (uintptr_t p):\n")
(format #t "  return (<int16_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline int32_t __get_int32 (uintptr_t p):\n")
(format #t "  return (<int32_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline int64_t __get_int64 (uintptr_t p):\n")
(format #t "  return (<int64_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uint8_t __get_uint8 (uintptr_t p):\n")
(format #t "  return (<uint8_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uint16_t __get_uint16 (uintptr_t p):\n")
(format #t "  return (<uint16_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uint32_t __get_uint32 (uintptr_t p):\n")
(format #t "  return (<uint32_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uint64_t __get_uint64 (uintptr_t p):\n")
(format #t "  return (<uint64_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline bint __get_bool8 (uintptr_t p):\n")
(format #t "  return <bint> ((<uint8_t *> p)[0] != 0)\n")
(format #t "\n")
(format #t "cdef inline bint __get_bool16 (uintptr_t p):\n")
(format #t "  return <bint> ((<uint16_t *> p)[0] != 0)\n")
(format #t "\n")
(format #t "cdef inline bint __get_bool32 (uintptr_t p):\n")
(format #t "  return <bint> ((<uint32_t *> p)[0] != 0)\n")
(format #t "\n")
(format #t "cdef inline bint __get_bool64 (uintptr_t p):\n")
(format #t "  return <bint> ((<uint64_t *> p)[0] != 0)\n")
(format #t "\n")
(format #t "cdef inline float __get_float (uintptr_t p):\n")
(format #t "  return (<float *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline double __get_double (uintptr_t p):\n")
(format #t "  return (<double *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uintptr_t __get_ptr8 (uintptr_t p):\n")
(format #t "  return <uintptr_t> (<uint8_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uintptr_t __get_ptr16 (uintptr_t p):\n")
(format #t "  return <uintptr_t> (<uint16_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uintptr_t __get_ptr32 (uintptr_t p):\n")
(format #t "  return <uintptr_t> (<uint32_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline uintptr_t __get_ptr64 (uintptr_t p):\n")
(format #t "  return <uintptr_t> (<uint64_t *> p)[0]\n")
(format #t "\n")
(format #t "cdef inline object __get_SCM (uintptr_t p):\n")
(format #t "  return sortsmill.pyguile.pyguile (<uintptr_t> (<sortsmill.cython.guile.SCM *> p)[0])\n")
(format #t "\n")
(format #t "cdef inline __set_int8 (uintptr_t p, int8_t v):\n")
(format #t "  (<int8_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_int16 (uintptr_t p, int16_t v):\n")
(format #t "  (<int16_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_int32 (uintptr_t p, int32_t v):\n")
(format #t "  (<int32_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_int64 (uintptr_t p, int64_t v):\n")
(format #t "  (<int64_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_uint8 (uintptr_t p, uint8_t v):\n")
(format #t "  (<uint8_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_uint16 (uintptr_t p, uint16_t v):\n")
(format #t "  (<uint16_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_uint32 (uintptr_t p, uint32_t v):\n")
(format #t "  (<uint32_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_uint64 (uintptr_t p, uint64_t v):\n")
(format #t "  (<uint64_t *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_bool8 (uintptr_t p, bint v):\n")
(format #t "  (<uint8_t *> p)[0] = <uint8_t> (v != 0)\n")
(format #t "\n")
(format #t "cdef inline __set_bool16 (uintptr_t p, bint v):\n")
(format #t "  (<uint16_t *> p)[0] = <uint16_t> (v != 0)\n")
(format #t "\n")
(format #t "cdef inline __set_bool32 (uintptr_t p, bint v):\n")
(format #t "  (<uint32_t *> p)[0] = <uint32_t> (v != 0)\n")
(format #t "\n")
(format #t "cdef inline __set_bool64 (uintptr_t p, bint v):\n")
(format #t "  (<uint64_t *> p)[0] = <uint64_t> (v != 0)\n")
(format #t "\n")
(format #t "cdef inline __set_float (uintptr_t p, float v):\n")
(format #t "  (<float *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_double (uintptr_t p, double v):\n")
(format #t "  (<double *> p)[0] = v\n")
(format #t "\n")
(format #t "cdef inline __set_ptr8 (uintptr_t p, uintptr_t v):\n")
(format #t "  (<uint8_t *> p)[0] = <uint8_t> v\n")
(format #t "\n")
(format #t "cdef inline __set_ptr16 (uintptr_t p, uintptr_t v):\n")
(format #t "  (<uint16_t *> p)[0] = <uint16_t> v\n")
(format #t "\n")
(format #t "cdef inline __set_ptr32 (uintptr_t p, uintptr_t v):\n")
(format #t "  (<uint32_t *> p)[0] = <uint32_t> v\n")
(format #t "\n")
(format #t "cdef inline __set_ptr64 (uintptr_t p, uintptr_t v):\n")
(format #t "  (<uint64_t *> p)[0] = <uint64_t> v\n")
(format #t "\n")
(format #t "cdef inline __set_SCM (uintptr_t p, object v):\n")
(format #t "  (<sortsmill.cython.guile.SCM *> p)[0] = sortsmill.cython.guile.scm_from_pyguile_object (v)\n")
(format #t "\n")
(let ((instructions (read-instructions-from-program-input)))
  (for-each write-instruction instructions))
