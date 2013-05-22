# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

#--------------------------------------------------------------------------

include 'sortsmill/cython/config.pxi'

from cpython.ref cimport PyObject
from libc.stdint cimport int8_t, int16_t, int32_t, int64_t, intmax_t, intptr_t
from libc.stdint cimport uint8_t, uint16_t, uint32_t, uint64_t, uintmax_t, uintptr_t
from sortsmill.cython.const_pointers cimport const_char_ptr

cdef extern from 'stdbool.h':
  pass
from libcpp cimport bool

cdef extern from 'libguile.h':

  # FIXME: THIS ASSUMES SCM_DEBUG_TYPING_STRICTNESS == 1
  # FIXME: Either provide support for the other levels, or
  # FIXME: force this level more carefully.
  cdef struct scm_unused_struct:
    pass
  ctypedef scm_unused_struct *SCM

  int scm_is_true (SCM)
  int scm_is_false (SCM)
  int scm_is_eq (SCM, SCM)

  char scm_to_char (SCM x)
  signed char scm_to_schar (SCM x)
  unsigned char scm_to_uchar (SCM x)
  short scm_to_short (SCM x)
  unsigned short scm_to_ushort (SCM x)
  int scm_to_int (SCM x)
  unsigned int scm_to_uint (SCM x)
  long scm_to_long (SCM x)
  unsigned long scm_to_ulong (SCM x)
  long long scm_to_long_long (SCM x)
  unsigned long long scm_to_ulong_long (SCM x)
  size_t scm_to_size_t (SCM x)
  ssize_t scm_to_ssize_t (SCM x)
  int8_t scm_to_int8 (SCM x)
  uint8_t scm_to_uint8 (SCM x)
  int16_t scm_to_int16 (SCM x)
  uint16_t scm_to_uint16 (SCM x)
  int32_t scm_to_int32 (SCM x)
  uint32_t scm_to_uint32 (SCM x)
  int64_t scm_to_int64 (SCM x)
  uint64_t scm_to_uint64 (SCM x)
  intmax_t scm_to_intmax (SCM x)
  uintmax_t scm_to_uintmax (SCM x)

  SCM scm_from_char (char x)
  SCM scm_from_schar (signed char x)
  SCM scm_from_uchar (unsigned char x)
  SCM scm_from_short (short x)
  SCM scm_from_ushort (unsigned short x)
  SCM scm_from_int (int x)
  SCM scm_from_uint (unsigned int x)
  SCM scm_from_long (long x)
  SCM scm_from_ulong (unsigned long x)
  SCM scm_from_long_long (long long x)
  SCM scm_from_ulong_long (unsigned long long x)
  SCM scm_from_size_t (size_t x)
  SCM scm_from_ssize_t (ssize_t x)
  SCM scm_from_int8 (int8_t x)
  SCM scm_from_uint8 (uint8_t x)
  SCM scm_from_int16 (int16_t x)
  SCM scm_from_uint16 (uint16_t x)
  SCM scm_from_int32 (int32_t x)
  SCM scm_from_uint32 (uint32_t x)
  SCM scm_from_int64 (int64_t x)
  SCM scm_from_uint64 (uint64_t x)
  SCM scm_from_intmax (intmax_t x)
  SCM scm_from_uintmax (uintmax_t x)

  SCM scm_from_bool (int val)
  int scm_to_bool (SCM x)

  SCM scm_from_latin1_string (const_char_ptr str)
  SCM scm_from_utf8_string (const_char_ptr str)
  SCM scm_from_latin1_stringn (const_char_ptr str, size_t len)
  SCM scm_from_utf8_stringn (const_char_ptr str, size_t len)
  char *scm_to_latin1_stringn (SCM str, size_t *lenp)
  char *scm_to_utf8_stringn (SCM str, size_t *lenp)

  SCM scm_symbol_hash (SCM symbol)
  SCM scm_symbol_p (SCM obj)
  int scm_is_symbol (SCM val)
  SCM scm_symbol_to_string (SCM s)
  SCM scm_string_to_symbol (SCM string)
  SCM scm_string_ci_to_symbol (SCM str)
  SCM scm_from_latin1_symbol (const_char_ptr name)
  SCM scm_from_utf8_symbol (const_char_ptr name)
  SCM scm_from_locale_symbol (const_char_ptr name)
  SCM scm_from_locale_symboln (const_char_ptr name, size_t len)
  SCM scm_take_locale_symbol (char *str)
  SCM scm_take_locale_symboln (char *str, size_t len)
  size_t scm_c_symbol_length (SCM sym)
  SCM scm_gensym (SCM prefix)

  SCM scm_keyword_p (SCM obj)
  SCM scm_keyword_to_symbol (SCM keyword)
  SCM scm_symbol_to_keyword (SCM symbol)
  int scm_is_keyword (SCM obj)
  SCM scm_from_locale_keyword (const_char_ptr name)
  SCM scm_from_locale_keywordn (const_char_ptr name, size_t len)
  SCM scm_from_latin1_keyword (const_char_ptr name)
  SCM scm_from_utf8_keyword (const_char_ptr name)

  SCM scm_cons (SCM x, SCM y)
  SCM scm_pair_p (SCM x)
  int scm_is_pair (SCM x)
  SCM scm_car (SCM pair)
  SCM scm_cdr (SCM pair)
  SCM scm_cddr (SCM pair)
  SCM scm_cdar (SCM pair)
  SCM scm_cadr (SCM pair)
  SCM scm_caar (SCM pair)
  SCM scm_cdddr (SCM pair)
  SCM scm_cddar (SCM pair)
  SCM scm_cdadr (SCM pair)
  SCM scm_cdaar (SCM pair)
  SCM scm_caddr (SCM pair)
  SCM scm_cadar (SCM pair)
  SCM scm_caadr (SCM pair)
  SCM scm_caaar (SCM pair)
  SCM scm_cddddr (SCM pair)
  SCM scm_cdddar (SCM pair)
  SCM scm_cddadr (SCM pair)
  SCM scm_cddaar (SCM pair)
  SCM scm_cdaddr (SCM pair)
  SCM scm_cdadar (SCM pair)
  SCM scm_cdaadr (SCM pair)
  SCM scm_cdaaar (SCM pair)
  SCM scm_cadddr (SCM pair)
  SCM scm_caddar (SCM pair)
  SCM scm_cadadr (SCM pair)
  SCM scm_cadaar (SCM pair)
  SCM scm_caaddr (SCM pair)
  SCM scm_caadar (SCM pair)
  SCM scm_caaadr (SCM pair)
  SCM scm_caaaar (SCM pair)
  SCM scm_set_car_x (SCM pair, SCM value)
  SCM scm_set_cdr_x (SCM pair, SCM value)

  SCM scm_list_p (SCM x)
  SCM scm_null_p (SCM x)
  int scm_is_null (SCM x)
  SCM scm_list_1 (SCM elem1)
  SCM scm_list_2 (SCM elem1, SCM elem2)
  SCM scm_list_3 (SCM elem1, SCM elem2, SCM elem3)
  SCM scm_list_4 (SCM elem1, SCM elem2, SCM elem3, SCM elem4)
  SCM scm_list_5 (SCM elem1, SCM elem2, SCM elem3, SCM elem4, SCM elem5)
  SCM scm_list_n (SCM elem1, ...)
  SCM scm_list_copy (SCM lst)
  SCM scm_length (SCM lst)
  SCM scm_last_pair (SCM lst)
  SCM scm_list_ref (SCM lst, SCM k)
  SCM scm_list_tail (SCM lst, SCM k)
  SCM scm_list_head (SCM lst, SCM k)
  SCM scm_append (SCM lstlst)
  SCM scm_append_x (SCM lstlst)
  SCM scm_reverse (SCM lst)
  SCM scm_reverse_x (SCM lst, SCM newtail)
  SCM scm_list_set_x (SCM lst, SCM k, SCM val)
  SCM scm_list_cdr_set_x (SCM lst, SCM k, SCM val)
  SCM scm_delq (SCM item, SCM lst)
  SCM scm_delete (SCM item, SCM lst)
  SCM scm_delq_x (SCM item, SCM lst)
  SCM scm_delv_x (SCM item, SCM lst)
  SCM scm_delete_x (SCM item, SCM lst)
  SCM scm_delq1_x (SCM item, SCM lst)
  SCM scm_delv1_x (SCM item, SCM lst)
  SCM scm_delete1_x (SCM item, SCM lst)
  SCM scm_memq (SCM x, SCM lst)
  SCM scm_memv (SCM x, SCM lst)
  SCM scm_member (SCM x, SCM lst)
  SCM scm_map (SCM proc, SCM arg1, SCM args)

  ctypedef SCM (*scm_t_catch_body) (void *data)
  ctypedef SCM (*scm_t_catch_handler) (void *data, SCM key, SCM args)
  SCM scm_catch_with_pre_unwind_handler (SCM key, SCM thunk, SCM handler, SCM pre_unwind_handler)
  SCM scm_catch (SCM key, SCM thunk, SCM handler)
  SCM scm_c_catch (SCM tag, scm_t_catch_body body, void *body_data, scm_t_catch_handler handler, \
                     void *handler_data, scm_t_catch_handler pre_unwind_handler, void *pre_unwind_handler_data)
  SCM scm_internal_catch (SCM tag, scm_t_catch_body body, void *body_data, \
                            scm_t_catch_handler handler, void *handler_data)
  SCM scm_with_throw_handler (SCM key, SCM thunk, SCM handler)
  SCM scm_c_with_throw_handler (SCM tag, scm_t_catch_body body, void *body_data, \
                                  scm_t_catch_handler handler, void *handler_data, int lazy_catch_p)
  SCM scm_throw (SCM key, SCM args)

  SCM scm_error_scm (SCM key, SCM subr, SCM message, SCM args, SCM data)
  SCM scm_strerror (SCM err)
  SCM scm_display_error (SCM frame, SCM port, SCM subr, SCM message, SCM args, SCM rest)
  SCM scm_error (SCM key, char *subr, char *message, SCM args, SCM rest)
  void scm_syserror (char *subr)
  void scm_syserror_msg (char *subr, char *message, SCM args)
  void scm_num_overflow (char *subr)
  void scm_out_of_range (char *subr, SCM bad_value)
  void scm_wrong_num_args (SCM proc)
  void scm_wrong_type_arg (char *subr, int argnum, SCM bad_value)
  void scm_wrong_type_arg_msg (char *subr, int argnum, SCM bad_value, const_char_ptr expected)
  void scm_memory_error (char *subr)
  void scm_misc_error (const_char_ptr subr, const_char_ptr message, SCM args)

  ctypedef enum scm_t_dynwind_flags:
    SCM_F_DYNWIND_REWINDABLE = __SCM_F_DYNWIND_REWINDABLE
  ctypedef enum scm_t_wind_flags:
    SCM_F_WIND_EXPLICITLY = __SCM_F_WIND_EXPLICITLY
  SCM scm_dynamic_wind (SCM in_guard, SCM thunk, SCM out_guard)
  SCM scm_dynwind_begin (scm_t_dynwind_flags flags)
  void scm_dynwind_end ()
  void scm_dynwind_unwind_handler (void (*func)(void *), void *data, scm_t_wind_flags flags)
  void scm_dynwind_unwind_handler_with_scm (void (*func)(SCM), SCM data, scm_t_wind_flags flags)
  void scm_dynwind_rewind_handler (void (*func)(void *), void *data, scm_t_wind_flags flags)
  void scm_dynwind_rewind_handler_with_scm (void (*func)(SCM), SCM data, scm_t_wind_flags flags)
  void scm_dynwind_free (void *mem)

  SCM scm_with_continuation_barrier (SCM proc)
  void *scm_c_with_continuation_barrier (void *(*func) (void *), void *data)

  SCM scm_object_address (SCM obj)
  SCM scm_gc_stats ()
  SCM scm_gc_live_object_stats ()
  SCM scm_gc ()
  #void scm_gc_for_alloc (struct scm_t_cell_type_statistics *freelist)
  #SCM scm_gc_for_newcell (struct scm_t_cell_type_statistics *master, SCM *freelist)
  void scm_i_gc (const_char_ptr what)
  void scm_gc_mark (SCM p)
  void scm_gc_mark_dependencies (SCM p)
  #void scm_mark_locations (SCM_STACKITEM x[], unsigned long n)
  int scm_in_heap_p (SCM value)
  void scm_gc_sweep ()
  void *scm_malloc (size_t size)
  void *scm_calloc (size_t size)
  void *scm_realloc (void *mem, size_t size)
  char *scm_strdup (const_char_ptr str)
  char *scm_strndup (const_char_ptr str, size_t n)
  void scm_gc_register_collectable_memory (void *mem, size_t size,
                                           const_char_ptr what)
  void scm_gc_unregister_collectable_memory (void *mem, size_t size,
                                             const_char_ptr what)
  void *scm_gc_calloc (size_t size, const_char_ptr what)
  void *scm_gc_malloc (size_t size, const_char_ptr what)
  void *scm_gc_realloc (void *mem, size_t old_size, 
                        size_t new_size, const_char_ptr what)
  void scm_gc_free (void *mem, size_t size, const_char_ptr what)
  char *scm_gc_strdup (const_char_ptr str, const_char_ptr what)
  char *scm_gc_strndup (const_char_ptr str, size_t n, const_char_ptr what)
  void scm_remember_upto_here_1 (SCM obj)
  void scm_remember_upto_here_2 (SCM obj1, SCM obj2)
  void scm_remember_upto_here (SCM obj1, ...)
  SCM scm_return_first (SCM elt, ...)
  int scm_return_first_int (int x, ...)
  SCM scm_permanent_object (SCM obj)
  SCM scm_gc_protect_object (SCM obj)
  SCM scm_gc_unprotect_object (SCM obj)
  void scm_gc_register_root (SCM *p)
  void scm_gc_unregister_root (SCM *p)
  void scm_gc_register_roots (SCM *b, unsigned long n)
  void scm_gc_unregister_roots (SCM *b, unsigned long n)
  void scm_storage_prehistory ()
  int scm_init_storage ()
  void *scm_get_stack_base ()
  void scm_init_gc ()
  SCM scm_gc_stats ()
  SCM scm_gc_live_object_stats ()
  void scm_gc_mark (SCM x)

  SCM scm_dynamic_pointer (SCM name, SCM dobj)
  SCM scm_pointer_address (SCM pointer)
  SCM scm_from_pointer (void *ptr, void (*finalizer) (void*))
  void* scm_to_pointer (SCM obj)
  
  SCM scm_read_hash_extend (SCM chr, SCM proc)

  SCM scm_close_port (SCM port)
  SCM scm_read (SCM port)
  SCM scm_write (SCM value, SCM port)
  SCM scm_call_with_output_string (SCM proc)
  SCM scm_call_with_input_string (SCM string, SCM proc)
  SCM scm_open_input_string (SCM str)
  SCM scm_open_output_string ()
  SCM scm_get_output_string (SCM port)

  SCM scm_eval (SCM exp, SCM module_or_state)
  SCM scm_interaction_environment ()
  SCM scm_eval_string (SCM string)
  SCM scm_eval_string_in_module (SCM string, SCM module)
  SCM scm_c_eval_string (const_char_ptr string)

  SCM scm_apply_0 (SCM proc, SCM arglst)
  SCM scm_apply_1 (SCM proc, SCM arg1, SCM arglst)
  SCM scm_apply_2 (SCM proc, SCM arg1, SCM arg2, SCM arglst)
  SCM scm_apply_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arglst)
  SCM scm_apply (SCM proc, SCM arg, SCM rest)

  SCM scm_call_0 (SCM proc)
  SCM scm_call_1 (SCM proc, SCM arg1)
  SCM scm_call_2 (SCM proc, SCM arg1, SCM arg2)
  SCM scm_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
  SCM scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
  SCM scm_call_5 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
  SCM scm_call_6 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
  SCM scm_call_7 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7)
  SCM scm_call_8 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8)
  SCM scm_call_9 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8, SCM arg9)
  SCM scm_call (SCM proc, ...)
  SCM scm_call_n (SCM proc, SCM *argv, size_t nargs)

  SCM scm_nconc2last (SCM lst)

  SCM scm_primitive_eval (SCM exp)

  SCM scm_c_call_with_current_module (SCM module, SCM (*func) (void *), void *data)
  SCM scm_public_variable (SCM module_name, SCM name)
  SCM scm_c_public_variable (const_char_ptr module_name, const_char_ptr name)
  SCM scm_private_variable (SCM module_name, SCM name)
  SCM scm_c_private_variable (const_char_ptr module_name, const_char_ptr name)
  SCM scm_public_lookup (SCM module_name, SCM name)
  SCM scm_c_public_lookup (const_char_ptr module_name, const_char_ptr name)
  SCM scm_private_lookup (SCM module_name, SCM name)
  SCM scm_c_private_lookup (const_char_ptr module_name, const_char_ptr name)
  SCM scm_public_ref (SCM module_name, SCM name)
  SCM scm_c_public_ref (const_char_ptr module_name, const_char_ptr name)
  SCM scm_private_ref (SCM module_name, SCM name)
  SCM scm_c_private_ref (const_char_ptr module_name, const_char_ptr name)
  SCM scm_c_lookup (const_char_ptr name)
  SCM scm_lookup (SCM name)
  SCM scm_c_module_lookup (SCM module, const_char_ptr name)
  SCM scm_module_lookup (SCM module, SCM name)
  SCM scm_module_variable (SCM module, SCM name)
  SCM scm_define (SCM name, SCM val)
  SCM scm_c_module_define (SCM module, const_char_ptr name, SCM val)
  SCM scm_module_define (SCM module, SCM name, SCM val)
  SCM scm_module_ensure_local_variable (SCM module, SCM sym)
  SCM scm_module_reverse_lookup (SCM module, SCM variable)
  SCM scm_current_module ()
  SCM scm_set_current_module (SCM module)
  SCM scm_resolve_module (SCM name)
  SCM scm_c_resolve_module (const_char_ptr name)
  SCM scm_c_define_module (const_char_ptr name, void (*init) (void *), void *data)
  void scm_c_use_module (const_char_ptr name)
  void scm_c_export (const_char_ptr name, ...)

  SCM scm_c_make_gsubr (const_char_ptr name, int req, int opt, int rst, void *fcn)
  SCM scm_c_define_gsubr (const_char_ptr name, int req, int opt, int rst, void *fcn)

  # Initialization.
  void *scm_with_guile (void *(*func)(void *), void *data)
  void scm_init_guile ()
  void scm_boot_guile (int argc, char **argv, void (*main_func) (void *data, int argc, char **argv), void *data)
  void scm_shell (int argc, char **argv)

#--------------------------------------------------------------------------

cdef extern from 'sortsmill/guile/python.h':

  SCM scm_pointer_from_pyref (PyObject *obj)
  SCM scm_pointer_from_borrowed_pyref (PyObject *obj)
  SCM scm_from_scm_pyref (SCM p)
  SCM scm_from_borrowed_scm_pyref (SCM p)
  SCM scm_from_PyObject_ptr (PyObject *p)
  SCM scm_from_borrowed_PyObject_ptr (PyObject *p)
  PyObject *scm_to_PyObject_ptr (SCM obj)

  SCM scm_py_failure (SCM who, SCM irritants)
  SCM scm_c_py_failure (const_char_ptr who, SCM irritants)

  SCM scm_pynone_p (SCM)
  SCM scm_pybool_p (SCM)
  SCM scm_pyint_p (SCM)
  SCM scm_pylong_p (SCM)
  SCM scm_pympz_p (SCM)
  SCM scm_pympq_p (SCM)
  SCM scm_pyfloat_p (SCM)
  SCM scm_pycomplex_p (SCM)
  SCM scm_pyunicode_p (SCM)
  SCM scm_pybytes_p (SCM)
  SCM scm_pystring_p (SCM)
  SCM scm_pytuple_p (SCM)
  SCM scm_pylist_p (SCM)
  SCM scm_pydict_p (SCM)
  SCM scm_pycallable_p (SCM)
  SCM scm_pymodule_p (SCM)
  SCM scm_pysequence_p (SCM)
  SCM scm_pyiterable_p (SCM)
  SCM scm_pygenerator_p (SCM)

  bool scm_is_pynone (SCM)
  bool scm_is_pybool (SCM)
  bool scm_is_pyint (SCM)
  bool scm_is_pylong (SCM)
  bool scm_is_pympz (SCM)
  bool scm_is_pympq (SCM)
  bool scm_is_pyfloat (SCM)
  bool scm_is_pycomplex (SCM)
  bool scm_is_pyunicode (SCM)
  bool scm_is_pybytes (SCM)
  bool scm_is_pystring (SCM)
  bool scm_is_pytuple (SCM)
  bool scm_is_pylist (SCM)
  bool scm_is_pydict (SCM)
  bool scm_is_pycallable (SCM)
  bool scm_is_pymodule (SCM)
  bool scm_is_pysequence (SCM)
  bool scm_is_pyiterable (SCM)
  bool scm_is_pygenerator (SCM)

  bool scm_is_pyobject (SCM obj)

  SCM scm_py_none ()
  SCM scm_py_false ()
  SCM scm_py_true ()
  SCM scm_py_not (SCM obj)
  SCM scm_py_not_not (SCM obj)

  SCM scm_boolean_to_pybool (SCM obj)
  SCM scm_pybool_to_boolean (SCM obj)

  SCM scm_integer_to_pyint (SCM obj)
  SCM scm_pyint_to_integer (SCM obj)

  SCM scm_integer_to_pympz (SCM obj)
  SCM scm_pympz_to_integer (SCM obj)
  SCM scm_pympz_to_pylong (SCM obj)
  SCM scm_pylong_to_pympz (SCM obj)
  SCM scm_integer_to_pylong (SCM obj)
  SCM scm_pylong_to_integer (SCM obj)

  SCM scm_rational_to_pympq (SCM obj)
  SCM scm_pympq_to_rational (SCM obj)

  SCM scm_inexact_to_pyfloat (SCM obj)
  SCM scm_pyfloat_to_inexact (SCM obj)

  SCM scm_complex_to_pycomplex (SCM obj)
  SCM scm_pycomplex_to_complex (SCM obj)

  SCM scm_number_to_pyobject (SCM obj)
  SCM scm_pyobject_to_number (SCM obj)

  SCM scm_pointer_to_pylong (SCM obj)
  SCM scm_pylong_to_pointer (SCM obj)

  SCM scm_string_to_pystring (SCM obj)
  SCM scm_pystring_to_string (SCM obj)

  SCM scm_list_to_pytuple (SCM obj)
  SCM scm_list_to_pylist (SCM obj)

  SCM scm_pytuple_to_list (SCM obj)
  SCM scm_pylist_to_list (SCM obj)
  SCM scm_pysequence_to_list (SCM obj)

  SCM scm_py_builtins ()
  SCM scm_py_locals ()
  SCM scm_py_globals ()

#--------------------------------------------------------------------------

cdef extern from 'sortsmill/guile/fonts/anchors.h':

  SCM scm_view_anchor_classes (SCM view)
  SCM scm_glyph_view_anchor_points (SCM gv)
  SCM scm_glyph_view_anchor_points_set_x (SCM gv, SCM anchor_points)
  SCM scm_glyph_view_anchor_points_add_x (SCM gv, SCM anchor_point)
  SCM scm_anchor_point_name (SCM anchor_point)
  SCM scm_anchor_point_type (SCM anchor_point)
  SCM scm_anchor_point_coords (SCM anchor_point)
  SCM scm_anchor_point_selected_p (SCM anchor_point)
  SCM scm_anchor_point_ligature_index (SCM anchor_point)
  SCM scm_anchor_point_with_name (SCM anchor_point, SCM value)
  SCM scm_anchor_point_with_type (SCM anchor_point, SCM value)
  SCM scm_anchor_point_with_coords (SCM anchor_point, SCM value)
  SCM scm_anchor_point_with_selected_p (SCM anchor_point, SCM value)
  SCM scm_anchor_point_with_ligature_index (SCM anchor_point, SCM value)

#--------------------------------------------------------------------------

cdef extern from 'sortsmill/guile/fonts/contours.h':

  SCM scm_make_contour_point (SCM x, SCM y, SCM on_curve_p, SCM selected_p,
                              SCM name)
  SCM scm_make_on_curve_point (SCM x, SCM y)
  SCM scm_make_off_curve_point (SCM x, SCM y)
  SCM scm_c_make_on_curve_point (double x, double y)
  SCM scm_c_make_off_curve_point (double x, double y)

  SCM scm_contour_point_p (SCM obj)
  bool scm_is_contour_point (SCM obj)

  SCM scm_contour_point_x (SCM point)
  SCM scm_contour_point_y (SCM point)
  SCM scm_contour_point_on_curve_p (SCM point)
  SCM scm_contour_point_selected_p (SCM point)
  SCM scm_contour_point_name (SCM point)

  double scm_c_contour_point_x (SCM point)
  double scm_c_contour_point_y (SCM point)
  bool scm_c_contour_point_on_curve_p (SCM point)
  bool scm_c_contour_point_selected_p (SCM point)
  char *scm_c_contour_point_name (SCM point)

  SCM scm_contour_point_x_set_x (SCM point, SCM value)
  SCM scm_contour_point_y_set_x (SCM point, SCM value)
  SCM scm_contour_point_on_curve_p_set_x (SCM point, SCM value)
  SCM scm_contour_point_selected_p_set_x (SCM point, SCM value)
  SCM scm_contour_point_name_set_x (SCM point, SCM value)

  void scm_c_contour_point_x_set_x (SCM point, double value)
  void scm_c_contour_point_y_set_x (SCM point, double value)
  void scm_c_contour_point_on_curve_p_set_x (SCM point, bool value)
  void scm_c_contour_point_selected_p_set_x (SCM point, bool value)
  void scm_c_contour_point_name_set_x (SCM point, const_char_ptr value)

  SCM scm_make_contour (SCM points, SCM closed_p, SCM degree, SCM name)
  SCM scm_c_make_contour (SCM points, bool closed_p, int degree,
                          const_char_ptr name)

  SCM scm_contour_p (SCM obj)
  bool scm_is_contour (SCM obj)

  SCM scm_contour_points (SCM contour)
  SCM scm_contour_closed_p (SCM contour)
  SCM scm_contour_degree (SCM contour)
  SCM scm_contour_name (SCM contour)

  bool scm_c_contour_closed_p (SCM contour)
  int scm_c_contour_degree (SCM contour)
  char *scm_c_contour_name (SCM contour)

  SCM scm_contour_points_set_x (SCM contour, SCM value)
  SCM scm_contour_closed_p_set_x (SCM contour, SCM value)
  SCM scm_contour_degree_set_x (SCM contour, SCM value)
  SCM scm_contour_name_set_x (SCM contour, SCM value)

  void scm_c_contour_closed_p_set_x (SCM contour, bool value)
  void scm_c_contour_degree_set_x (SCM contour, int value)
  void scm_c_contour_name_set_x (SCM contour, const_char_ptr value)

  SCM scm_contour_to_malloced_SplinePointList (SCM contour)

#--------------------------------------------------------------------------

cdef inline SCM scm_eol ():
  return <SCM> <void *> <uintptr_t> __SCM_EOL

cdef inline SCM scm_eof_val ():
  return <SCM> <void *> <uintptr_t> __SCM_EOF_VAL

cdef inline SCM scm_unspecified ():
  return <SCM> <void *> <uintptr_t> __SCM_UNSPECIFIED

cdef inline SCM scm_undefined ():
  return <SCM> <void *> <uintptr_t> __SCM_UNDEFINED

cdef inline int scm_unbndp (SCM x):
  return (scm_is_eq (x, scm_undefined ()))

cdef inline SCM scm_from_object (object obj):
  return scm_from_PyObject_ptr (<PyObject *> obj)

cdef inline SCM scm_from_borrowed_object (object obj):
  return scm_from_borrowed_PyObject_ptr (<PyObject *> obj)

cdef inline object scm_to_object (SCM scm_obj):
  return <object> scm_to_PyObject_ptr (scm_obj)

cdef inline SCM scm_from_string_object (object string):
  return scm_pystring_to_string (scm_from_object (string))

cdef inline SCM scm_from_pyguile_object (object obj):
  return <SCM> <uintptr_t> obj.address

#--------------------------------------------------------------------------
