# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012, 2013 Barry Schwartz
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

#--------------------------------------------------------------------------

include 'sortsmillff/cython/config.pxi'

from cpython.ref cimport PyObject
from libc.stdint cimport int8_t, int16_t, int32_t, int64_t, intmax_t, intptr_t
from libc.stdint cimport uint8_t, uint16_t, uint32_t, uint64_t, uintmax_t, uintptr_t
from sortsmillff.cython.const_pointers cimport const_char_ptr

cdef extern from "libguile.h":

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
  void * scm_c_with_continuation_barrier (void *(*func) (void *), void *data)

  SCM scm_dynamic_pointer (SCM name, SCM dobj)
  SCM scm_pointer_address (SCM pointer)
  SCM scm_from_pointer (void *ptr, void (*finalizer) (void*))
  void* scm_to_pointer (SCM obj)
  
  SCM scm_read_hash_extend (SCM chr, SCM proc)
  SCM scm_read (SCM port)

  SCM scm_c_public_ref (const_char_ptr module_name, const_char_ptr name)
  SCM scm_c_private_ref (const_char_ptr module_name, const_char_ptr name)

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

  # Initialization.
  void *scm_with_guile (void *(*func)(void *), void *data)
  void scm_init_guile ()
  void scm_boot_guile (int argc, char **argv, void (*main_func) (void *data, int argc, char **argv), void *data)
  void scm_shell (int argc, char **argv)

#--------------------------------------------------------------------------

####
#### FIXME: Refer to a .h file here after the code has been moved from
#### the python.scm support DLL to the auxiliary library.
####
###cdef extern:
###  SCM PyObject_ptr_to_scm_pyobject (PyObject *p)
###  SCM borrowed_PyObject_ptr_to_scm_pyobject (PyObject *p)
###  PyObject *pyobject_to_PyObject_ptr (SCM obj)
###
###  SCM scm_list_to_pytuple (SCM obj)
###  SCM scm_pytuple_to_list (SCM obj)
###  SCM scm_list_to_pylist (SCM obj)
###  SCM scm_pylist_to_list (SCM obj)

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

#--------------------------------------------------------------------------
