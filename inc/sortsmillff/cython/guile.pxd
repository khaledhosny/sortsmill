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

from libc.stdint cimport int8_t, int16_t, int32_t, int64_t, intmax_t
from libc.stdint cimport uint8_t, uint16_t, uint32_t, uint64_t, uintmax_t
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
  SCM scm_length (SCM)

  int scm_to_int (SCM)
  int8_t scm_to_int8 (SCM)
  int16_t scm_to_int16 (SCM)
  int32_t scm_to_int32 (SCM)
  int64_t scm_to_int64 (SCM)
  char scm_to_char (SCM)
  short int scm_to_short (SCM)
  long int scm_to_long (SCM)
  ssize_t scm_to_ssize_t (SCM)
  intmax_t scm_to_intmax (SCM)

  unsigned int scm_to_uint (SCM)
  uint8_t scm_to_uint8 (SCM)
  uint16_t scm_to_uint16 (SCM)
  uint32_t scm_to_uint32 (SCM)
  uint64_t scm_to_uint64 (SCM)
  unsigned char scm_to_uchar (SCM)
  unsigned short int scm_to_ushort (SCM)
  unsigned long int scm_to_ulong (SCM)
  size_t scm_to_size_t (SCM)
  uintmax_t scm_to_uintmax (SCM)

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
