# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Barry Schwartz
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

  # Initialization.
  void *scm_with_guile (void *(*func)(void *), void *data)
  void scm_init_guile ()
  void scm_boot_guile (int argc, char **argv, void (*main_func) (void *data, int argc, char **argv), void *data)
  void scm_shell (int argc, char **argv)


#--------------------------------------------------------------------------
