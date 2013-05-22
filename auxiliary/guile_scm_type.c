#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/guile/scm_type.h>

// Generate instances of inline functions.
VISIBLE SCM scm_c_SCM_ref (void *p);
VISIBLE SCM scm_SCM_ref (SCM ptr);
VISIBLE SCM scm_c_SCM_set_x (void *p, SCM value);
VISIBLE SCM scm_SCM_set_x (SCM ptr, SCM value);
VISIBLE SCM scm_bytevector_SCM_ref (SCM bv, SCM index);
VISIBLE SCM scm_bytevector_SCM_set_x (SCM bv, SCM index, SCM value);

VISIBLE SCM
scm_c_bytevector_SCM_ref (SCM bv, size_t index)
{
  uint8_t temp[sizeof (SCM)];
  for (size_t i = 0; i < sizeof (SCM); i++)
    temp[i] = scm_c_bytevector_ref (bv, index + i);
  SCM result;
  memcpy (&result, temp, sizeof (SCM));
  return result;
}

VISIBLE SCM
scm_c_bytevector_SCM_set_x (SCM bv, size_t index, SCM value)
{
  uint8_t temp[sizeof (SCM)];
  memcpy (temp, &value, sizeof (SCM));
  for (size_t i = 0; i < sizeof (SCM); i++)
    scm_c_bytevector_set_x (bv, index + i, temp[i]);
  return SCM_UNSPECIFIED;
}


VISIBLE void
init_sortsmill_guile_scm_type (void)
{
  scm_c_define_gsubr ("SCM-ref", 1, 0, 0, scm_SCM_ref);
  scm_c_define_gsubr ("SCM-set!", 2, 0, 0, scm_SCM_set_x);
  scm_c_define_gsubr ("bytevector-SCM-ref", 2, 0, 0, scm_bytevector_SCM_ref);
  scm_c_define_gsubr ("bytevector-SCM-set!", 3, 0, 0, scm_bytevector_SCM_set_x);
}
