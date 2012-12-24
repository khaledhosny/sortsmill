#include <config.h>

// Copyright (C) 2012 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#ifdef _NO_PYTHON

static int there_is_nothing_in_this_unit = 0;

#else // Python

#include <Python.h>
#include <libguile.h>

void init_guile_sortsmillff_python (void);

static SCM
_scm_get_Py_False (void)
{
  return scm_from_pointer (Py_False, NULL);
}

static SCM
_scm_get_Py_True (void)
{
  return scm_from_pointer (Py_True, NULL);
}

VISIBLE void
init_guile_sortsmillff_python (void)
{
  scm_c_define_gsubr ("get-Py_False", 0, 0, 0, _scm_get_Py_False);
  scm_c_define_gsubr ("get-Py_True", 0, 0, 0, _scm_get_Py_True);
}

#endif // Python

