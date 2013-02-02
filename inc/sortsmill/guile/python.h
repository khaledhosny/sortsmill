/*
 * Copyright (C) 2013 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_PYTHON_H
#define _SORTSMILL_PYTHON_H

#include <Python.h>
#include <libguile.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* reference -> foreign pointer */
SCM scm_pointer_from_pyref (PyObject *obj);

/* borrowed reference -> foreign pointer */
SCM scm_pointer_from_borrowed_pyref (PyObject *obj);

/* @var{pointer->pyobject} */
SCM scm_from_scm_pyref (SCM p);

/* @var{borrowed-pointer->pyobject} */
SCM scm_from_borrowed_scm_pyref (SCM p);

/* reference -> pyobject */
SCM scm_from_PyObject_ptr (PyObject *p);

/* borrowed reference -> pyobject */
SCM borrowed_scm_from_PyObject_ptr (PyObject *p);

/* pyobject -> reference */
PyObject *scm_to_PyObject_ptr (SCM obj);

SCM scm_py_failure (SCM who, SCM irritants);
SCM scm_c_py_failure (const char *who, SCM irritants);

SCM scm_pynone_p (SCM obj);
SCM scm_pybool_p (SCM obj);
SCM scm_pyint_p (SCM obj);
SCM scm_pylong_p (SCM obj);
SCM scm_pympz_p (SCM obj);
SCM scm_pyunicode_p (SCM obj);
SCM scm_pybytes_p (SCM obj);
SCM scm_pystring_p (SCM obj);
SCM scm_pytuple_p (SCM obj);
SCM scm_pylist_p (SCM obj);
SCM scm_pydict_p (SCM obj);
SCM scm_pycallable_p (SCM obj);
SCM scm_pymodule_p (SCM obj);
SCM scm_pysequence_p (SCM obj);
SCM scm_pyiterable_p (SCM obj);
SCM scm_pygenerator_p (SCM obj);

bool scm_is_pyobject (SCM obj);

SCM scm_py_none (void);
SCM scm_py_false (void);
SCM scm_py_true (void);
SCM scm_py_not (SCM obj);
SCM scm_py_not_not (SCM obj);

SCM scm_boolean_to_pybool (SCM obj);
SCM scm_pybool_to_boolean (SCM obj);

SCM scm_integer_to_pyint (SCM obj);
SCM scm_pyint_to_integer (SCM obj);

SCM scm_integer_to_pympz (SCM obj);
SCM scm_pympz_to_integer (SCM obj);

SCM scm_pointer_to_pylong (SCM obj);
SCM scm_pylong_to_pointer (SCM obj);

SCM scm_list_to_pytuple (SCM obj);
SCM scm_list_to_pylist (SCM obj);

SCM scm_pytuple_to_list (SCM obj);
SCM scm_pylist_to_list (SCM obj);
SCM scm_pysequence_to_list (SCM obj);

SCM scm_py_builtins (void);
SCM scm_py_locals (void);
SCM scm_py_globals (void);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_PYTHON_H */
