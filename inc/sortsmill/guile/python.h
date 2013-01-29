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

SCM scm_list_to_pytuple (SCM obj);
SCM scm_pytuple_to_list (SCM obj);
SCM scm_list_to_pylist (SCM obj);
SCM scm_pylist_to_list (SCM obj);
SCM scm_pysequence_to_list (SCM obj);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_PYTHON_H */
