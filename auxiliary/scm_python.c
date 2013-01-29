#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

int something_to_ensure_this_source_is_nonempty = 0;

#ifndef _NO_PYTHON

#include <sortsmill/guile/python.h>

static void
pyref_finalizer (void *obj)
{
  Py_XDECREF ((PyObject *) obj);
}

VISIBLE SCM
scm_from_pyref (PyObject *obj)
{
  return scm_from_pointer (obj, pyref_finalizer);
}

VISIBLE SCM
scm_from_borrowed_pyref (PyObject *obj)
{
  Py_XINCREF (obj);
  return scm_from_pyref (obj);
}

VISIBLE SCM
scm_pointer_to_pyobject (SCM p)
{
  return scm_call_1 (scm_c_public_ref ("sortsmill python",
                                       "pointer->pyobject"), p);
}

VISIBLE SCM
scm_borrowed_pointer_to_pyobject (SCM p)
{
  return scm_call_1 (scm_c_public_ref ("sortsmill python",
                                       "borrowed-pointer->pyobject"), p);
}

VISIBLE SCM
scm_from_PyObject_ptr (PyObject *p)
{
  return scm_pointer_to_pyobject (scm_from_pointer (p, NULL));
}

VISIBLE SCM
borrowed_scm_from_PyObject_ptr (PyObject *p)
{
  return scm_borrowed_pointer_to_pyobject (scm_from_pointer (p, NULL));
}

static SCM
scm_pyobject_to_scm_pointer (SCM obj)
{
  return
    scm_call_1 (scm_c_public_ref ("sortsmill python", "pyobject->pointer"),
                obj);
}

VISIBLE PyObject *
scm_to_PyObject_ptr (SCM obj)
{
  return (PyObject *) scm_to_pointer (scm_pyobject_to_scm_pointer (obj));
}

#endif	// _NO_PYTHON
