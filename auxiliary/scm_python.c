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

#include <sortsmill/guile/python.h>

static void
pyref_finalizer (void *obj)
{
  Py_XDECREF ((PyObject *) obj);
}

VISIBLE SCM
scm_pointer_from_pyref (PyObject *obj)
{
  return scm_from_pointer (obj, pyref_finalizer);
}

VISIBLE SCM
scm_pointer_from_borrowed_pyref (PyObject *obj)
{
  Py_XINCREF (obj);
  return scm_pointer_from_pyref (obj);
}

VISIBLE SCM
scm_from_scm_pyref (SCM p)
{
  return scm_call_1 (scm_c_public_ref ("sortsmill python",
                                       "pointer->pyobject"), p);
}

VISIBLE SCM
scm_from_borrowed_scm_pyref (SCM p)
{
  return scm_call_1 (scm_c_public_ref ("sortsmill python",
                                       "borrowed-pointer->pyobject"), p);
}

VISIBLE SCM
scm_from_PyObject_ptr (PyObject *p)
{
  return scm_from_scm_pyref (scm_from_pointer (p, NULL));
}

VISIBLE SCM
scm_from_borrowed_PyObject_ptr (PyObject *p)
{
  return scm_from_borrowed_scm_pyref (scm_from_pointer (p, NULL));
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
