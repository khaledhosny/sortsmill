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

#include <Python.h>
#include <libguile.h>

void init_guile_sortsmillff_python (void);

static void
pyobject_finalizer (void *x)
{
  Py_XDECREF ((PyObject *) x);
}

static inline SCM
scm_pyobject_to_scm (PyObject *obj)
{
  return scm_from_pointer (obj, pyobject_finalizer);
}

static inline SCM
scm_borrowed_pyobject_to_scm (PyObject *obj)
{
  Py_XINCREF (obj);
  return scm_from_pointer (obj, pyobject_finalizer);
}

static SCM
scm_grab_pyobject_reference (SCM p)
{
  return scm_pyobject_to_scm ((PyObject *) scm_to_pointer (p));
}

static SCM
scm_grab_borrowed_pyobject_reference (SCM p)
{
  return scm_borrowed_pyobject_to_scm ((PyObject *) scm_to_pointer (p));
}

static SCM
scm_py_false (void)
{
  return scm_borrowed_pyobject_to_scm (Py_False);
}

static SCM
scm_py_true (void)
{
  return scm_borrowed_pyobject_to_scm (Py_True);
}


VISIBLE void
init_guile_sortsmillff_python (void)
{
  scm_c_define_gsubr ("grab-pyobject-reference", 1, 0, 0,
                      scm_grab_pyobject_reference);
  scm_c_define_gsubr ("grab-borrowed-pyobject-reference", 1, 0, 0,
                      scm_grab_borrowed_pyobject_reference);
  scm_c_define_gsubr ("py-false", 0, 0, 0, scm_py_false);
  scm_c_define_gsubr ("py-true", 0, 0, 0, scm_py_true);
}
