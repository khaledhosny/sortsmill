#include <config.h>

// Copyright (C) 2012, 2013 Barry Schwartz
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
#include <assert.h>
#include <sortsmillff/guile/rnrs_conditions.h>
#include <intl.h>

void init_guile_sortsmillff_python (void);

// FIXME: Put these in the auxiliary library, but with more
// comprehensible names.
static SCM PyObject_ptr_to_scm_pyobject (PyObject *p);
static SCM borrowed_PyObject_ptr_to_scm_pyobject (PyObject *p);

//-------------------------------------------------------------------------

// FIXME: Put this in the auxiliary library.
static SCM
rnrs_make_python_error (SCM ptype, SCM pvalue, SCM ptraceback)
{
  return scm_call_3 (scm_c_public_ref ("sortsmillff python",
                                       "make-python-error"),
                     ptype, pvalue, ptraceback);
}

// FIXME: Put this in the auxiliary library.
static SCM
rnrs_c_make_python_error (PyObject *ptype, PyObject *pvalue,
                          PyObject *ptraceback)
{
  return rnrs_make_python_error (PyObject_ptr_to_scm_pyobject (ptype),
                                 PyObject_ptr_to_scm_pyobject (pvalue),
                                 PyObject_ptr_to_scm_pyobject (ptraceback));
}

static SCM
scm_py_failure (SCM who, SCM irritants)
{
  assert (scm_list_p (irritants));

  PyObject *py_err = PyErr_Occurred ();
  if (py_err != NULL)
    {
      PyObject *ptype;
      PyObject *pvalue;
      PyObject *ptraceback;
      PyErr_Fetch (&ptype, &pvalue, &ptraceback);
      rnrs_raise_condition
        (scm_list_3
         (rnrs_c_make_python_error (ptype, pvalue, ptraceback),
          rnrs_make_who_condition (who),
          rnrs_make_irritants_condition (irritants)));
    }
  else
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_error (),
        rnrs_make_who_condition (who),
        rnrs_c_make_message_condition (_("Python failure")),
        rnrs_make_irritants_condition (irritants)));

  return SCM_UNSPECIFIED;
}

static SCM
scm_c_py_failure (const char *who, SCM irritants)
{
  return scm_py_failure (scm_from_utf8_string (who), irritants);
}

//-------------------------------------------------------------------------

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
  return scm_pyobject_to_scm (obj);
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
scm_pointer_to_scm_pyobject (SCM p)
{
  return scm_call_1 (scm_c_public_ref ("sortsmillff python",
                                       "pointer->pyobject"), p);
}

static SCM
scm_borrowed_pointer_to_scm_pyobject (SCM p)
{
  return scm_call_1 (scm_c_public_ref ("sortsmillff python",
                                       "borrowed-pointer->pyobject"), p);
}

static SCM
PyObject_ptr_to_scm_pyobject (PyObject *p)
{
  return scm_pointer_to_scm_pyobject (scm_from_pointer (p, NULL));
}

static SCM
borrowed_PyObject_ptr_to_scm_pyobject (PyObject *p)
{
  return scm_borrowed_pointer_to_scm_pyobject (scm_from_pointer (p, NULL));
}

static SCM
scm_pyobject_to_scm_pointer (SCM obj)
{
  return scm_call_1 (scm_c_private_ref ("sortsmillff python",
                                        "procedure:pyobject->pointer"), obj);
}

static PyObject *
pyobject_to_PyObject_ptr (SCM obj)
{
  return (PyObject *) scm_to_pointer (scm_pyobject_to_scm_pointer (obj));
}

static SCM
scm_py_false (void)
{
  return borrowed_PyObject_ptr_to_scm_pyobject (Py_False);
}

static SCM
scm_py_true (void)
{
  return borrowed_PyObject_ptr_to_scm_pyobject (Py_True);
}

static SCM
scm_py_not (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  int is_true = PyObject_Not (py_obj);
  if (is_true == -1)
    scm_c_py_failure ("scm_py_not", scm_list_1 (obj));
  return (is_true) ? scm_py_true () : scm_py_false ();
}

static SCM
scm_py_not_not (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  int is_true = PyObject_IsTrue (py_obj);
  if (is_true == -1)
    scm_c_py_failure ("scm_py_not_not", scm_list_1 (obj));
  return (is_true) ? scm_py_true () : scm_py_false ();
}

static SCM
scm_boolean_to_pybool (SCM obj)
{
  return (scm_to_bool (obj)) ? scm_py_true () : scm_py_false ();
}

static SCM
scm_pybool_to_boolean (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  if (!PyBool_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (_("scm_pybool_to_boolean")),
        rnrs_c_make_message_condition (_("expected a Python bool")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  int is_true = PyObject_IsTrue (py_obj);
  if (is_true == -1)
    scm_c_py_failure ("scm_pybool_to_boolean", scm_list_1 (obj));
  return (is_true) ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM
scm_pointer_to_pylong (SCM p)
{
  PyObject *obj = PyLong_FromVoidPtr (scm_to_pointer (p));
  if (obj == NULL)
    scm_c_py_failure ("scm_pointer_to_pylong", scm_list_1 (p));
  return PyObject_ptr_to_scm_pyobject (obj);
}

static SCM
scm_pylong_to_pointer (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  if (!PyLong_Check (py_obj) && !PyInt_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (_("scm_pylong_to_pointer")),
        rnrs_c_make_message_condition (_("expected a Python long or int")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  void *p = PyLong_AsVoidPtr (py_obj);
  return scm_from_pointer (p, NULL);
}

VISIBLE void
init_guile_sortsmillff_python (void)
{
  scm_c_define_gsubr ("py-failure", 2, 0, 0, scm_py_failure);
  scm_c_define_gsubr ("grab-pyobject-reference", 1, 0, 0,
                      scm_grab_pyobject_reference);
  scm_c_define_gsubr ("grab-borrowed-pyobject-reference", 1, 0, 0,
                      scm_grab_borrowed_pyobject_reference);
  scm_c_define_gsubr ("py-false", 0, 0, 0, scm_py_false);
  scm_c_define_gsubr ("py-true", 0, 0, 0, scm_py_true);
  scm_c_define_gsubr ("py-not", 1, 0, 0, scm_py_not);
  scm_c_define_gsubr ("py-not-not", 1, 0, 0, scm_py_not_not);
  scm_c_define_gsubr ("boolean->pybool", 1, 0, 0, scm_boolean_to_pybool);
  scm_c_define_gsubr ("pybool->boolean", 1, 0, 0, scm_pybool_to_boolean);
  scm_c_define_gsubr ("pointer->pylong", 1, 0, 0, scm_pointer_to_pylong);
  scm_c_define_gsubr ("pylong->pointer", 1, 0, 0, scm_pylong_to_pointer);
}
