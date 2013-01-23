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
#include <gmpy.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <sortsmillff/guile/rnrs_conditions.h>
#include <intl.h>

#include <atomic_ops.h>
#include <sortsmillff/xgc.h>    // Includes gc.h and pthreads.h in the right order.

void init_guile_sortsmillff_python (void);

// FIXME: Put these in the auxiliary library, but with more
// comprehensible names.
static SCM PyObject_ptr_to_scm_pyobject (PyObject *p);
static SCM borrowed_PyObject_ptr_to_scm_pyobject (PyObject *p);

//-------------------------------------------------------------------------

static volatile AO_t gmpy_pymodule_is_initialized = false;
static pthread_mutex_t gmpy_pymodule_mutex = PTHREAD_MUTEX_INITIALIZER;

static void
initialize_gmpy_pymodule_if_necessary (void)
{
  if (!AO_load_acquire_read (&gmpy_pymodule_is_initialized))
    {
      pthread_mutex_lock (&gmpy_pymodule_mutex);
      if (!gmpy_pymodule_is_initialized)
        {
          import_gmpy ();
          AO_store_release_write (&gmpy_pymodule_is_initialized, true);
        }
      pthread_mutex_unlock (&gmpy_pymodule_mutex);
    }
}

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
scm_py_none (void)
{
  return borrowed_PyObject_ptr_to_scm_pyobject (Py_None);
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

static bool
scm_is_pyobject (SCM obj)
{
  return
    scm_is_true (scm_call_1
                 (scm_c_private_ref
                  ("sortsmillff python", "procedure:pyobject?"), obj));
}

#define _SCM_TYPECHECK_P(NAME, TYPECHECK)			\
  static SCM							\
  NAME (SCM obj)						\
  {								\
    bool result = false;					\
    if (scm_is_pyobject (obj))					\
      {								\
	PyObject *py_obj = pyobject_to_PyObject_ptr (obj);	\
	result = TYPECHECK (py_obj);				\
      }								\
    return scm_from_bool (result);				\
  }

#define _FF_PYNONE_CHECK(py_obj) ((py_obj) == Py_None)

#define _FF_PYSTRING_CHECK(py_obj)			\
  (PyUnicode_Check (py_obj) || PyBytes_Check (py_obj))

_SCM_TYPECHECK_P (scm_pynone_p, _FF_PYNONE_CHECK);
_SCM_TYPECHECK_P (scm_pybool_p, PyBool_Check);
_SCM_TYPECHECK_P (scm_pyint_p, PyInt_Check);
_SCM_TYPECHECK_P (scm_pylong_p, PyLong_Check);
_SCM_TYPECHECK_P (scm_pympz_p_core, Pympz_Check);
_SCM_TYPECHECK_P (scm_pyunicode_p, PyUnicode_Check);
_SCM_TYPECHECK_P (scm_pybytes_p, PyBytes_Check);
_SCM_TYPECHECK_P (scm_pystring_p, _FF_PYSTRING_CHECK);
_SCM_TYPECHECK_P (scm_pytuple_p, PyTuple_Check);
_SCM_TYPECHECK_P (scm_pylist_p, PyList_Check);
_SCM_TYPECHECK_P (scm_pydict_p, PyDict_Check);

static SCM
scm_pympz_p (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();
  return scm_pympz_p_core (obj);
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
        rnrs_c_make_who_condition ("scm_pybool_to_boolean"),
        rnrs_c_make_message_condition (_("expected a Python bool")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  int is_true = PyObject_IsTrue (py_obj);
  if (is_true == -1)
    scm_c_py_failure ("scm_pybool_to_boolean", scm_list_1 (obj));
  return (is_true) ? SCM_BOOL_T : SCM_BOOL_F;
}

static SCM
scm_integer_to_pyint (SCM obj)
{
  return PyObject_ptr_to_scm_pyobject (PyInt_FromLong (scm_to_long (obj)));
}

static SCM
scm_pyint_to_integer (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  if (!PyInt_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pyint_to_integer"),
        rnrs_c_make_message_condition (_("expected a Python int")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  long int n = PyInt_AsLong (py_obj);
  if (n == -1 && PyErr_Occurred ())
    scm_c_py_failure ("scm_pyint_to_integer", scm_list_1 (obj));
  return scm_from_long (n);
}

static SCM
scm_integer_to_pympz (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();
  PympzObject *z = Pympz_new ();
  scm_to_mpz (obj, Pympz_AS_MPZ (z));
  return PyObject_ptr_to_scm_pyobject ((PyObject *) z);
}

static SCM
scm_pympz_to_integer (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  if (!Pympz_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pympz_to_integer"),
        rnrs_c_make_message_condition (_("expected a Python mpz object")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  return scm_from_mpz (Pympz_AS_MPZ (py_obj));
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
        rnrs_c_make_who_condition ("scm_pylong_to_pointer"),
        rnrs_c_make_message_condition (_("expected a Python long or int")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  void *p = PyLong_AsVoidPtr (py_obj);
  return scm_from_pointer (p, NULL);
}

static SCM
scm_list_to_pytuple_general (SCM obj, const char *caller, SCM scheme_to_pyobj)
{
  ssize_t n = scm_to_ssize_t (scm_length (obj));
  PyObject *tup = PyTuple_New (n);
  if (tup == NULL)
    scm_c_py_failure (caller, scm_list_1 (obj));
  ssize_t i = 0;
  SCM p = obj;
  while (p != SCM_EOL)
    {
      SCM element = scm_call_1 (scheme_to_pyobj, SCM_CAR (p));
      PyObject *py_element = pyobject_to_PyObject_ptr (element);
      if (py_element == NULL)
        scm_c_py_failure (caller, scm_list_1 (SCM_CAR (p)));
      PyTuple_SET_ITEM (tup, i, py_element);
      i++;
      p = SCM_CDR (p);
    }
  return PyObject_ptr_to_scm_pyobject (tup);
}

static SCM
scm_list_to_pytuple (SCM obj, SCM scm_to_py)
{
  if (SCM_UNBNDP (scm_to_py))
    scm_to_py = scm_c_public_ref ("sortsmillff python", "scm->pyguile");
  return scm_list_to_pytuple_general (obj, "scm_list_to_pytuple", scm_to_py);
}

static SCM
scm_pytuple_to_list (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  if (!PyTuple_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pytuple_to_list"),
        rnrs_c_make_message_condition (_("expected a Python tuple")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  ssize_t length = PyTuple_GET_SIZE (py_obj);
  SCM p = SCM_EOL;
  for (ssize_t i = length - 1; 0 <= i; i--)
    {
      SCM element = PyObject_ptr_to_scm_pyobject (PyTuple_GET_ITEM (py_obj, i));
      p = scm_cons (element, p);
    }
  return p;
}

static SCM
scm_list_to_pylist_general (SCM obj, const char *caller, SCM scheme_to_pyobj)
{
  ssize_t n = scm_to_ssize_t (scm_length (obj));
  PyObject *lst = PyList_New (n);
  if (lst == NULL)
    scm_c_py_failure (caller, scm_list_1 (obj));
  ssize_t i = 0;
  SCM p = obj;
  while (p != SCM_EOL)
    {
      SCM element = scm_call_1 (scheme_to_pyobj, SCM_CAR (p));
      PyObject *py_element = pyobject_to_PyObject_ptr (element);
      if (py_element == NULL)
        scm_c_py_failure (caller, scm_list_1 (SCM_CAR (p)));
      PyList_SET_ITEM (lst, i, py_element);
      i++;
      p = SCM_CDR (p);
    }
  return PyObject_ptr_to_scm_pyobject (lst);
}

static SCM
scm_list_to_pylist (SCM obj, SCM scm_to_py)
{
  if (SCM_UNBNDP (scm_to_py))
    scm_to_py = scm_c_public_ref ("sortsmillff python", "scm->pyguile");
  return scm_list_to_pylist_general (obj, "scm_list_to_pylist", scm_to_py);
}

static SCM
scm_pylist_to_list (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  if (!PyList_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pylist_to_list"),
        rnrs_c_make_message_condition (_("expected a Python list")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  ssize_t length = PyList_GET_SIZE (py_obj);
  SCM p = SCM_EOL;
  for (ssize_t i = length - 1; 0 <= i; i--)
    {
      SCM element = PyObject_ptr_to_scm_pyobject (PyList_GET_ITEM (py_obj, i));
      p = scm_cons (element, p);
    }
  return p;
}

static SCM
scm_alist_to_pydict_general (SCM obj, const char *caller, SCM scheme_to_pyobj)
{
  PyObject *dict = PyDict_New ();
  if (dict == NULL)
    scm_c_py_failure (caller, scm_list_1 (obj));

  SCM p = obj;
  while (p != SCM_EOL)
    {
      SCM pair = SCM_CAR (p);
      if (!scm_is_pair)
        rnrs_raise_condition
          (scm_list_4
           (rnrs_make_assertion_violation (),
            rnrs_c_make_who_condition (caller),
            rnrs_c_make_message_condition (_("expected an association list")),
            rnrs_make_irritants_condition (scm_list_1 (obj))));

      SCM key = scm_call_1 (scheme_to_pyobj, SCM_CAR (pair));
      PyObject *py_key = pyobject_to_PyObject_ptr (key);
      if (py_key == NULL)
        scm_c_py_failure (caller, scm_list_1 (key));

      SCM value = scm_call_1 (scheme_to_pyobj, SCM_CDR (pair));
      PyObject *py_value = pyobject_to_PyObject_ptr (value);
      if (py_value == NULL)
        scm_c_py_failure (caller, scm_list_1 (value));

      int errval = PyDict_SetItem (dict, py_key, py_value);
      if (errval != 0)
        scm_c_py_failure (caller, scm_list_1 (obj));

      p = SCM_CDR (p);
    }
  return PyObject_ptr_to_scm_pyobject (dict);
}

static SCM
scm_alist_to_pydict (SCM obj, SCM scm_to_py)
{
  if (SCM_UNBNDP (scm_to_py))
    scm_to_py = scm_c_public_ref ("sortsmillff python", "scm->pyguile");
  return scm_alist_to_pydict_general (obj, "scm_alist_to_pydict", scm_to_py);
}

static SCM
scm_pyimport (SCM obj)
{
  scm_dynwind_begin (0);

  char *s = scm_to_utf8_stringn (obj, NULL);
  scm_dynwind_free (s);

  PyObject *py_string = PyUnicode_FromString (s);
  if (py_string == NULL)
    scm_c_py_failure ("scm_c_pyimport", scm_list_1 (obj));

  PyObject *module = PyImport_Import (py_string);
  Py_DECREF (py_string);
  if (module == NULL)
    scm_c_py_failure ("scm_c_pyimport", scm_list_1 (obj));

  scm_dynwind_end ();

  return PyObject_ptr_to_scm_pyobject (module);
}

// FIXME: This function should be written differently for Python 3.2+,
// because PyModule_GetFilename is deprecated then. Wrapping
// PyModule_GetFilenameObject to look like PyModule_GetFilename seems
// a good approach.
static SCM
scm_pymodule_get_file_name (SCM obj)
{
  PyObject *py_obj = pyobject_to_PyObject_ptr (obj);
  if (!PyModule_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pymodule_get_file_name"),
        rnrs_c_make_message_condition (_("expected a Python module")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  char *file_name = PyModule_GetFilename (py_obj);
  if (file_name == NULL)
    scm_c_py_failure ("scm_pymodule_get_file_name", scm_list_1 (obj));
  return scm_from_utf8_string (file_name);
}

//-------------------------------------------------------------------------
//
// Access to the sortsmillff.internal.guile_python_pyx Cython module.
// The module will be imported the first time it is accessed.
//

// FIXME: Come up with a way to do this more directly in Guile, and
// more generally.
static volatile AO_t guile_python_pyx_pymodule_is_initialized = false;
static pthread_mutex_t guile_python_pyx_pymodule_mutex =
  PTHREAD_MUTEX_INITIALIZER;
static SCM guile_python_pyx_pymodule = SCM_BOOL_F;

// This function returns a Cython module we use in the implmentation
// of (sortsmillff python).
static SCM
scm_guile_python_pyx_pymodule (void)
{
  if (!AO_load_acquire_read (&guile_python_pyx_pymodule_is_initialized))
    {
      pthread_mutex_lock (&guile_python_pyx_pymodule_mutex);
      if (!guile_python_pyx_pymodule_is_initialized)
        {
          guile_python_pyx_pymodule =
            scm_pyimport (scm_from_utf8_string
                          ("sortsmillff.internal.guile_python_pyx"));
          if (scm_is_true (guile_python_pyx_pymodule))
            scm_permanent_object (guile_python_pyx_pymodule);
          AO_store_release_write (&guile_python_pyx_pymodule_is_initialized,
                                  scm_is_true (guile_python_pyx_pymodule));
        }
      pthread_mutex_unlock (&guile_python_pyx_pymodule_mutex);
      if (scm_is_false (guile_python_pyx_pymodule))
        rnrs_raise_condition
          (scm_list_4
           (rnrs_make_error (),
            rnrs_c_make_who_condition ("scm_guile_python_pyx_pymodule"),
            rnrs_c_make_message_condition
            (_
             ("failed to import Python module sortsmillff.internal.guile_python_pyx")),
            rnrs_make_irritants_condition (SCM_EOL)));
    }
  return guile_python_pyx_pymodule;
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmillff_python (void)
{
  scm_c_define_gsubr ("py-failure", 2, 0, 0, scm_py_failure);

  scm_c_define_gsubr ("grab-pyobject-reference", 1, 0, 0,
                      scm_grab_pyobject_reference);
  scm_c_define_gsubr ("grab-borrowed-pyobject-reference", 1, 0, 0,
                      scm_grab_borrowed_pyobject_reference);

  scm_c_define_gsubr ("py-none", 0, 0, 0, scm_py_none);
  scm_c_define_gsubr ("py-false", 0, 0, 0, scm_py_false);
  scm_c_define_gsubr ("py-true", 0, 0, 0, scm_py_true);
  scm_c_define_gsubr ("py-not", 1, 0, 0, scm_py_not);
  scm_c_define_gsubr ("py-not-not", 1, 0, 0, scm_py_not_not);

  scm_c_define_gsubr ("pynone?", 1, 0, 0, scm_pynone_p);
  scm_c_define_gsubr ("pybool?", 1, 0, 0, scm_pybool_p);
  scm_c_define_gsubr ("pyint?", 1, 0, 0, scm_pyint_p);
  scm_c_define_gsubr ("pylong?", 1, 0, 0, scm_pylong_p);
  scm_c_define_gsubr ("pympz?", 1, 0, 0, scm_pympz_p);
  scm_c_define_gsubr ("pyunicode?", 1, 0, 0, scm_pyunicode_p);
  scm_c_define_gsubr ("pybytes?", 1, 0, 0, scm_pybytes_p);
  scm_c_define_gsubr ("pystring?", 1, 0, 0, scm_pystring_p);
  scm_c_define_gsubr ("pytuple?", 1, 0, 0, scm_pytuple_p);
  scm_c_define_gsubr ("pylist?", 1, 0, 0, scm_pylist_p);
  scm_c_define_gsubr ("pydict?", 1, 0, 0, scm_pydict_p);

  scm_c_define_gsubr ("boolean->pybool", 1, 0, 0, scm_boolean_to_pybool);
  scm_c_define_gsubr ("pybool->boolean", 1, 0, 0, scm_pybool_to_boolean);

  scm_c_define_gsubr ("integer->pyint", 1, 0, 0, scm_integer_to_pyint);
  scm_c_define_gsubr ("pyint->integer", 1, 0, 0, scm_pyint_to_integer);

  scm_c_define_gsubr ("integer->pympz", 1, 0, 0, scm_integer_to_pympz);
  scm_c_define_gsubr ("pympz->integer", 1, 0, 0, scm_pympz_to_integer);

  scm_c_define_gsubr ("pointer->pylong", 1, 0, 0, scm_pointer_to_pylong);
  scm_c_define_gsubr ("pylong->pointer", 1, 0, 0, scm_pylong_to_pointer);

  scm_c_define_gsubr ("list->pytuple", 1, 1, 0, scm_list_to_pytuple);
  scm_c_define_gsubr ("list->pylist", 1, 1, 0, scm_list_to_pylist);
  scm_c_define_gsubr ("alist->pydict", 1, 1, 0, scm_alist_to_pydict);

  scm_c_define_gsubr ("pytuple->list", 1, 0, 0, scm_pytuple_to_list);
  scm_c_define_gsubr ("pylist->list", 1, 0, 0, scm_pylist_to_list);

  scm_c_define_gsubr ("pyimport", 1, 0, 0, scm_pyimport);
  scm_c_define_gsubr ("pymodule-get-file-name", 1, 0, 0,
                      scm_pymodule_get_file_name);
  scm_c_define_gsubr ("guile-python-pyx-pymodule", 0, 0, 0,
                      scm_guile_python_pyx_pymodule);
}

//-------------------------------------------------------------------------
