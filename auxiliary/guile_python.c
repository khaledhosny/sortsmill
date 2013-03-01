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
#include <sortsmill/guile/rnrs_conditions.h>
#include <sortsmill/guile/python.h>
#include <intl.h>

#include <atomic_ops.h>
#include <sortsmill/xgc.h>      // Includes gc.h and pthreads.h in the right order.

void init_guile_sortsmill_python (void);

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

VISIBLE SCM
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
      SCM scm_ptype = scm_from_PyObject_ptr (ptype);
      SCM scm_pvalue = scm_from_PyObject_ptr (pvalue);
      SCM scm_ptraceback = scm_from_PyObject_ptr (ptraceback);
      SCM exc_info =
        scm_list_to_pytuple (scm_list_3
                             (scm_ptype, scm_pvalue, scm_ptraceback));
      scm_throw (scm_from_utf8_symbol ("python-exception"),
                 scm_list_2 (scm_from_utf8_string ("scm_py_failure"),
                             exc_info));
    }
  else
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_error (),
        rnrs_make_who_condition (who),
        rnrs_c_make_message_condition (_("Python error")),
        rnrs_make_irritants_condition (irritants)));

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_c_py_failure (const char *who, SCM irritants)
{
  return scm_py_failure (scm_from_utf8_string (who), irritants);
}

//-------------------------------------------------------------------------

static SCM
scm_grab_pyref (SCM p)
{
  return scm_pointer_from_pyref ((PyObject *) scm_to_pointer (p));
}

static SCM
scm_grab_borrowed_pyref (SCM p)
{
  return scm_pointer_from_borrowed_pyref ((PyObject *) scm_to_pointer (p));
}

VISIBLE SCM
scm_py_none (void)
{
  return scm_from_borrowed_PyObject_ptr (Py_None);
}

VISIBLE SCM
scm_py_false (void)
{
  return scm_from_borrowed_PyObject_ptr (Py_False);
}

VISIBLE SCM
scm_py_true (void)
{
  return scm_from_borrowed_PyObject_ptr (Py_True);
}

VISIBLE SCM
scm_py_not (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
  int is_true = PyObject_Not (py_obj);
  if (is_true == -1)
    scm_c_py_failure ("scm_py_not", scm_list_1 (obj));
  return (is_true) ? scm_py_true () : scm_py_false ();
}

VISIBLE SCM
scm_py_not_not (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
  int is_true = PyObject_IsTrue (py_obj);
  if (is_true == -1)
    scm_c_py_failure ("scm_py_not_not", scm_list_1 (obj));
  return (is_true) ? scm_py_true () : scm_py_false ();
}

VISIBLE bool
scm_is_pyobject (SCM obj)
{
  return
    scm_is_true (scm_call_1
                 (scm_c_private_ref ("sortsmill python", "procedure:pyobject?"),
                  obj));
}

#define _VISIBLE_SCM_TYPECHECK_P(P_NAME, C_NAME, TYPECHECK)		\
									\
  VISIBLE SCM								\
  P_NAME (SCM obj)							\
  {									\
    bool result = false;						\
    if (scm_is_pyobject (obj))						\
      {									\
	PyObject *py_obj = scm_to_PyObject_ptr (obj);			\
	result = TYPECHECK (py_obj);					\
      }									\
    return scm_from_bool (result);					\
  }									\
									\
  /* Generate code from an inline definition that is elsewhere. */	\
  VISIBLE bool C_NAME (SCM obj);

#define _STATIC_SCM_TYPECHECK_P(P_NAME, TYPECHECK)	\
  static SCM						\
  P_NAME (SCM obj)					\
  {							\
    bool result = false;				\
    if (scm_is_pyobject (obj))				\
      {							\
	PyObject *py_obj = scm_to_PyObject_ptr (obj);	\
	result = TYPECHECK (py_obj);			\
      }							\
    return scm_from_bool (result);			\
  }							\

#define _FF_PYNONE_CHECK(py_obj) ((py_obj) == Py_None)

#define _FF_PYSTRING_CHECK(py_obj)			\
  (PyUnicode_Check (py_obj) || PyBytes_Check (py_obj))

_VISIBLE_SCM_TYPECHECK_P (scm_pynone_p, scm_is_pynone, _FF_PYNONE_CHECK);
_VISIBLE_SCM_TYPECHECK_P (scm_pybool_p, scm_is_pybool, PyBool_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pyint_p, scm_is_pyint, PyInt_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pylong_p, scm_is_pylong, PyLong_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pyfloat_p, scm_is_pyfloat, PyFloat_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pycomplex_p, scm_is_pycomplex, PyComplex_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pyunicode_p, scm_is_pyunicode, PyUnicode_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pybytes_p, scm_is_pybytes, PyBytes_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pystring_p, scm_is_pystring, _FF_PYSTRING_CHECK);
_VISIBLE_SCM_TYPECHECK_P (scm_pytuple_p, scm_is_pytuple, PyTuple_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pylist_p, scm_is_pylist, PyList_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pydict_p, scm_is_pydict, PyDict_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pycallable_p, scm_is_pycallable,
                          PyCallable_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pymodule_p, scm_is_pymodule, PyModule_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pysequence_p, scm_is_pysequence,
                          PySequence_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pyiterable_p, scm_is_pyiterable, PyIter_Check);
_VISIBLE_SCM_TYPECHECK_P (scm_pygenerator_p, scm_is_pygenerator, PyGen_Check);

_STATIC_SCM_TYPECHECK_P (scm_pympz_p_core, Pympz_Check);
_STATIC_SCM_TYPECHECK_P (scm_pympq_p_core, Pympq_Check);

VISIBLE SCM
scm_pympz_p (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();
  return scm_pympz_p_core (obj);
}

// Generate code from an inline definition that is elsewhere.
VISIBLE bool scm_is_pympz (SCM obj);

VISIBLE SCM
scm_pympq_p (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();
  return scm_pympq_p_core (obj);
}

// Generate code from an inline definition that is elsewhere.
VISIBLE bool scm_is_pympq (SCM obj);

VISIBLE SCM
scm_boolean_to_pybool (SCM obj)
{
  return (scm_to_bool (obj)) ? scm_py_true () : scm_py_false ();
}

VISIBLE SCM
scm_pybool_to_boolean (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
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

VISIBLE SCM
scm_integer_to_pyint (SCM obj)
{
  return scm_from_PyObject_ptr (PyInt_FromLong (scm_to_long (obj)));
}

VISIBLE SCM
scm_pyint_to_integer (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
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

VISIBLE SCM
scm_integer_to_pympz (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();
  PympzObject *z = Pympz_new ();
  if (z == NULL)
    scm_c_py_failure ("scm_integer_to_pympz", scm_list_1 (obj));
  scm_to_mpz (obj, Pympz_AS_MPZ (z));
  return scm_from_PyObject_ptr ((PyObject *) z);
}

VISIBLE SCM
scm_pympz_to_integer (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
  if (!Pympz_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pympz_to_integer"),
        rnrs_c_make_message_condition (_("expected a Python mpz object")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  return scm_from_mpz (Pympz_AS_MPZ (py_obj));
}

VISIBLE SCM
scm_pympz_to_pylong (SCM obj)
{
  return
    scm_call_1 (scm_c_public_ref ("sortsmill python", "pympz->pylong"), obj);
}

VISIBLE SCM
scm_pylong_to_pympz (SCM obj)
{
  return
    scm_call_1 (scm_c_public_ref ("sortsmill python", "pylong->pympz"), obj);
}

VISIBLE SCM
scm_integer_to_pylong (SCM obj)
{
  return scm_call_1 (scm_c_public_ref ("sortsmill python", "integer->pylong"),
                     obj);
}

VISIBLE SCM
scm_pylong_to_integer (SCM obj)
{
  return scm_call_1 (scm_c_public_ref ("sortsmill python", "pylong->integer"),
                     obj);
}

static void
mpz_clear_void_ptr (void *z)
{
  mpz_clear (*(mpz_t *) z);
}

VISIBLE SCM
scm_rational_to_pympq (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();

  PympqObject *q = Pympq_new ();
  if (q == NULL)
    scm_c_py_failure ("scm_rational_to_pympq", scm_list_1 (obj));

  scm_dynwind_begin (0);

  mpz_t z;
  mpz_init (z);
  scm_dynwind_unwind_handler (mpz_clear_void_ptr, &z, SCM_F_WIND_EXPLICITLY);

  scm_to_mpz (scm_numerator (obj), z);
  mpz_set (mpq_numref (Pympq_AS_MPQ (q)), z);

  scm_to_mpz (scm_denominator (obj), z);
  mpz_set (mpq_denref (Pympq_AS_MPQ (q)), z);

  mpq_canonicalize (Pympq_AS_MPQ (q));

  scm_dynwind_end ();

  return scm_from_PyObject_ptr ((PyObject *) q);
}

VISIBLE SCM
scm_pympq_to_rational (SCM obj)
{
  initialize_gmpy_pymodule_if_necessary ();

  PyObject *py_obj = scm_to_PyObject_ptr (obj);
  if (!Pympq_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pympq_to_rational"),
        rnrs_c_make_message_condition (_("expected a Python mpq object")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));

  SCM numer = scm_from_mpz (mpq_numref (Pympq_AS_MPQ ((PympqObject *) py_obj)));
  SCM denom = scm_from_mpz (mpq_denref (Pympq_AS_MPQ ((PympqObject *) py_obj)));

  return scm_divide (numer, denom);
}

VISIBLE SCM
scm_inexact_to_pyfloat (SCM obj)
{
  PyObject *py_obj = PyFloat_FromDouble (scm_to_double (obj));
  if (py_obj == NULL)
    scm_c_py_failure ("scm_inexact_to_pyfloat", scm_list_1 (obj));
  return scm_from_PyObject_ptr (py_obj);
}

VISIBLE SCM
scm_pyfloat_to_inexact (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
  if (!PyFloat_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pyfloat_to_inexact"),
        rnrs_c_make_message_condition (_("expected a Python float object")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  double r = PyFloat_AsDouble (py_obj);
  if (r == -1.0 && PyErr_Occurred ())
    scm_c_py_failure ("scm_pyfloat_to_inexact", scm_list_1 (obj));
  return scm_from_double (r);
}

VISIBLE SCM
scm_complex_to_pycomplex (SCM z)
{
  PyObject *py_obj = PyComplex_FromDoubles (scm_to_double (scm_real_part (z)),
                                            scm_to_double (scm_imag_part (z)));
  if (py_obj == NULL)
    scm_c_py_failure ("scm_complex_to_pycomplex", scm_list_1 (z));
  return scm_from_PyObject_ptr (py_obj);
}

VISIBLE SCM
scm_pycomplex_to_complex (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
  if (!PyComplex_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pycomplex_to_complex"),
        rnrs_c_make_message_condition (_("expected a Python complex object")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  return
    scm_make_rectangular (scm_from_double (PyComplex_RealAsDouble (py_obj)),
                          scm_from_double (PyComplex_ImagAsDouble (py_obj)));
}

VISIBLE SCM
scm_number_to_pyobject (SCM obj)
{
  SCM result = SCM_UNDEFINED;
  if (scm_is_inexact (obj))
    {
      if (scm_is_real (obj))
	result = scm_inexact_to_pyfloat (obj);
      else
	result = scm_complex_to_pycomplex (obj);
    }
  else if (scm_is_integer (obj))
    {
      if (scm_is_signed_integer (obj, -2147483648, 2147483647))
	result = scm_integer_to_pyint (obj);
      else
	result = scm_integer_to_pylong (obj);
    }
  else if (scm_is_rational (obj))
    result = scm_rational_to_pympq (obj);
  else
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_number_to_pyobject"),
        rnrs_c_make_message_condition (_("expected a number")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  return result;
}

VISIBLE SCM
scm_pyobject_to_number (SCM obj)
{
  SCM result = SCM_UNDEFINED;
  if (scm_is_pyint (obj))
    result = scm_pyint_to_integer (obj);
  else if (scm_is_pyfloat (obj))
    result = scm_pyfloat_to_inexact (obj);
  else if (scm_is_pylong (obj))
    result = scm_pylong_to_integer (obj);
  else if (scm_is_pympz (obj))
    result = scm_pympz_to_integer (obj);
  else if (scm_is_pympq (obj))
    result = scm_pympq_to_rational (obj);
  else if (scm_is_pycomplex (obj))
    result = scm_pycomplex_to_complex (obj);
  else
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_pyobject_to_number"),
        rnrs_c_make_message_condition (_("cannot convert the Python object to a Guile number")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  return result;
}

VISIBLE SCM
scm_pointer_to_pylong (SCM p)
{
  PyObject *obj = PyLong_FromVoidPtr (scm_to_pointer (p));
  if (obj == NULL)
    scm_c_py_failure ("scm_pointer_to_pylong", scm_list_1 (p));
  return scm_from_PyObject_ptr (obj);
}

VISIBLE SCM
scm_pylong_to_pointer (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
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

// As a convenience, scm_string_to_pystring accepts pystrings and
// returns them unmodified.
VISIBLE SCM
scm_string_to_pystring (SCM obj)
{
  const char *who = "scm_string_to_pystring";

  SCM result = SCM_UNDEFINED;
  if (scm_is_pystring (obj))
    result = obj;
  else if (!scm_is_string (obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("expected a string or pystring")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  else
    {
      scm_dynwind_begin (0);

      size_t n;
      char *s = scm_to_utf8_stringn (obj, &n);
      scm_dynwind_free (s);

      PyObject *py_s = PyUnicode_FromStringAndSize (s, n);

      scm_dynwind_end ();

      if (py_s == NULL)
        scm_c_py_failure (who, scm_list_1 (obj));

      result = scm_from_PyObject_ptr (py_s);
    }
  return result;
}

// As a convenience, scm_pystring_to_string accepts strings and
// returns them unmodified.
VISIBLE SCM
scm_pystring_to_string (SCM obj)
{
  const char *who = "scm_pystring_to_string";

  SCM result = SCM_UNDEFINED;
  if (scm_is_string (obj))
    result = obj;
  else
    {
      PyObject *py_obj = scm_to_PyObject_ptr (obj);
      if (PyUnicode_Check (py_obj))
        {
          py_obj = PyUnicode_AsEncodedString (py_obj, "UTF-8", "strict");
          if (py_obj == NULL)
            scm_c_py_failure (who, scm_list_1 (obj));
        }
      else if (!PyBytes_Check (py_obj))
        rnrs_raise_condition
          (scm_list_4
           (rnrs_make_assertion_violation (),
            rnrs_c_make_who_condition (who),
            rnrs_c_make_message_condition (_("expected a string or pystring")),
            rnrs_make_irritants_condition (scm_list_1 (obj))));
      else
        Py_INCREF (py_obj);

      char *buffer;
      ssize_t n;
      int errval = PyBytes_AsStringAndSize (py_obj, &buffer, &n);
      if (errval == -1)
        {
          Py_DECREF (py_obj);
          scm_c_py_failure (who, scm_list_1 (obj));
        }
      else
        {
          // scm_from_utf8_stringn copies the string, possibly
          // decoding it.
          result = scm_from_utf8_stringn (buffer, n);

          Py_DECREF (py_obj);
        }
    }
  return result;
}

VISIBLE SCM
scm_list_to_pytuple (SCM obj)
{
  const char *who = "scm_list_to_pytuple";

  ssize_t n = scm_to_ssize_t (scm_length (obj));
  PyObject *tup = PyTuple_New (n);
  if (tup == NULL)
    scm_c_py_failure (who, scm_list_1 (obj));

  SCM p = obj;
  for (ssize_t i = 0; i < n; i++)
    {
      PyObject *py_element = scm_to_PyObject_ptr (SCM_CAR (p));
      if (py_element == NULL)
        scm_c_py_failure (who, scm_list_1 (SCM_CAR (p)));
      PyTuple_SET_ITEM (tup, i, py_element);
      p = SCM_CDR (p);
    }

  // Do not increment reference counts until after exceptions would
  // have been thrown.
  for (ssize_t i = 0; i < n; i++)
    {
      PyObject *py_element = PyTuple_GET_ITEM (tup, i);
      Py_INCREF (py_element);
      PyTuple_SET_ITEM (tup, i, py_element);
    }

  return scm_from_PyObject_ptr (tup);
}

VISIBLE SCM
scm_pytuple_to_list (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
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
      SCM element =
        scm_from_borrowed_PyObject_ptr (PyTuple_GET_ITEM (py_obj, i));
      p = scm_cons (element, p);
    }
  return p;
}

VISIBLE SCM
scm_list_to_pylist (SCM obj)
{
  const char *who = "scm_list_to_pylist";

  ssize_t n = scm_to_ssize_t (scm_length (obj));
  PyObject *lst = PyList_New (n);
  if (lst == NULL)
    scm_c_py_failure (who, scm_list_1 (obj));

  SCM p = obj;
  for (ssize_t i = 0; i < n; i++)
    {
      PyObject *py_element = scm_to_PyObject_ptr (SCM_CAR (p));
      if (py_element == NULL)
        scm_c_py_failure (who, scm_list_1 (SCM_CAR (p)));
      PyList_SET_ITEM (lst, i, py_element);
      p = SCM_CDR (p);
    }

  // Do not increment reference counts until after exceptions would
  // have been thrown.
  for (ssize_t i = 0; i < n; i++)
    {
      PyObject *py_element = PyList_GET_ITEM (lst, i);
      Py_INCREF (py_element);
      PyList_SET_ITEM (lst, i, py_element);
    }

  return scm_from_PyObject_ptr (lst);
}

VISIBLE SCM
scm_pylist_to_list (SCM obj)
{
  PyObject *py_obj = scm_to_PyObject_ptr (obj);
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
      SCM element =
        scm_from_borrowed_PyObject_ptr (PyList_GET_ITEM (py_obj, i));
      p = scm_cons (element, p);
    }
  return p;
}

VISIBLE SCM
scm_pysequence_to_list (SCM obj)
{
  const char *who = "scm_pysequence_to_list";

  PyObject *py_obj = scm_to_PyObject_ptr (obj);
  if (!PySequence_Check (py_obj))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("expected a Python sequence")),
        rnrs_make_irritants_condition (scm_list_1 (obj))));
  PyObject *tup = PySequence_Tuple (py_obj);
  if (tup == NULL)
    scm_c_py_failure (who, scm_list_1 (obj));
  ssize_t length = PyTuple_GET_SIZE (tup);
  SCM p = SCM_EOL;
  for (ssize_t i = length - 1; 0 <= i; i--)
    {
      SCM element = scm_from_borrowed_PyObject_ptr (PyTuple_GET_ITEM (tup, i));
      p = scm_cons (element, p);
    }
  Py_DECREF (tup);
  return p;
}

VISIBLE SCM
scm_py_builtins (void)
{
  return scm_from_borrowed_PyObject_ptr (PyEval_GetBuiltins ());
}

VISIBLE SCM
scm_py_locals (void)
{
  PyObject *obj = PyEval_GetLocals ();
  return (obj == NULL) ? scm_py_none () : scm_from_borrowed_PyObject_ptr (obj);
}

VISIBLE SCM
scm_py_globals (void)
{
  PyObject *obj = PyEval_GetGlobals ();
  return (obj == NULL) ? scm_py_none () : scm_from_borrowed_PyObject_ptr (obj);
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmill_python (void)
{
  if (!Py_IsInitialized ())
    Py_Initialize ();

  scm_c_define_gsubr ("py-failure", 2, 0, 0, scm_py_failure);

  scm_c_define_gsubr ("grab-pyref", 1, 0, 0, scm_grab_pyref);
  scm_c_define_gsubr ("grab-borrowed-pyref", 1, 0, 0, scm_grab_borrowed_pyref);

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
  scm_c_define_gsubr ("pympq?", 1, 0, 0, scm_pympq_p);
  scm_c_define_gsubr ("pyfloat?", 1, 0, 0, scm_pyfloat_p);
  scm_c_define_gsubr ("pycomplex?", 1, 0, 0, scm_pycomplex_p);
  scm_c_define_gsubr ("pyunicode?", 1, 0, 0, scm_pyunicode_p);
  scm_c_define_gsubr ("pybytes?", 1, 0, 0, scm_pybytes_p);
  scm_c_define_gsubr ("pystring?", 1, 0, 0, scm_pystring_p);
  scm_c_define_gsubr ("pytuple?", 1, 0, 0, scm_pytuple_p);
  scm_c_define_gsubr ("pylist?", 1, 0, 0, scm_pylist_p);
  scm_c_define_gsubr ("pydict?", 1, 0, 0, scm_pydict_p);
  scm_c_define_gsubr ("pycallable?", 1, 0, 0, scm_pycallable_p);
  scm_c_define_gsubr ("pymodule?", 1, 0, 0, scm_pymodule_p);
  scm_c_define_gsubr ("pysequence?", 1, 0, 0, scm_pysequence_p);
  scm_c_define_gsubr ("pyiterable?", 1, 0, 0, scm_pyiterable_p);
  scm_c_define_gsubr ("pygenerator?", 1, 0, 0, scm_pygenerator_p);

  scm_c_define_gsubr ("boolean->pybool", 1, 0, 0, scm_boolean_to_pybool);
  scm_c_define_gsubr ("pybool->boolean", 1, 0, 0, scm_pybool_to_boolean);

  scm_c_define_gsubr ("integer->pyint", 1, 0, 0, scm_integer_to_pyint);
  scm_c_define_gsubr ("pyint->integer", 1, 0, 0, scm_pyint_to_integer);

  scm_c_define_gsubr ("integer->pympz", 1, 0, 0, scm_integer_to_pympz);
  scm_c_define_gsubr ("pympz->integer", 1, 0, 0, scm_pympz_to_integer);

  scm_c_define_gsubr ("rational->pympq", 1, 0, 0, scm_rational_to_pympq);
  scm_c_define_gsubr ("pympq->rational", 1, 0, 0, scm_pympq_to_rational);

  scm_c_define_gsubr ("inexact->pyfloat", 1, 0, 0, scm_inexact_to_pyfloat);
  scm_c_define_gsubr ("pyfloat->inexact", 1, 0, 0, scm_pyfloat_to_inexact);

  scm_c_define_gsubr ("complex->pycomplex", 1, 0, 0, scm_complex_to_pycomplex);
  scm_c_define_gsubr ("pycomplex->complex", 1, 0, 0, scm_pycomplex_to_complex);

  scm_c_define_gsubr ("number->pyobject", 1, 0, 0, scm_number_to_pyobject);
  scm_c_define_gsubr ("pyobject->number", 1, 0, 0, scm_pyobject_to_number);

  scm_c_define_gsubr ("pointer->pylong", 1, 0, 0, scm_pointer_to_pylong);
  scm_c_define_gsubr ("pylong->pointer", 1, 0, 0, scm_pylong_to_pointer);

  // As a convenience, string->pystring accepts pystrings and returns
  // them unmodified.
  scm_c_define_gsubr ("string->pystring", 1, 0, 0, scm_string_to_pystring);

  // As a convenience, pystring->string accepts strings and returns
  // them unmodified.
  scm_c_define_gsubr ("pystring->string", 1, 0, 0, scm_pystring_to_string);

  scm_c_define_gsubr ("list->pytuple", 1, 0, 0, scm_list_to_pytuple);
  scm_c_define_gsubr ("list->pylist", 1, 0, 0, scm_list_to_pylist);

  scm_c_define_gsubr ("pytuple->list", 1, 0, 0, scm_pytuple_to_list);
  scm_c_define_gsubr ("pylist->list", 1, 0, 0, scm_pylist_to_list);
  scm_c_define_gsubr ("pysequence->list", 1, 0, 0, scm_pysequence_to_list);

  scm_c_define_gsubr ("py-builtins", 0, 0, 0, scm_py_builtins);
  scm_c_define_gsubr ("py-locals", 0, 0, 0, scm_py_locals);
  scm_c_define_gsubr ("py-globals", 0, 0, 0, scm_py_globals);
}

//-------------------------------------------------------------------------
