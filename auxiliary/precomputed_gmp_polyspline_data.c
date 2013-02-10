#include <config.h>

// Copyright (C) 2012, 2013 by Barry Schwartz
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

#include <precomputed_polyspline_data.h>
#include <atomic_ops.h>
#include <sortsmill/xgc.h>      /* Includes pthread.h and gc.h in the
                                   correct order. */
#include <xalloc.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>

static volatile AO_t _degree_max_is_initialized = false;
static pthread_mutex_t _degree_max_mutex = PTHREAD_MUTEX_INITIALIZER;
static unsigned int _degree_max = 0;

static void
initialize__degree_max (void)
{
  // "Double-checked locking": This code first uses libatomic_ops to
  // read the 'is_initialized' field, thus avoiding the need for a
  // pthread lock if the field is already set.
  //
  // See http://www.hpl.hp.com/research/linux/atomic_ops/example.php4
  //
  // (The double-checking may be beyond necessity in this case,
  // because this initialization routine is called from within a
  // thread lock, anyway. But it does no harm.)
  //
  if (!AO_load_acquire_read (&_degree_max_is_initialized))
    {
      pthread_mutex_lock (&_degree_max_mutex);
      if (!_degree_max_is_initialized)
        {
          _degree_max = polyspline_precomputed_degree_max ();
          AO_store_release_write (&_degree_max_is_initialized, true);
        }
      pthread_mutex_unlock (&_degree_max_mutex);
    }
}

// The precomputed data is `permanent'. The memory never is recovered,
// until the program ends.

#define _FF_GMP_1D_DATA_FROM_DOUBLE_DATA(TYPE, NAME)			\
  static volatile AO_t _##TYPE##_##NAME##_is_initialized = false;	\
  static pthread_mutex_t _##TYPE##_##NAME##_mutex =			\
    PTHREAD_MUTEX_INITIALIZER;						\
  static __##TYPE##_struct **_##TYPE##_##NAME##_data = NULL;		\
									\
  VISIBLE const __##TYPE##_struct *					\
  TYPE##_##NAME (unsigned int degree)					\
  {									\
    if (!AO_load_acquire_read (&_##TYPE##_##NAME##_is_initialized))	\
      {									\
	pthread_mutex_lock (&_##TYPE##_##NAME##_mutex);			\
	if (!_##TYPE##_##NAME##_is_initialized)				\
	  {								\
	    initialize__degree_max ();					\
	    _##TYPE##_##NAME##_data =					\
	      (__##TYPE##_struct **)					\
	      xmalloc ((_degree_max + 1) *				\
		       sizeof (__##TYPE##_struct *));			\
	    for (unsigned int deg = 0; deg <= _degree_max; deg++)	\
	      {								\
		const double *fldata = fl_##NAME (deg);			\
		assert (fldata != NULL);				\
		_##TYPE##_##NAME##_data[deg] =				\
		  (__##TYPE##_struct *)					\
		  xmalloc ((deg + 1) * sizeof (TYPE##_t));		\
		for (unsigned int i = 0; i <= deg; i++)			\
		  {							\
		    TYPE##_init (&_##TYPE##_##NAME##_data[deg][i]);	\
		    TYPE##_set_d (&_##TYPE##_##NAME##_data[deg][i],	\
				  fldata[i]);				\
		  }							\
	      }								\
	    AO_store_release_write (&_##TYPE##_##NAME##_is_initialized,	\
				    true);				\
	  }								\
	pthread_mutex_unlock (&_##TYPE##_##NAME##_mutex);		\
      }									\
									\
    return (const __##TYPE##_struct *)					\
      ((degree <= _degree_max) ?					\
       _##TYPE##_##NAME##_data[degree] : NULL);				\
  }

#define _FF_GMP_2D_DATA_FROM_DOUBLE_DATA(TYPE, NAME)			\
  static volatile AO_t _##TYPE##_##NAME##_is_initialized = false;	\
  static pthread_mutex_t _##TYPE##_##NAME##_mutex =			\
    PTHREAD_MUTEX_INITIALIZER;						\
  static __##TYPE##_struct **_##TYPE##_##NAME##_data = NULL;		\
									\
  VISIBLE const __##TYPE##_struct *					\
  TYPE##_##NAME (unsigned int degree)					\
  {									\
    if (!AO_load_acquire_read (&_##TYPE##_##NAME##_is_initialized))	\
      {									\
	pthread_mutex_lock (&_##TYPE##_##NAME##_mutex);			\
	if (!_##TYPE##_##NAME##_is_initialized)				\
	  {								\
	    initialize__degree_max ();					\
	    _##TYPE##_##NAME##_data =					\
	      (__##TYPE##_struct **)					\
	      xmalloc ((_degree_max + 1) *				\
		       sizeof (__##TYPE##_struct *));			\
	    for (unsigned int deg = 0; deg <= _degree_max; deg++)	\
	      {								\
		const unsigned int size = (deg + 1) * (deg + 1);	\
		const double *fldata = fl_##NAME (deg);			\
		assert (fldata != NULL);				\
		_##TYPE##_##NAME##_data[deg] =				\
		  (__##TYPE##_struct *)					\
		  xmalloc (size * sizeof (TYPE##_t));			\
		for (unsigned int i = 0; i < size; i++)			\
		  {							\
		    TYPE##_init (&_##TYPE##_##NAME##_data[deg][i]);	\
		    TYPE##_set_d (&_##TYPE##_##NAME##_data[deg][i],	\
				  fldata[i]);				\
		  }							\
	      }								\
	    AO_store_release_write (&_##TYPE##_##NAME##_is_initialized,	\
				    true);				\
	  }								\
	pthread_mutex_unlock (&_##TYPE##_##NAME##_mutex);		\
      }									\
									\
    return (const __##TYPE##_struct *)					\
      ((degree <= _degree_max) ?					\
       _##TYPE##_##NAME##_data[degree] : NULL);				\
  }

_FF_GMP_1D_DATA_FROM_DOUBLE_DATA (mpz, precomputed_binomial_coefficients);
_FF_GMP_1D_DATA_FROM_DOUBLE_DATA (mpz,
                                  precomputed_binomial_coefficients_altsigns);

_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpz, precomputed_sbern_basis_in_mono);
_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpz, precomputed_mono_basis_in_sbern);
_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpz, precomputed_sbern_basis_in_spower);
_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpz, precomputed_spower_basis_in_sbern);

_FF_GMP_1D_DATA_FROM_DOUBLE_DATA (mpq, precomputed_binomial_coefficients);
_FF_GMP_1D_DATA_FROM_DOUBLE_DATA (mpq,
                                  precomputed_binomial_coefficients_altsigns);

_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpq, precomputed_sbern_basis_in_mono);
_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpq, precomputed_mono_basis_in_sbern);
_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpq, precomputed_sbern_basis_in_spower);
_FF_GMP_2D_DATA_FROM_DOUBLE_DATA (mpq, precomputed_spower_basis_in_sbern);
