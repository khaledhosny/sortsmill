#include <config.h>

/*
 * Copyright (C) 2012 by Barry Schwartz
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <sortsmillff/polyspline.h>
#include <atomic_ops.h>
#include <sortsmillff/xgc.h>    /* Includes pthread.h and gc.h in the
                                   correct order. */
#include <sortsmillff/gmp_gc.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>

static volatile AO_t _degree_max_is_initialized = false;
static pthread_mutex_t _degree_max_mutex = PTHREAD_MUTEX_INITIALIZER;
static unsigned int _degree_max = 0;

static void
initialize__degree_max (void)
{
  // "Double-checked locking": This code first uses libatomic_ops
  // to read the 'is_initialized' field, thus avoiding the need for a
  // pthread lock if the field is already set.
  //
  // See http://www.hpl.hp.com/research/linux/atomic_ops/example.php4
  //
  if (!AO_load_acquire_read (&_degree_max_is_initialized))
    {
      pthread_mutex_lock (&_degree_max_mutex);
      if (!_degree_max_is_initialized)
        {
          _degree_max = polyspline_degree_max ();
          AO_store_release_write (&_degree_max_is_initialized, true);
        }
      pthread_mutex_unlock (&_degree_max_mutex);
    }
}

#define _FF_GMP_PRECOMPUTED_DATA(TYPE, NAME)				\
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
	      x_gc_malloc ((_degree_max + 1) *				\
			   sizeof (__##TYPE##_struct *));		\
	    for (unsigned int deg = 0; deg <= _degree_max; deg++)	\
	      {								\
		const double *fldata = fl_##NAME (degree);		\
		assert (fldata != NULL);				\
		_##TYPE##_##NAME##_data[deg] =				\
		  (__##TYPE##_struct *)					\
		  x_gc_malloc ((deg + 1) * sizeof (TYPE##_t));		\
		for (unsigned int i = 0; i <= deg; i++)			\
		  {							\
		    TYPE##_gc_init (_##TYPE##_##NAME##_data[i]);	\
		    TYPE##_set_d (_##TYPE##_##NAME##_data[i],		\
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


_FF_GMP_PRECOMPUTED_DATA(mpz, binomial_coefficients);
_FF_GMP_PRECOMPUTED_DATA(mpz, binomial_coefficients_altsigns);
_FF_GMP_PRECOMPUTED_DATA(mpz, sbern_basis_in_mono);
_FF_GMP_PRECOMPUTED_DATA(mpz, mono_basis_in_sbern);
_FF_GMP_PRECOMPUTED_DATA(mpz, sbern_basis_in_spower);
_FF_GMP_PRECOMPUTED_DATA(mpz, spower_basis_in_sbern);
					       
_FF_GMP_PRECOMPUTED_DATA(mpq, binomial_coefficients);
_FF_GMP_PRECOMPUTED_DATA(mpq, binomial_coefficients_altsigns);
_FF_GMP_PRECOMPUTED_DATA(mpq, sbern_basis_in_mono);
_FF_GMP_PRECOMPUTED_DATA(mpq, mono_basis_in_sbern);
_FF_GMP_PRECOMPUTED_DATA(mpq, sbern_basis_in_spower);
_FF_GMP_PRECOMPUTED_DATA(mpq, spower_basis_in_sbern);
