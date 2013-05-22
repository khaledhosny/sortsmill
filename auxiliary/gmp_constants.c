#include <config.h>

// Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

#include <sortsmill/math/gmp_constants.h>
#include <sortsmill/xgc.h>    /* Includes gc.h and pthreads.h in the
                                   right order. */
#include <stdbool.h>

static inline void
mpz_canonicalize (mpz_t UNUSED (_))
{
  // Do nothing.
}

#define _FF_GMP_CONSTANT_DEFN(TYPE, NAME, VALUE, RADIX)			\
									\
  VISIBLE TYPE##_t _##NAME;						\
  VISIBLE volatile AO_t _##NAME##_is_initialized = false;		\
  static pthread_mutex_t _##NAME##_mutex = PTHREAD_MUTEX_INITIALIZER;	\
  void _initialize_##NAME (void);					\
									\
  VISIBLE void								\
  _initialize_##NAME (void)						\
  {									\
    pthread_mutex_lock (&_##NAME##_mutex);				\
    if (!_##NAME##_is_initialized)					\
      {									\
	/* There is no need to run TYPE##_clear, because the */		\
	/* constant is `permanent', once initialized.        */		\
	TYPE##_init (_##NAME);						\
									\
	TYPE##_set_str ((_##NAME), (VALUE), (RADIX));			\
	TYPE##_canonicalize (_##NAME);					\
									\
	AO_store_release_write (&_##NAME##_is_initialized, true);	\
      }									\
    pthread_mutex_unlock (&_##NAME##_mutex);				\
  }

_FF_GMP_CONSTANT_DEFN (mpz, mpz_zero, "0", 10);
_FF_GMP_CONSTANT_DEFN (mpz, mpz_one, "1", 10);
_FF_GMP_CONSTANT_DEFN (mpz, mpz_neg_one, "-1", 10);
_FF_GMP_CONSTANT_DEFN (mpz, mpz_two, "2", 10);
_FF_GMP_CONSTANT_DEFN (mpz, mpz_neg_two, "-2", 10);
_FF_GMP_CONSTANT_DEFN (mpz, mpz_three, "3", 10);
_FF_GMP_CONSTANT_DEFN (mpz, mpz_neg_three, "-3", 10);

_FF_GMP_CONSTANT_DEFN (mpq, mpq_zero, "0", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_one_half, "1/2", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_neg_one_half, "-1/2", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_one, "1", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_neg_one, "-1", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_two, "2", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_neg_two, "-2", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_three, "3", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_neg_three, "-3", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_four, "4", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_neg_four, "-4", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_five, "5", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_neg_five, "-5", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_six, "6", 10);
_FF_GMP_CONSTANT_DEFN (mpq, mpq_neg_six, "-6", 10);
