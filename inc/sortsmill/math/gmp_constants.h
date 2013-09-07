/*
 * Copyright (C) 2012 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_MATH_GMP_CONSTANTS_H
#define _SORTSMILL_MATH_GMP_CONSTANTS_H

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

#include <gmp.h>
#include <sortsmill/core.h>

#define _FF_GMP_CONSTANT_DECL(TYPE, NAME)				\
  extern TYPE##_t _##NAME;						\
  extern volatile stm_dcl_indicator_t _##NAME##_is_initialized;         \
									\
  static inline const __##TYPE##_struct *				\
  NAME (void)								\
  {									\
    void _initialize_##NAME (void);					\
    /* "Double-checked locking"; see                                */	\
    /* http://www.hpl.hp.com/research/linux/atomic_ops/example.php4 */	\
    if (!stm_dcl_load_indicator (&_##NAME##_is_initialized))		\
      _initialize_##NAME ();						\
    return _##NAME;							\
  }

_FF_GMP_CONSTANT_DECL (mpz, mpz_zero);
_FF_GMP_CONSTANT_DECL (mpz, mpz_one);
_FF_GMP_CONSTANT_DECL (mpz, mpz_neg_one);
_FF_GMP_CONSTANT_DECL (mpz, mpz_two);
_FF_GMP_CONSTANT_DECL (mpz, mpz_neg_two);
_FF_GMP_CONSTANT_DECL (mpz, mpz_three);
_FF_GMP_CONSTANT_DECL (mpz, mpz_neg_three);

_FF_GMP_CONSTANT_DECL (mpq, mpq_zero);
_FF_GMP_CONSTANT_DECL (mpq, mpq_one_half);
_FF_GMP_CONSTANT_DECL (mpq, mpq_neg_one_half);
_FF_GMP_CONSTANT_DECL (mpq, mpq_one);
_FF_GMP_CONSTANT_DECL (mpq, mpq_neg_one);
_FF_GMP_CONSTANT_DECL (mpq, mpq_two);
_FF_GMP_CONSTANT_DECL (mpq, mpq_neg_two);
_FF_GMP_CONSTANT_DECL (mpq, mpq_three);
_FF_GMP_CONSTANT_DECL (mpq, mpq_neg_three);
_FF_GMP_CONSTANT_DECL (mpq, mpq_four);
_FF_GMP_CONSTANT_DECL (mpq, mpq_neg_four);
_FF_GMP_CONSTANT_DECL (mpq, mpq_five);
_FF_GMP_CONSTANT_DECL (mpq, mpq_neg_five);
_FF_GMP_CONSTANT_DECL (mpq, mpq_six);
_FF_GMP_CONSTANT_DECL (mpq, mpq_neg_six);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_GMP_CONSTANTS_H */
