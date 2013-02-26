/*
 * Copyright (C) 2013 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_GUILE_MATH_GMP_H
#define _SORTSMILL_GUILE_MATH_GMP_H

#include <libguile.h>
#include <gmp.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void scm_dynwind_mpz_unwind_handler (void *);
void scm_dynwind_mpq_unwind_handler (void *);

void scm_dynwind_mpz_clear (mpz_t);
void scm_dynwind_mpq_clear (mpq_t);

void scm_to_mpq (SCM val, mpq_t rop);
SCM scm_from_mpq (mpq_t val);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_GMP_H */
