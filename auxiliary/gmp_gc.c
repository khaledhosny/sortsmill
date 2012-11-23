#include <config.h>

/*
 * Copyright (C) 2012 Barry Schwartz
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

#include <sortsmillff/gmp_gc.h>
#include <sortsmillff/xgc.h>
#include <stddef.h>
#include <stdarg.h>

static void
finalize_mpz_t (void *obj, void *UNUSED (client_data))
{
  mpz_clear ((__mpz_struct *) obj);
}

static void
finalize_mpq_t (void *obj, void *UNUSED (client_data))
{
  mpq_clear ((__mpq_struct *) obj);
}

static void
register_mpz_t_finalizer (mpz_t x)
{
  GC_finalization_proc ofn;
  void *ocd;
  GC_REGISTER_FINALIZER (x, finalize_mpz_t, NULL, &ofn, &ocd);
}

static void
register_mpq_t_finalizer (mpq_t x)
{
  GC_finalization_proc ofn;
  void *ocd;
  GC_REGISTER_FINALIZER (x, finalize_mpq_t, NULL, &ofn, &ocd);
}

VISIBLE void
mpz_gc_init (mpz_t x)
{
  mpz_init (x);
  register_mpz_t_finalizer (x);
}

VISIBLE void
mpq_gc_init (mpq_t x)
{
  mpq_init (x);
  register_mpq_t_finalizer (x);
}

VISIBLE void
mpz_gc_inits (mpz_t x, ...)
{
  mpz_gc_init (x);

  va_list ap;
  va_start (ap, x);
  __mpz_struct *p = va_arg (ap, __mpz_struct *);
  while (p != NULL)
    {
      mpz_gc_init (p);
      p = va_arg (ap, __mpz_struct *);
    }
  va_end (ap);
}

VISIBLE void
mpq_gc_inits (mpq_t x, ...)
{
  mpq_gc_init (x);

  va_list ap;
  va_start (ap, x);
  __mpq_struct *p = va_arg (ap, __mpq_struct *);
  while (p != NULL)
    {
      mpq_gc_init (p);
      p = va_arg (ap, __mpq_struct *);
    }
  va_end (ap);
}
