#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <sortsmill/guile/gmp.h>

VISIBLE void
scm_to_mpq (SCM val, mpq_t rop)
{
  scm_to_mpz (scm_numerator (val), mpq_numref (rop));
  scm_to_mpz (scm_denominator (val), mpq_denref (rop));
  mpq_canonicalize (rop);
}

VISIBLE SCM
scm_from_mpq (mpq_t val)
{
  return scm_divide (scm_from_mpz (mpq_numref (val)),
                     scm_from_mpz (mpq_denref (val)));
}
