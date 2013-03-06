#include <config.h>             // -*- coding: utf-8 -*-

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

#include <sortsmill/bincoef.h>

// Binomial coefficients.
//
// See http://en.wikipedia.org/wiki/Binomial_coefficient

static uint8_t precomputed_bincoef[11][11] = {
  { 1 },
  { 1, 1 },
  { 1, 2, 1 },
  { 1, 3, 3, 1 },
  { 1, 4, 6, 4, 1 },
  { 1, 5, 10, 10, 5, 1 },
  { 1, 6, 15, 20, 15, 6, 1 },
  { 1, 7, 21, 35, 35, 21, 7, 1 },
  { 1, 8, 28, 56, 70, 56, 28, 8, 1 },
  { 1, 9, 36, 84, 126, 126, 84, 36, 9, 1 },
  { 1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1 }
};

VISIBLE uintmax_t
bincoef (uintmax_t n, uintmax_t k)
{
  uintmax_t C;

  if (n < k)
    C = 0;
  else if (n <= 10)
    C = precomputed_bincoef[n][k];
  else
    {
      // Use C(n,k) ≡ C(n, n − k) ≡ C(n, min(k, n − k)) to improve
      // efficiency.
      if (n - k < k)
        k = n - k;

      C = 1;
      for (uintmax_t i = 1; i <= k; i++)
        C = ((n - (k - i)) * C) / i;
    }
  return C;
}

VISIBLE void
mpz_bincoef_ui (mpz_t C, uintmax_t n, uintmax_t k)
{
  mpz_t temp;
  mpz_init (temp);

  if (n <= 34)
    // C(34,17) = 2333606220, which can be represented as a 32-bit
    // unsigned integer. Therefore we are safe using machine integers.
    mpz_set_ui (C, bincoef (n, k));
  else if (n < k)
    mpz_set_ui (C, 0);
  else
    {
      // Use C(n,k) ≡ C(n, n − k) ≡ C(n, min(k, n − k)) to improve
      // efficiency.
      if (n - k < k)
        k = n - k;

      mpz_set_ui (C, 1);
      for (uintmax_t i = 1; i <= k; i++)
        {
          mpz_set_ui (temp, n - (k - i));
          mpz_mul (C, C, temp);
          mpz_set_ui (temp, i);
          mpz_tdiv_q (C, C, temp);
        }
    }

  mpz_clear (temp);
}

VISIBLE void
mpq_bincoef_ui (mpq_t C, uintmax_t n, uintmax_t k)
{
  mpz_bincoef_ui (mpq_numref (C), n, k);
  mpz_set_ui (mpq_denref (C), 1);
}
