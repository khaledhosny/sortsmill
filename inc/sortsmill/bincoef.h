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

#ifndef _SORTSMILL_BINCOEF_H
#define _SORTSMILL_BINCOEF_H

#include <stdint.h>
#include <gmp.h>

/* The binary coefficient C(n,k). */
uintmax_t bincoef (uintmax_t n, uintmax_t k);
void mpz_bincoef_ui (mpz_t C, uintmax_t n, uintmax_t k);

#endif /* _SORTSMILL_BINCOEF_H */
