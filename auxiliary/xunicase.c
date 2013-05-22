#include <config.h>

// Copyright (C) 2012 by Khaled Hosny and Barry Schwartz
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

#include <assert.h>
#include <errno.h>
#include <xalloc.h>
#include <basics.h>
#include <xunistring.h>

VISIBLE int
u8_casecompare (const uint8_t *s1, const uint8_t *s2)
{
  int result;
  int error =
    u8_casecmp (s1, u8_strlen (s1), s2, u8_strlen (s2), uc_locale_language (),
                UNINORM_NFD, &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}

VISIBLE int
u16_casecompare (const uint16_t *s1, const uint16_t *s2)
{
  int result;
  int error = u16_casecmp (s1, u16_strlen (s1), s2, u16_strlen (s2),
                           uc_locale_language (), UNINORM_NFD, &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}

VISIBLE int
u32_casecompare (const uint32_t *s1, const uint32_t *s2)
{
  int result;
  int error = u32_casecmp (s1, u32_strlen (s1), s2, u32_strlen (s2),
                           uc_locale_language (), UNINORM_NFD, &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}

VISIBLE int
u8_ncasecompare (const uint8_t *s1, const uint8_t *s2, size_t n)
{
  int result;
  int error =
    u8_casecmp (s1, szmin (n, u8_strlen (s1)), s2, szmin (n, u8_strlen (s2)),
                uc_locale_language (),
                UNINORM_NFD, &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}

VISIBLE int
u16_ncasecompare (const uint16_t *s1, const uint16_t *s2, size_t n)
{
  int result;
  int error = u16_casecmp (s1, szmin (n, u16_strlen (s1)), s2,
                           szmin (n, u16_strlen (s2)),
                           uc_locale_language (), UNINORM_NFD, &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}

VISIBLE int
u32_ncasecompare (const uint32_t *s1, const uint32_t *s2, size_t n)
{
  int result;
  int error = u32_casecmp (s1, szmin (n, u32_strlen (s1)), s2,
                           szmin (n, u32_strlen (s2)),
                           uc_locale_language (), UNINORM_NFD, &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}
