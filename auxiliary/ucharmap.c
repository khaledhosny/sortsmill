#include <config.h>

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

/* Copyright (C) 2000-2012 by George Williams */
/*
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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <ustring.h>
#include <iconv.h>
#include <uniconv.h>
#include <xunistring.h>
#include <null_passthru.h>

static iconv_t from_unicode = (iconv_t) (-1);

static const char *unicode_name = MY_ICONV_STRING_UNICHAR_T;
static char *iconv_local_encoding_name = NULL;

static void
my_iconv_setup (void)
{
  if (iconv_local_encoding_name == NULL)
    iconv_local_encoding_name = xstrdup (locale_charset ());

  from_unicode = iconv_open (iconv_local_encoding_name, unicode_name);
}

char *
u2def_strncpy (char *to, const uint32_t *ufrom, int n)
{
  my_iconv_setup ();

  size_t in_left = sizeof (uint32_t) * n, out_left = n;
  char *cfrom = (char *) ufrom, *cto = to;
  iconv (from_unicode, (ICONV_CONST char **) &cfrom, &in_left, &cto,
         &out_left);
  if (cto < to + n)
    *cto++ = '\0';
  if (cto < to + n)
    *cto++ = '\0';
  if (cto < to + n)
    *cto++ = '\0';
  if (cto < to + n)
    *cto++ = '\0';
  return (to);
}

char *
u2def_copy (const uint32_t *ufrom)
{
  return NULL_PASSTHRU (ufrom,
                        x_u32_strconv_to_locale ((const uint32_t *) ufrom));
}

char *
utf82def_copy (const char *ufrom)
{
  return NULL_PASSTHRU (ufrom,
                        x_u8_strconv_to_locale ((const uint8_t *) ufrom));
}
