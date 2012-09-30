#include <config.h>

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
#include <utype.h>
#include <chardata.h>
#include <iconv.h>
#include <xuniconv.h>

static iconv_t to_unicode = (iconv_t) (-1);
static iconv_t from_unicode = (iconv_t) (-1);
static iconv_t to_utf8 = (iconv_t) (-1);
static iconv_t from_utf8 = (iconv_t) (-1);

static const char *unicode_name = MY_ICONV_STRING_UNICHAR_T;
static char *iconv_local_encoding_name = NULL;

static void
my_iconv_setup (void)
{
  if (iconv_local_encoding_name == NULL)
    iconv_local_encoding_name = xstrdup (locale_charset ());

  to_utf8 = iconv_open ("UTF-8", iconv_local_encoding_name);
  from_utf8 = iconv_open (iconv_local_encoding_name, "UTF-8");

  to_unicode = iconv_open (unicode_name, iconv_local_encoding_name);
  from_unicode = iconv_open (iconv_local_encoding_name, unicode_name);
}

unichar_t *
def2u_strncpy (unichar_t *uto, const char *from, int n)
{
  my_iconv_setup ();

  size_t in_left = n, out_left = sizeof (unichar_t) * n;
  char *cto = (char *) uto;
  iconv (to_unicode, (ICONV_CONST char **) &from, &in_left, &cto, &out_left);
  if (cto < ((char *) uto) + 2 * n)
    *cto++ = '\0';
  if (cto < ((char *) uto) + 2 * n)
    *cto++ = '\0';
  if (cto < ((char *) uto) + 4 * n)
    *cto++ = '\0';
  if (cto < ((char *) uto) + 4 * n)
    *cto++ = '\0';
  return (uto);
}

char *
u2def_strncpy (char *to, const unichar_t *ufrom, int n)
{
  my_iconv_setup ();

  size_t in_left = sizeof (unichar_t) * n, out_left = n;
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

unichar_t *
def2u_copy (const char *from)
{
  return (unichar_t *) x_u32_strconv_from_locale (from);
}

char *
u2def_copy (const unichar_t *ufrom)
{
  return x_u32_strconv_to_locale ((const uint32_t *) ufrom);
}

char *
def2utf8_copy (const char *from)
{
  return (char *) x_u8_strconv_from_locale (from);
}

char *
utf82def_copy (const char *ufrom)
{
  return x_u8_strconv_to_locale ((const uint8_t *) ufrom);
}
