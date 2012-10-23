#include <config.h>

/* Copyright (C) 2012 by Barry Schwartz */
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

#include <stdlib.h>
#include <string.h>
#include <iconv.h>
#include <libguile.h>

// Export symbols that are very unlikely to conflict, taking advantage
// of Guile’s dependence on ltdl. See "Using libltdl" in the libtool
// manual to learn how this method works.
//
// We assume these very long symbol names can be handled.
//
#define init_guile_sortsmillff_iconv \
  libguile_sortsmillff_iconv_LTX_init_guile_sortsmillff_iconv
#define scm_embedded_utf7_to_string \
  libguile_sortsmillff_iconv_LTX_scm_embedded_utf7_to_string

VISIBLE void init_guile_sortsmillff_iconv (void);
VISIBLE SCM scm_embedded_utf7_to_string (SCM str);

SCM
scm_embedded_utf7_to_string (SCM str)
{
  char *utf8;

  scm_dynwind_begin (0);

  // Treat the string as UTF-7 embedded in ASCII/UTF-8.
  char *utf7 = scm_to_utf8_stringn (str, NULL);
  scm_dynwind_free (utf7);

  // I doubt the MY_ICONV_SUFFIX_TRANSLIT_STRING here has any effect,
  // but it should do no harm, either, and demonstrates how to use it.
  iconv_t descriptor =
    iconv_open (MY_ICONV_UTF8_STRING MY_ICONV_SUFFIX_TRANSLIT_STRING,
                MY_ICONV_UTF7_STRING);

  if (descriptor != (iconv_t) (-1))
    {
      size_t utf7_length = strlen (utf7);
      size_t utf8_length = utf7_length * 6; /* FIXME: What is a good
					       length here? */
      utf8 = scm_gc_malloc_pointerless ((utf8_length + 1) * sizeof (char),
                                        "string");
      memset (utf8, 0, (utf8_length + 1) * sizeof (char));

      // iconv(3) modifies these pointers and integers.
      char *utf7_p = utf7;
      char *utf8_p = utf8;
      size_t utf7_n = utf7_length;
      size_t utf8_n = utf8_length;

      (void) iconv (descriptor, &utf7_p, &utf7_n, &utf8_p, &utf8_n);

      iconv_close (descriptor);
    }
  else
    {
      utf8 = scm_gc_malloc_pointerless ((strlen (utf7) + 1) * sizeof (char),
                                        "string");
      strcpy (utf8, utf7);
    }

  SCM result = scm_from_utf8_string (utf8);

  scm_dynwind_end ();

  return result;
}

void
init_guile_sortsmillff_iconv (void)
{
  (void) scm_c_define_gsubr ("embedded-utf7->string", 1, 0, 0,
                             scm_embedded_utf7_to_string);
}
