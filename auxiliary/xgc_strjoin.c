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

#include <sortsmillff/xgc.h>
#include <stdarg.h>
#include <string.h>

VISIBLE char *
x_gc_strjoin (const char *s1, ...)
{
  va_list ap;

  va_start (ap, s1);
  char *p = x_gc_vstrjoin (s1, ap);
  va_end (ap);
  return p;
}

VISIBLE char *
x_gc_vstrjoin (const char *s1, va_list ap)
{
  char *p;

  if (s1 == NULL)
    p = x_gc_strdup ("");
  else
    {
      va_list aq;

      size_t length = strlen (s1);

      size_t total_length = length;
      va_copy (aq, ap);
      char *s = va_arg (aq, char *);
      while (s != NULL)
        {
          total_length += strlen (s);
          s = va_arg (aq, char *);
        }
      va_end (aq);

      p = (char *) x_gc_malloc_atomic ((total_length + 1) * sizeof (char));

      memcpy (p, s1, length * sizeof (char));
      total_length = length;
      va_copy (aq, ap);
      s = va_arg (aq, char *);
      while (s != NULL)
        {
          length = strlen (s);
          memcpy (p + total_length, s, length * sizeof (char));
          total_length += length;
          s = va_arg (aq, char *);
        }
      p[total_length] = '\0';
      va_end (aq);
    }

  return p;
}
