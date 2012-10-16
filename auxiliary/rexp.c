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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <xgc.h>
#include <xunistring.h>
#include <pcre.h>
#include <rexp.h>
#include <intl.h>

static void
finalize_rexp_t (void *obj, void *UNUSED (client_data))
{
  pcre_free (((rexp_t) obj)->pcre_ptr);
}

static void
register_rexp_t_finalizer (rexp_t re)
{
  GC_finalization_proc ofn;
  void *ocd;
  GC_REGISTER_FINALIZER (re, finalize_rexp_t, NULL, &ofn, &ocd);
}

rexp_t
rexp_compile_opt (const char *pattern, int options)
{
  rexp_t re = x_gc_malloc (sizeof (struct rexp));
  int error_code;
  const char *error;
  int error_offset;
  int capture_count;
  re->pcre_ptr =
    pcre_compile2 (pattern, 0, &error_code, &error, &error_offset, NULL);
  if (re->pcre_ptr != NULL)
    {
      (void) pcre_fullinfo (re->pcre_ptr, NULL, PCRE_INFO_CAPTURECOUNT,
                            &capture_count);
      re->capture_count = (size_t) capture_count;
      register_rexp_t_finalizer (re);
    }
  else
    {
      // FIXME: Do something better than the following.

#ifndef NDEBUG
      fprintf (stderr, _("pcre_compile2 error code: %d\n"), error_code);
      fprintf (stderr, "%s\n", error);
#endif

      re = NULL;
    }

  return re;
}

rexp_t
rexp_compile (const char *pattern)
{
  return rexp_compile_opt (pattern, 0);
}

rexp_match_t
rexp_search_opt (rexp_t re, const char *s, int options)
{
  assert (s != NULL);

  rexp_match_t m = NULL;

  if (re != NULL)
    {
      m = x_gc_malloc (sizeof (struct rexp_match));
      int ovecsize = 3 * (re->capture_count + 1);
      m->ovector = x_gc_malloc_atomic (ovecsize * sizeof (int));
      m->capture_count = re->capture_count;
      int exec_return = pcre_exec (re->pcre_ptr, NULL, s, strlen (s), 0,
                                   options, m->ovector, ovecsize);
      if (exec_return < 0)
        {
          // FIXME: Do something better than the following.

#ifndef NDEBUG
          if (exec_return != PCRE_ERROR_NOMATCH)
            fprintf (stderr, _("pcre_exec error code: %d\n"), exec_return);
#endif

          m = NULL;
        }
    }

  return m;
}

rexp_match_t
rexp_match (rexp_t re, const char *s)
{
  return rexp_search_opt (re, s, PCRE_ANCHORED);
}

rexp_match_t
rexp_search (rexp_t re, const char *s)
{
  return rexp_search_opt (re, s, 0);
}

size_t
rexp_num_subexpr (rexp_match_t m)
{
  assert (m != NULL);

  // Include the 0th subexpression, which equals the entire match.
  return (m->capture_count + 1);
}

rexp_interval_t
rexp_interval (rexp_match_t m, size_t subexpression)
{
  rexp_interval_t interv;

  if (subexpression <= m->capture_count)
    {
      interv.i_start = m->ovector[2 * subexpression];
      interv.i_end = m->ovector[2 * subexpression + 1];
    }
  else
    {
      interv.i_start = -1;
      interv.i_end = -1;
    }

  return interv;
}

char *
rexp_substr (rexp_match_t m, const char *s, size_t subexpression)
{
  rexp_interval_t interv = rexp_interval (m, subexpression);
  char *subexpr = NULL;
  if (0 <= interv.i_start && interv.i_start <= interv.i_end)
    subexpr =
      x_gc_strndup (s + interv.i_start, interv.i_end - interv.i_start);
  return subexpr;
}
