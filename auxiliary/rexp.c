#include <config.h>

// Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

#include <atomic_ops.h>
#include <sortsmill/xgc.h>                // Includes gc.h and pthreads.h in the right order.

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <intl.h>
#include <sortsmill/xunistring.h>
#include <sortsmill/rexp.h>

static void
finalize_rexp_t (void *obj, void *UNUSED (client_data))
{
  rexp_t re = (rexp_t) obj;
  if (re->pcre_ptr != NULL)
    pcre_free (re->pcre_ptr);
  if (re->extra != NULL)
    pcre_free_study (re->extra);
}

static void
register_rexp_t_finalizer (rexp_t re)
{
  GC_finalization_proc ofn;
  void *ocd;
  GC_REGISTER_FINALIZER (re, finalize_rexp_t, NULL, &ofn, &ocd);
}

static void
deregister_rexp_t_finalizer (rexp_t re)
{
  GC_finalization_proc ofn;
  void *ocd;
  GC_REGISTER_FINALIZER (re, NULL, NULL, &ofn, &ocd);
}

VISIBLE rexp_t
rexp_compile_opt (const char *pattern, int options)
{
  // x_gc_malloc clears memory, leaving (re->extra == NULL), as we
  // want.
  rexp_t re = x_gc_malloc (sizeof (rexp_buffer_t));

  int error_code;
  const char *error;
  int error_offset;
  int capture_count;
  re->pcre_ptr =
    pcre_compile2 (pattern, options, &error_code, &error, &error_offset,
                   NULL);
  if (re->pcre_ptr != NULL)
    {
      (void) pcre_fullinfo (re->pcre_ptr, re->extra, PCRE_INFO_CAPTURECOUNT,
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

VISIBLE rexp_t
rexp_compile (const char *pattern)
{
  return rexp_compile_opt (pattern, 0);
}

VISIBLE rexp_t
rexp_compile_study (const char *pattern)
{
  return rexp_study (rexp_compile (pattern));
}

VISIBLE rexp_t
rexp_compile_jit (const char *pattern)
{
  return rexp_jit (rexp_compile (pattern));
}

VISIBLE rexp_t
rexp_compile_once_opt (rexp_buffer_t *re_buf_ptr, const char *pattern,
                       int options)
{
  // "Double-checked locking": This code first uses libatomic_ops
  // to read the 'is_initialized' field, thus avoiding the need for a
  // pthread lock if the field is already set.
  //
  // See http://www.hpl.hp.com/research/linux/atomic_ops/example.php4
  //
  if (!AO_load_acquire_read (&re_buf_ptr->is_initialized))
    {
      pthread_mutex_lock (&re_buf_ptr->mutex);
      if (!re_buf_ptr->is_initialized)
        {
          rexp_t new_re = rexp_compile_opt (pattern, options);
          if (new_re != NULL)
            {
              deregister_rexp_t_finalizer (new_re);
              re_buf_ptr->pcre_ptr = new_re->pcre_ptr;
              re_buf_ptr->extra = new_re->extra;
              re_buf_ptr->capture_count = new_re->capture_count;
            }
          AO_store_release_write (&re_buf_ptr->is_initialized, true);
        }
      pthread_mutex_unlock (&re_buf_ptr->mutex);
    }

  rexp_t re;
  if (re_buf_ptr->pcre_ptr != NULL)
    re = re_buf_ptr;
  else
    re = NULL;
  return re;
}

VISIBLE rexp_t
rexp_compile_once (rexp_buffer_t *re_buf_ptr, const char *pattern)
{
  return rexp_compile_once_opt (re_buf_ptr, pattern, 0);
}

VISIBLE rexp_t
rexp_compile_once_study (rexp_buffer_t *re_buf_ptr, const char *pattern)
{
  return rexp_study (rexp_compile_once (re_buf_ptr, pattern));
}

VISIBLE rexp_t
rexp_compile_once_jit (rexp_buffer_t *re_buf_ptr, const char *pattern)
{
  return rexp_jit (rexp_compile_once (re_buf_ptr, pattern));
}

VISIBLE rexp_t
u8_rexp_compile_opt (const uint8_t *pattern, int options)
{
  return rexp_compile_opt ((const char *) pattern, options);
}

VISIBLE rexp_t
u8_rexp_compile (const uint8_t *pattern)
{
  return u8_rexp_compile_opt (pattern, (PCRE_UTF8 | PCRE_UCP));
}

VISIBLE rexp_t
u8_rexp_compile_study (const uint8_t *pattern)
{
  return rexp_study (u8_rexp_compile (pattern));
}

VISIBLE rexp_t
u8_rexp_compile_jit (const uint8_t *pattern)
{
  return rexp_jit (u8_rexp_compile (pattern));
}

VISIBLE rexp_t
u8_rexp_compile_once_opt (rexp_buffer_t *re_buf_ptr, const uint8_t *pattern,
                          int options)
{
  return rexp_compile_once_opt (re_buf_ptr, (const char *) pattern, options);
}

VISIBLE rexp_t
u8_rexp_compile_once (rexp_buffer_t *re_buf_ptr, const uint8_t *pattern)
{
  return u8_rexp_compile_once_opt (re_buf_ptr, pattern,
                                   (PCRE_UTF8 | PCRE_UCP));
}

VISIBLE rexp_t
u8_rexp_compile_once_study (rexp_buffer_t *re_buf_ptr, const uint8_t *pattern)
{
  return rexp_study (u8_rexp_compile_once (re_buf_ptr, pattern));
}

VISIBLE rexp_t
u8_rexp_compile_once_jit (rexp_buffer_t *re_buf_ptr, const uint8_t *pattern)
{
  return rexp_jit (u8_rexp_compile_once (re_buf_ptr, pattern));
}

VISIBLE rexp_t
rexp_study_opt (rexp_t re, int options)
{
  rexp_t new_re = re;

  if (re != NULL)
    {
      if (re->extra != NULL)
        pcre_free_study (re->extra);

      const char *error;
      re->extra = pcre_study (re->pcre_ptr, options, &error);
      if (re->extra == NULL || error != NULL)
        {
          // FIXME: Do something better than the following.

#ifndef NDEBUG
          fprintf (stderr, _("%s\n"), error);
#endif

          new_re = NULL;
        }
    }

  return new_re;
}

VISIBLE rexp_t
rexp_study (rexp_t re)
{
  return rexp_study_opt (re, 0);
}

VISIBLE rexp_t
rexp_jit (rexp_t re)
{
  return rexp_study_opt (re, PCRE_STUDY_JIT_COMPILE);
}

VISIBLE rexp_t
rexp_identity (rexp_t re)
{
  return re;
}

VISIBLE rexp_match_t
rexp_search_opt (rexp_t re, const char *s, int options)
{
  assert (s != NULL);

  rexp_match_t m = NULL;

  if (re != NULL)
    {
      m = x_gc_malloc (sizeof (rexp_match_buffer_t));
      int ovecsize = 3 * (re->capture_count + 1);
      m->ovector = x_gc_malloc_atomic (ovecsize * sizeof (int));
      m->capture_count = re->capture_count;
      int exec_return = pcre_exec (re->pcre_ptr, re->extra, s, strlen (s), 0,
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

VISIBLE rexp_match_t
rexp_match (rexp_t re, const char *s)
{
  return rexp_search_opt (re, s, PCRE_ANCHORED);
}

VISIBLE rexp_match_t
rexp_search (rexp_t re, const char *s)
{
  return rexp_search_opt (re, s, 0);
}

VISIBLE size_t
rexp_num_subexpr (rexp_match_t m)
{
  assert (m != NULL);

  // Include the 0th subexpression, which equals the entire match.
  return (m->capture_count + 1);
}

VISIBLE rexp_interval_t
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

VISIBLE char *
rexp_substr (rexp_match_t m, const char *s, size_t subexpression)
{
  rexp_interval_t interv = rexp_interval (m, subexpression);
  char *subexpr = NULL;
  if (0 <= interv.i_start && interv.i_start <= interv.i_end)
    subexpr =
      x_gc_strndup (s + interv.i_start, interv.i_end - interv.i_start);
  return subexpr;
}

VISIBLE rexp_match_t
u8_rexp_search_opt (rexp_t re, const uint8_t *s, int options)
{
  return rexp_search_opt (re, (const char *) s, options);
}

VISIBLE rexp_match_t
u8_rexp_match (rexp_t re, const uint8_t *s)
{
  return u8_rexp_search_opt (re, s, PCRE_ANCHORED);
}

VISIBLE rexp_match_t
u8_rexp_search (rexp_t re, const uint8_t *s)
{
  return u8_rexp_search_opt (re, s, 0);
}

VISIBLE uint8_t *
u8_rexp_substr (rexp_match_t m, const uint8_t *s, size_t subexpression)
{
  return (uint8_t *) rexp_substr (m, (const char *) s, subexpression);
}
