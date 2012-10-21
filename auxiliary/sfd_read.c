#include <config.h>

/* Copyright (C) 2012 by Barry Schwartz

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

#include <sfd_read.h>
#include <rexp.h>
#include <xgc.h>

SCM
get_sfd_line (SCM port, SCM continuation_allowed)
{
  SCM line = SCM_CAR (scm_read_line (port));
  //  if (scm_is_true (continuation_allowed)
  //      && scm_is_true (scm_string_suffix_p (
}








// Break a keyword pair into its two main parts. For example:
//
//    TeXData: 1 10485760 0 269484 134742 89828 526385 1048576 89828
//
// yields
//
//    *keyword = "TeXData:"
//    *parameters = "1 10485760 0 269484 134742 89828 526385 1048576 89828"
//
static void
split_keyword_pair (const char *line, char **keyword, char **parameters)
{
  static rexp_buffer_t re_buf = REXP_BUFFER_T_INITIALIZER;

  *keyword = NULL;
  *parameters = NULL;
  rexp_t re =
    rexp_compile_once_jit
    (&re_buf, "^[[:space:]]*([[:alpha:]][[:alnum:]]*:?)[[:space:]]*(.*)[[:space:]]*$");
  rexp_match_t m = rexp_search (re, line);
  if (m)
    {
      *keyword = rexp_substr (m, line, 1);
      *parameters = rexp_substr (m, line, 2);
    }
}

char *
get_sfd_line_from_file (void *file)
{
  FILE *f = (FILE *) file;
  size_t n;
  char *line = NULL;
  ssize_t num_read = getline(&line, &n, f);
  return (0 < num_read) ? x_gc_grabstr (line) : NULL;
}

void
sfd_to_scheme (char *(get_next_line) (void *), void *source_of_lines)
{
  char *line = get_next_line (source_of_lines);
  while (line != NULL)
    {
      char *keyword;
      char *parameters;
      split_keyword_pair (line, &keyword, &parameters);
      printf ("%s: %s\n", keyword, parameters);
      line = get_next_line (source_of_lines);
    }  
}

