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

#include <config.h>

#include "giofuncP.h"
#include "ustring.h"


struct transtab
{
  uint32_t *old;
  uint32_t *new;
  int olen;
  int gf_mask;
};
static struct transtab *transtab = NULL;

uint32_t *
_GIO_translateURL (uint32_t *path, enum giofuncs gf)
{
  struct transtab *test;

  if (transtab == NULL)
    return (NULL);

  for (test = transtab; test->old != NULL; ++test)
    {
      // FIXME: Should this u32_strncmp be a normalized comparison?
      if ((test->gf_mask & (1 << gf))
          && u32_strncmp (path, test->old, test->olen) == 0)
        {
          uint32_t *res =
            xmalloc ((u32_strlen (path) - test->olen +
                      u32_strlen (test->new) + 1) * sizeof (uint32_t));
          u32_strcpy (res, test->new);
          u32_strcat (res, path + test->olen);
          return (res);
        }
    }
  return (NULL);
}
