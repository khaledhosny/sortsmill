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
#include <errno.h>
#include <xalloc.h>
#include <xunistring.h>

int
u8_compare (const uint8_t *s1, const uint8_t *s2)
{
  int result;
  int error =
    u8_normcmp (s1, u8_strlen (s1), s2, u8_strlen (s2), UNINORM_NFD, &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}

int
u16_compare (const uint16_t *s1, const uint16_t *s2)
{
  int result;
  int error =
    u16_normcmp (s1, u16_strlen (s1), s2, u16_strlen (s2), UNINORM_NFD,
                 &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}

int
u32_compare (const uint32_t *s1, const uint32_t *s2)
{
  int result;
  int error =
    u32_normcmp (s1, u32_strlen (s1), s2, u32_strlen (s2), UNINORM_NFD,
                 &result);
  if (error != 0 && errno == ENOMEM)
    xalloc_die ();
  assert (error == 0);
  return result;
}
